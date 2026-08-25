{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module MonadIO.Temp
  ( OutputData(..), mkTempDir, mkTempDir', mkTempDir''
  , progNamePrefix, __progNamePrefix__
  , tempdir, __tempdir__, tempfile, tempfile', tempfile''
  , testsWithTempfile, testsWithTempfiles, testsWithTempfiles'
  , testsWithTempDir, testsWithTempDir', testsWithTempDir''
  , withTempDir, withTempDir', withTempDir''
  , withTempDirCD, withTempDirCD'
  , withTempfile, withTempfile', withTempfile'', withTempfile'''

  , tests
  )
where

import Base1T

-- base --------------------------------

import qualified  System.IO

import Control.Monad          ( (>=>) )
import Data.Functor.Identity  ( Identity( Identity ), runIdentity )
import Data.List              ( dropWhileEnd )
import Data.Maybe             ( fromJust )
import Data.Tuple             ( uncurry )
import System.Environment     ( getProgName )
import System.IO              ( FilePath, Handle
                              , SeekMode( AbsoluteSeek )
                              , char8, hSeek, hSetEncoding, hSetNewlineMode
                              , nativeNewlineMode, noNewlineTranslation, utf8
                              )

-- base-unicode-symbols ----------------

import Prelude.Unicode  ( (≠) )

-- bytestring --------------------------

import qualified Data.ByteString  as  BS

import Data.ByteString  ( ByteString )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadMask, bracket )

-- fpath -------------------------------

import FPath                   ( (⫻) )
import FPath.AbsDir            ( AbsDir )
import FPath.AbsFile           ( AbsFile )
import FPath.AsFilePath        ( filepath )
import FPath.Basename          ( basename )
import FPath.Dir               ( DirAs( _Dir_ ) )
import FPath.Dirname           ( dirname )
import FPath.Error.FPathError  ( AsFPathError, FPathError, FPathIOError )
import FPath.File              ( FileAs )
import FPath.Parseable         ( Parseable( parse, parseDir, __parse__ ) )
import FPath.PathComponent     ( PathComponent, pc )
import FPath.RelFile           ( relfile )

-- fstat -------------------------------

import qualified FStat

import FStat  ( FStat )

-- lens --------------------------------

import Control.Lens.At         ( ix )
import Control.Lens.Each       ( Each, each )
import Control.Lens.Fold       ( mapMOf_ )
import Control.Lens.Traversal  ( mapMOf )
import Control.Lens.Tuple      ( _1, _2, _3 )

-- monaderror-io -----------------------

import MonadError           ( ж )
import MonadError.IO        ( ӝ, asIOErrorT )
import MonadError.IO.Error  ( IOError )

-- more-unicode ------------------------

import Data.MoreUnicode.Semigroup  ( ѹ )

-- mtl ---------------------------------

import Control.Monad.State   ( MonadState, modify, runStateT )
import Control.Monad.Trans   ( lift )

-- natural -----------------------------

import Natural  ( ỻ )

-- tasty -------------------------------

import Test.Tasty  ( DependencyType( AllSucceed ), dependentTestGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, assertBool, assertEqual, assertFailure )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), ioTests, withResourceCleanup )

-- temporary ---------------------------

import System.IO.Temp  ( createTempDirectory, getCanonicalTemporaryDirectory
                       , withTempDirectory )

-- text --------------------------------

import qualified  Data.Text     as  T
import qualified  Data.Text.IO  as  TextIO

-- time --------------------------------

import Data.Time.Calendar  ( Day )
import Data.Time.Clock     ( getCurrentTime, utctDay )
import Data.Time.Format    ( defaultTimeLocale, formatTime )

-- unix --------------------------------

import System.Posix.Process  ( getProcessID )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO            ( warn )
import MonadIO.Base       ( hClose, unlink )
import MonadIO.Directory  ( chdir, inDir, glob, nuke, pwd, __pwd__)
import MonadIO.OpenFile   ( readFile, writeFile )

--------------------------------------------------------------------------------

type ℍ  = Handle
type 𝔹𝕊 = ByteString

----------------------------------------

{-| create a temporary file; return file name and/or handle; if just the handle,
    the file is deleted immediately upon creation; else, the caller is
    responsible for deletion
-}
openTempFile ∷ ∀ ε δ ξ μ .
               (MonadIO μ, DirAs δ, ReturnFNFH ξ,
                AsIOError ε, AsFPathError ε, MonadError ε μ, HasCallStack) ⇒
               δ -- ^ Dir in which tmpfile is to be created
             → PathComponent -- ^ Pattern for filename of tmpfile.  See
                             -- `System.IO.openTempFile` for details; a unique
                             -- large random integer is inserted between the
                             -- body of the filename and the extension (if any).
             → μ ξ -- ^ May be a filename & filehandle pair; or just a handle
                   --   (file will be deleted immediately upon creation) or just
                   --   a filename (handle will be closed).
openTempFile (review $ filepath ∘ _Dir_ → d) (review filepath → r) = do
  (fn,fh) ← asIOError $ System.IO.openTempFile d r
  fn' ← parse @AbsFile fn
  toFNFH fn' fh

----------------------------------------

{- | Data that may be written to a filehandle, setting encoding and newline
     mode; `Text` is written as utf8 with native line-endings, while
     `ByteString`s are written as bytes with no newline translation.  `String`s
     are written as latin1 with native line-endings.  The `Handle` is left in
     whatever encoding & newline-translation is implied by the input type.
 -}

class OutputData τ where
  output ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack) ⇒ ℍ → τ → μ ()

instance OutputData 𝕊 where
  output h t = asIOError $ do
    hSetEncoding      h utf8
    hSetNewlineMode   h nativeNewlineMode
    System.IO.hPutStr h t

instance OutputData 𝕋 where
  output h t = asIOError $ do
    hSetEncoding     h utf8
    hSetNewlineMode  h nativeNewlineMode
    TextIO.hPutStr   h t

instance OutputData 𝔹𝕊 where
  output h bs = asIOError $ do
    hSetEncoding     h char8
    hSetNewlineMode  h noNewlineTranslation
    BS.hPutStr       h bs

instance OutputData () where
  {- | No data output (obviously), no setting of encoding or newline-translation
       mode. -}
  output _ () = return ()

------------------------------------------------------------

{- | A type of things that can be returned by some tempfile functions; if
     the return is an `AbsFile` only; the `Handle` is closed; if the return
     is a `Handle` only, the `AbsFile` is deleted before the return (else it's
     up to the client to arrange cleanup).

     Unless stated otherwise, the `Handle` is opened in `ReadWrite` mode.
-}
class ReturnFNFH ω where
  toFNFH ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack) ⇒
           AbsFile → ℍ → μ ω

instance ReturnFNFH ℍ where
  toFNFH fn fh = unlink fn ⪼ return fh

instance ReturnFNFH AbsFile where
  toFNFH fn fh = hClose fh ⪼ return fn

instance ReturnFNFH (AbsFile, ℍ) where
  toFNFH fn fh = return (fn,fh)

------------------------------------------------------------

{-| A prefix (suitable for, e.g., temp files or dirs) in the form of a
    `RelFile` (which is the program name), plus a '-' character. -}
progNamePrefix ∷ ∀ ε μ .
                 (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ,
                  HasCallStack) ⇒
                 μ PathComponent
progNamePrefix = asIOError getProgName ≫ parse

{-| A version of `progNamePrefix` in which errors are thrown into IO -}
__progNamePrefix__ ∷ ∀ μ . (MonadIO μ, HasCallStack) ⇒ μ PathComponent
__progNamePrefix__ = ӝ $ progNamePrefix @FPathIOError

----------------------------------------

{- | Create a temporary file, return it as name and/or filehandle.  Any content
     provided is pre-written to the file (and access position for the filehandle
     returned, if any, reset to the beginning of the file).
  -}

tempfile'' ∷ ∀ ε δ τ ξ μ .
             (MonadIO μ, DirAs δ, OutputData τ, ReturnFNFH ξ,
              AsIOError ε, AsFPathError ε, MonadError ε μ, HasCallStack) ⇒
             δ             -- ^ Directory in which the file is made.
           → PathComponent -- ^ Pattern for the file name.  If this pattern has
                           --   an extension suffix, that is removed before a
                           --   hyphen and a guaranteed-unique integer is added
                           --   to the end of the filename; then the extension
                           --   (if any) is re-added.
                           --   See `System.IO.openTempFile`.
           → τ             -- ^ Content to write to the file before returning.
           → μ ξ           -- ^ If the return type is a Handle (only), the file
                           --   is deleted before returning. If a filename
                           --   (only) is the return type, the handle is closed
                           --   before returning.  Note that if the filename
                           --   is part of the return type, then it is the
                           --   client's responsibility to delete that when
                           --   done.  See also `withTempfile`.

tempfile'' d r t = do
  (fn,h) ← openTempFile d r
  output h t
  asIOError $ hSeek h AbsoluteSeek 0
  toFNFH fn h

--------------------

{-| get the system temporary directory (TMPDIR, etc.) -}
tempdir ∷ ∀ ε μ .
          (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ,
           HasCallStack) ⇒
          μ AbsDir
tempdir = asIOError getCanonicalTemporaryDirectory ≫ parseDir

--------------------

{-| `tempdir`, throwing any errors into IO -}
__tempdir__ ∷ (MonadIO μ, HasCallStack) ⇒ μ AbsDir
__tempdir__ = ж @FPathIOError tempdir

----------------------------------------

{- | Like `tempfile''`, but using the system temp directory. -}
tempfile' ∷ ∀ ε τ ξ μ .
            (MonadIO μ, OutputData τ, ReturnFNFH ξ,
             AsIOError ε, AsFPathError ε, MonadError ε μ, HasCallStack) ⇒
            PathComponent → τ → μ ξ
tempfile' r t = tempdir ≫ \ d → tempfile'' d r t

--------------------

{- | Like `tempfile'`, but using the progname as the file pattern. -}
tempfile ∷ ∀ ε τ ξ μ .
           (MonadIO μ, OutputData τ, ReturnFNFH ξ,
            AsIOError ε, AsFPathError ε, MonadError ε μ, HasCallStack) ⇒
           τ → μ ξ
tempfile t = progNamePrefix ≫ \ p → tempfile' (p ◇ [pc|-|]) t

----------------------------------------

{- | Take a possibly error-throwing action, and if that error occurs, push it
     onto a stack; returning () whatever happens. -}
stackE ∷ ∀ ε η . MonadState [ε] η ⇒ ExceptT ε η () → η ()
stackE go = join $ either (modify ∘ (:)) return ⊳ ѥ go

{- | Create a temporary file, and perform some IO with that.

     The handle to the file is closed, and the file deleted after use.  Any
     IOErrors raised by these actions are returned in the list that is the
     second part of the return

     either of these actions raise an IOError, then that is written to the
     `MonadState`.
 -}
withTempfile''' ∷ ∀ ε ξ δ τ ω μ .
                 (MonadIO μ, MonadMask μ, DirAs δ, OutputData τ,
                  MonadError ε μ, AsIOError ε, AsFPathError ε, HasCallStack,
                  AsIOError ξ) ⇒
                 δ                   -- ^ Write the tempfile to this dir.
               → PathComponent       -- ^ Pattern to use for the tempfile
                                     --   name.  See `tempfile` for details.
               → τ                   -- ^ Any data to write to the temp file
                                     --   before handing it to the `IO` action.
               → (AbsFile → ℍ → μ ω) -- ^ An `IO` action to perform, with the
                                     --   tempfile.
               → μ (ω, [ξ])          -- ^ The result of the io action, along
                                     --   with any `IOError`s raised during
                                     --   cleanup.

withTempfile''' d p t io = flip runStateT [] $
  bracket
    (tempfile'' d p t)
    (\ (fn,fh) → mapM_ stackE [hClose fh, unlink fn])
    (lift ∘ uncurry io)

--------------------

{- | Like `withTempfile'''`, but writes any errors seen during cleanup to
     stderr. -}
withTempfile'' ∷ ∀ ε δ τ ω μ .
                 (MonadIO μ, MonadMask μ, DirAs δ, OutputData τ,
                  MonadError ε μ, AsIOError ε, AsFPathError ε, HasCallStack) ⇒
                 δ → PathComponent → τ → (AbsFile → Handle → μ ω) → μ ω

withTempfile'' d p t io = do
  (w, es) ← withTempfile''' @_ @IOError d p t io
  forM_ es warn
  return w

--------------------

{- | Like `withTempfile''`, but uses the system temp directory. -}
withTempfile' ∷ ∀ ε τ ω μ .
                (MonadIO μ, MonadMask μ, OutputData τ,
                 MonadError ε μ, AsIOError ε, AsFPathError ε, HasCallStack) ⇒
                PathComponent → τ → (AbsFile → Handle → μ ω) → μ ω
withTempfile' r t io = tempdir ≫ \ d → withTempfile'' d r t io

--------------------

{- | Like `withTempfile'`, but uses the program name as the file pattern. -}
withTempfile ∷ ∀ ε τ ω μ .
               (MonadIO μ, MonadMask μ, OutputData τ,
                MonadError ε μ, AsIOError ε, AsFPathError ε, HasCallStack) ⇒
               τ → (AbsFile → Handle → μ ω) → μ ω
withTempfile t io = progNamePrefix ≫ \ p → withTempfile' (p ◇ [pc|-|]) t io

----------------------------------------

{- | Create a temporary directory as a subdir of a given dir; return its name.
     It is the responsibility of the caller to arrange appropriate cleanup. -}
mkTempDir'' ∷ ∀ ε δ μ .
              (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ,
               HasCallStack, DirAs δ) ⇒
              δ → PathComponent → μ AbsDir
mkTempDir'' (review $ filepath ∘ _Dir_ → t) (review filepath → r) = do
  d ← liftIO $ createTempDirectory t r
  parseDir d

--------------------

{- | `mkTempDir''`, but create a dir in the system temp dir. -}
mkTempDir' ∷ ∀ ε μ .
             (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ,
              HasCallStack) ⇒
             PathComponent → μ AbsDir
mkTempDir' r = tempdir ≫ \ d → mkTempDir'' d r

--------------------

{- | `mkTempDir'`, with the prefix being the program name plus `"-"`. -}
mkTempDir ∷ ∀ ε μ .
            (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ,
             HasCallStack) ⇒
            μ AbsDir
mkTempDir = ((◇ [pc|-|]) ⊳ progNamePrefix) ≫ mkTempDir'

----------------------------------------

_parseD ∷ (Parseable χ, AsFPathError ε, MonadError ε η) ⇒ FilePath → η χ
_parseD = parse ∘ (⊕ "/") ∘ dropWhileEnd (≡ '/')

{- | Perform some IO with a given temporary directory, created within some given
     dir; the temporary dir is removed once IO is complete.  The directory
     created is passed into the IO as an `AbsDir`.  The directory name is
     prefixed by some relative name.
 -}
-- note that withTempDirectory will give us a relative dir if passed a relative
-- dir (that exists and is usable)
withTempDir'' ∷ ∀ ε ω δ μ .
                (MonadIO μ, MonadMask μ, AsFPathError ε, AsIOError ε,
                 MonadError ε μ, HasCallStack, DirAs δ, Parseable δ) ⇒
                δ → PathComponent → (δ → ExceptT ε IO ω) → μ ω
withTempDir'' d (review filepath → r) io =
  asIOErrorT $ withTempDirectory (d ⫥ filepath ∘ _Dir_) r (_parseD >=> io)

----------

{- | Perform some IO with a temporary directory (created in the system temp
     directory, see `tempdir`), which is removed once IO is complete.  The
     directory created is passed into the IO as an `AbsDir`.  The directory name
     is prefixed by some relative name.
 -}
withTempDir' ∷ ∀ ε ω μ .
               (MonadIO μ, MonadMask μ, AsFPathError ε, AsIOError ε,
                MonadError ε μ, HasCallStack) ⇒
               PathComponent → (AbsDir → ExceptT ε IO ω) → μ ω
withTempDir' r io = tempdir ≫ \ d → withTempDir'' d r io

{- | Like `withTempDir'`, with the prefix being the program name plus `"-"`. -}
withTempDir ∷ ∀ ε ω μ .
              (MonadIO μ, MonadMask μ, AsFPathError ε, AsIOError ε,
               MonadError ε μ, HasCallStack) ⇒
              (AbsDir → ExceptT ε IO ω) → μ ω
withTempDir io = progNamePrefix ≫ \ p → withTempDir' (p ◇ [pc|-|]) io

{- | Like `withTempDir`, but temporarily changes dir into the temporary
     directory, rather than passing the dir name to the IO. -}
withTempDirCD ∷ ∀ ε ω μ .
                (MonadIO μ, MonadMask μ, AsFPathError ε, AsIOError ε,
                 MonadError ε μ, HasCallStack) ⇒
                ExceptT ε IO ω → μ ω
withTempDirCD io = withTempDir (flip inDir io)

{- | Like `withTempDir`, but temporarily changes dir into the temporary
     directory, as well as passing the dir name to the IO. -}
withTempDirCD' ∷ ∀ ε ω μ .
                 (MonadIO μ, MonadMask μ, AsFPathError ε, AsIOError ε,
                  MonadError ε μ, HasCallStack) ⇒
                 (AbsDir → ExceptT ε IO ω) → μ ω
withTempDirCD' io = withTempDir (\ d → inDir d $ io d)

----------------------------------------

{-| perform tests using a testfile, which is created as a tempfile with given
    text contents -}
testsWithTempfile ∷ 𝕋 → [(TestName, AbsFile → Assertion)] → TestTree
testsWithTempfile txt tsts =
  testsWithTempfiles "testsWithTempfile"
                     (Identity txt) (second (∘ runIdentity) ⊳ tsts)

----------

{-| tests for `testsWithTempFile` -}
testsWithTempfileTests ∷ TestTree
testsWithTempfileTests =
  let foo = "foo" ∷ 𝕋

      doTest txt exp =
        let readfile ∷ AbsFile → IO 𝕋 = ӝ ∘ readFile @IOError
         in testsWithTempfile txt [(T.unpack exp,(\ x→ readfile x ≫ (≟ exp)))]
  in testGroup "testsWithTempfile"
               [ doTest foo foo
               ]

----------------------------------------

-- This exists purely for development; it should be a strict specialization
-- of testsWithTempfiles', using [] & AbsFile rather than Each & FileAs; to
-- enable us to work against a concrete, comprehensible type signature (and use
-- the hints that the compiler may give us).
_testsWithTempfiles ∷ ∀ τ δ . (OutputData τ) ⇒
                      TestName
                    → [τ]
                    -- other IO, whose result to pass to the tests
                    → ([(AbsFile,ℍ)] → IO δ)
                    -- setup for other IO
                    → (δ → IO())
                    -- release for other IO
                    → (δ → IO())
                    → [(TestName, [AbsFile] → δ → Assertion)]
                    → TestTree

_testsWithTempfiles name
                    txts
                    acquire
                    setup
                    release tsts =
  let f_fsts          ∷ ([AbsFile]→ δ→ Assertion)→ ([(AbsFile,ℍ)],δ) → Assertion
      f_fsts f (xs,z) = f (xs & each ⊧ fst) z
      hclose          = ӝ ∘ hClose @IOError
      rm              = ӝ ∘ unlink @IOError
      tempFile        = tempfile @FPathIOError
      tsts'           ∷ IO ([(AbsFile,ℍ)],δ) → TestTree
      tsts' io_fs_x   = ioTests name (fmap (second f_fsts) tsts) io_fs_x
      acquire'        = do
        fs ← ӝ $ mapMOf each tempFile txts
        x  ← acquire fs
        return (fs,x)
      setup' (fs,x)   = do
        mapMOf_ each (hclose ∘ snd) fs
        setup x
      release' (fs,x) = do
        mapMOf_ each (rm ∘ fst) fs
        release x
  in withResourceCleanup acquire' setup' release' tsts'


----------------------------------------

{- | Perform tests using a number of testfiles, which are created as tempfiles
     with the given contents.  Also, take some arbitrary IO, with acquire, setup
     and release functions (see `ioTests`); pass the result of that IO also
     to the tests.

     Note that when the acquire is run for the arbitrary IO; the tempfiles
     have been created and written, but their filehandles not closed; the
     filehandles are closed before the setup is called for the arbitrary IO.

     This type signature roughly equates to

     > (OutputData τ, FileAs β) ⇒
     > TestName → φ τ → IO δ → (δ → IO()) → (δ → IO())
     >          → [(TestName, (φ β,δ) → Assertion)] → TestTree

     Where φ is a traversable collection; e.g., @[τ]@ or @(τ,τ,τ)@.
     Note that to use a single tempfile, you need an instance of @Each@ that has
     a single data member - e.g., @Identity@.
-}
-- Each s t a b; s is a container for a, t is a container for b
-- s -> a, t -> b, s b -> t, t a -> s
-- a is the input type, b is the output type, s, t are the input/output
-- containers respectively.

testsWithTempfiles' ∷ ∀ τ γ δ
                        σ -- container for τ; same container as used for
                          -- (γ,ℍ) when passed around, and passed to the tests
                        ξ -- container for (γ, ℍ); same container as used
                          -- for input file contents, and passed to
                          -- the tests
                        β -- the container of the type passed to each test;
                          -- roughly ([γ],δ) or ((γ,γ,γ),δ), etc.
                        .
                      (OutputData τ, -- type of tempfile contents, e.g., 𝕋
                       FileAs γ, -- fileish thing to create, e.g., AbsFile
                       Each σ ξ τ (γ,ℍ), ReturnFNFH (γ,ℍ),
                       Each ξ β (γ,ℍ) γ, Each ξ ξ (γ,ℍ) (γ,ℍ)
                      ) ⇒
                      TestName
                    → σ          {- ^ collection of texts, or similar; contents
                                      of each temp file -}
                    → (ξ → IO δ) {- ^ other IO, whose result to pass to the
                                      tests -}
                    → (δ → IO()) {- ^ setup for other IO -}
                    → (δ → IO()) {- ^ release for other IO -}
                    → [(TestName, β → δ → Assertion)]
                    → TestTree

testsWithTempfiles' name txts acquire setup release tsts =
  let -- take a function from ([file],extra_io_out) to assertion and produce a
      -- function from ([(file,handle)],extra_io_out) to assertion

      -- roughly; f_fst ∷ ([x] → y → z) → ([(x,_)],y) → z
      f_fsts f (xs,z) = f (xs & each ⊧ fst) z
      hclose        = ӝ ∘ hClose @IOError
      rm            = ӝ ∘ unlink @IOError
      tempFile      = tempfile @FPathIOError
      acquire'      = do
        fs ← ӝ $ mapMOf each tempFile txts
        x  ← acquire fs
        return (fs,x)
      setup' (fs,x)   = do
        mapMOf_ each (hclose ∘ snd) fs
        setup x
      release' (fs,x) = do
        mapMOf_ each (rm ∘ fst) fs
        release x
      -- map f_fsts to the test fn part of `[(TestName, β → δ → Assertion)]`
      -- something like
      -- tsts' ∷ IO (β,δ) → TestTree
      tsts' io_fs_x = ioTests name (fmap (second f_fsts) tsts) io_fs_x
  in withResourceCleanup acquire' setup' release' tsts'

--------------------

class Len α where
  len ∷ α → ℕ
instance Len (Identity β) where
  len _ = 1
instance Len (β,β') where
  len _ = 2
instance Len (β,β',β'') where
  len _ = 3
instance Len [β] where
  len = ỻ

{-| tests for `testsWithTempfiles'` -}
testsWithTempfiles'Tests ∷ TestTree
testsWithTempfiles'Tests =
  let foo = "foo" ∷ 𝕋
      bar = "bar" ∷ 𝕋
      baz = "baz" ∷ 𝕋

      doTest name txts exps n =
        let readfile ∷ AbsFile → IO 𝕋 = ӝ ∘ readFile @IOError
         in testsWithTempfiles' name txts (\ fs → return (len fs))
                                (const $ return ()) (const $ return ())
                                [ (T.unpack t,(\ x y → do
                                                  t' ← readfile (x⊣f)
                                                  t ≟ t'
                                                  n @=? y
                                            ))
                                | (t,f) ← exps]
      doTest' name txts exps n =
        let readfile ∷ 𝕄 AbsFile → IO 𝕋 = ӝ ∘ readFile @IOError ∘ fromJust
         in testsWithTempfiles' name txts (\ fs → return (len fs))
                                (const $ return ()) (const $ return ())
                                [ (T.unpack t,(\ x y → do
                                                  t' ← readfile (x⩼f)
                                                  t' ≟ t
                                                  n @=? y
                                            ))
                                | (t,f) ← exps]
  in testGroup "testsWithTempfiles'"
               [ doTest "foo" (Identity foo) [(foo,_1)] 1
               , doTest "foo,bar" (foo,bar) [(foo,_1),(bar,_2)] 2
               , doTest "foo,bar,baz" (foo,bar,baz)
                                      [(foo,_1),(bar,_2),(baz,_3)] 3
               , doTest' "list" [foo,bar,baz]
                                [(foo,ix 0),(bar,ix 1),(baz,ix 2)] 3
               ]

----------------------------------------

{- | Perform tests using a number of testfiles, which are created as tempfiles
     with the given contents.

     This type signature roughly equates to

     > (OutputData τ, FileAs β) ⇒ φ τ → [(TestName, φ β → Assertion)] → TestTree

     Where φ is a traversable collection; e.g., @[τ]@ or @(τ,τ,τ)@.
     Note that to use a single tempfile, you need an instance of @Each@ that has
     a single data member - e.g., @Identity@.

     This is `testsWithTempfiles'`; but with the arbitrary IO elided.
-}

testsWithTempfiles ∷ ∀ τ β σ ξ γ .
                     (OutputData τ, FileAs γ, ReturnFNFH (γ,ℍ),
                      Each σ ξ τ (γ, ℍ),
                      Each ξ β (γ,ℍ) γ, Each ξ ξ (γ, ℍ) (γ,ℍ)) ⇒
                     TestName → σ → [(TestName, β → Assertion)] → TestTree


testsWithTempfiles nm fs ts =
  let nowt = const $ return ()
   in testsWithTempfiles' nm fs nowt nowt nowt
                          (fmap (second (\ f → \ b () → f b)) ts)

----------

{-| tests for `testsWithTempfiles` -}
testsWithTempfilesTests ∷ TestTree
testsWithTempfilesTests =
  let foo = "foo" ∷ 𝕋
      bar = "bar" ∷ 𝕋
      baz = "baz" ∷ 𝕋

      doTest name txts exps =
        let readfile ∷ AbsFile → IO 𝕋 = ӝ ∘ readFile @IOError
         in testsWithTempfiles name txts
                               [ (T.unpack t,(\ x→ readfile (x⊣f) ≫ (≟ t)))
                               | (t,f) ← exps]
      doTest' name txts exps =
        let readfile ∷ 𝕄 AbsFile → IO 𝕋 = ӝ ∘ readFile @IOError ∘ fromJust
         in testsWithTempfiles name txts
                               [ (T.unpack t,(\ x→(readfile (x⩼f)) ≫ (≟ t)))
                               | (t,f) ← exps]
  in testGroup "testsWithTempfiles"
               [ doTest "foo" (Identity foo) [(foo,_1)]
               , doTest "foo,bar" (foo,bar) [(foo,_1),(bar,_2)]
               , doTest "foo,bar,baz" (foo,bar,baz) [(foo,_1),(bar,_2),(baz,_3)]
               , doTest' "list" [foo,bar,baz] [(foo,ix 0),(bar,ix 1),(baz,ix 2)]
               ]

----------------------------------------

{-| perform tests with the context chdir-d into a newly-created directory, which
    is cleaned up at exit of the function -}
testsWithTempDir'' ∷ ∀ δ γ . DirAs γ =>
                     TestName
                   → IO γ                 {- ^ top dir to create within        -}
                   → IO PathComponent     {- ^ dirname to use within top dir   -}
                   → (AbsDir → IO δ)      {- ^ other IO, whose result to pass to
                                               tests                           -}
                   → ((AbsDir, δ) → IO()) {- ^ setup for other IO              -}
                   → (δ → IO ())          {- ^ release for other IO            -}
                   → ([(TestName, (AbsDir,δ) → Assertion)])
                                          {- ^ each test to run                -}
                   → TestTree
testsWithTempDir'' name tmpdir dirnam acquire setup release tsts =
  let
      -- acquire' creates a temporary directory and passes its path to the user's
      -- acquire function
      acquire' = do
        oldpwd ← ѥ @FPathError pwd ≫ \ case
          𝓛 e → assertFailure $ [fmt|ERROR during pwd: %T|] e
          𝓡 r → return r
        tmpdir' ← tmpdir
        dirnam' ← dirnam
        dir ← ѥ (mkTempDir'' @FPathIOError tmpdir' dirnam') ≫ \ case
          𝓛 e → assertFailure $ [fmt|ERROR during mkTempDir'' %T %T: %T|]
                                tmpdir' dirnam' e
          𝓡 d → return d
        x ← acquire dir
        return (dir,oldpwd,x)

      setup' (absDir,_,x) = setup (absDir,x)

      release' (absDir,oldpwd,x) = do
        ѥ @IOError (chdir oldpwd) ≫ \ case
          𝓛 e → assertFailure $ [fmt|ERROR during chdir %T: %T|] oldpwd e
          𝓡 _ → return ()
        ѥ (nuke @IOError absDir) ≫ \ case
          𝓛 e → assertFailure $ [fmt|ERROR during tempdir nuke: %T|] e
          𝓡 _ → return ()
        release x

      tsts' ∷ IO (AbsDir,AbsDir,δ) → TestTree
      tsts' io_dir_x =
        let f ∷ (TestName,(AbsDir,δ) → Assertion)
              → (TestName,(AbsDir,AbsDir,δ) → Assertion)
            f (n, g) = (n, \ (d,_,x) → 
                              ѥ @IOError (inDir d (liftIO $ g (d,x))) ≫ \ case
                                𝓛 e → assertFailure $ [fmt|ERROR (%s): %w|] n e
                                𝓡 y → return y)
        in  ioTests name (f ⊳ tsts) io_dir_x
  in
    withResourceCleanup acquire' setup' release' tsts'

--------------------

{-| tests for testsWithTempDir'' -}
testsWithTempDir''Tests ∷ TestTree
testsWithTempDir''Tests =
  let today  ∷ IO Day = utctDay ⊳ getCurrentTime

      -- we use a specific prefix so we can check that it's empty before & after
      -- the fact
      prefix ∷ IO PathComponent -- the prefix of the tmpdir, in tempdir
      prefix = do
        prog_name  ← __progNamePrefix__
        day        ← today
        pid        ← getProcessID
        let date   = formatTime defaultTimeLocale "%F" day
            datePC = __parse__ date
            pidPC  = __parse__ $ show pid
        return $ [pc|-|] `ѹ` (prog_name :| [ datePC, pidPC ])

      glob' ∷ (MonadIO μ, Printable τ, MonadError FPathIOError μ) =>
              τ → AbsDir
            → μ ([(AbsFile,FStat)],[(AbsDir,FStat)],[(AbsFile,IOError)])
      glob' p = glob  (toString p)

  in  dependentTestGroup "testsWithTempDir''" AllSucceed $
        let testfile = [relfile|testfile|]
        in  [ -- just to show tests are working
              testCase "check" $ assertSuccess "check"
            , testCase "temp-dir-doesnt-pre-exist" $ do

                t ∷ AbsDir ← __tempdir__
                pfx ← prefix

                ѥ (glob' pfx t) ≫ \ case
                  𝓛 e → assertFailure $ [fmt|error globbing %T: %T|] t e
                  𝓡 (fs,ds,es) → do
                    if es ≠ []
                    then let es' = (\ (f,e) → [fmtT|%T: %T|] f e) ⊳ es
                         in  assertFailure$ [fmt|error(s) found globbing %T: %L|]
                                            t es'
                    else do assertEqual ([fmt|no files matching %T|] pfx) [] fs
                            assertEqual ([fmt|no directories matching %T|] pfx)
                                        [] ds

            , let nil    = const $ return ()
              in  testsWithTempDir'' "for-real" __tempdir__ prefix nil nil nil
                [ ("cwd-is-in-tempdir",
                   const $ do p ← __pwd__
                              t ← __tempdir__
                              let name = [fmt|%T is dirname of %T|] t p
                              assertBool name (t ≡ p ⊣ dirname)
                  )
                , ("have-chdir-into-tmpdir",
                   \ (d,()) → __pwd__ ≫ \ p →assertEqual([fmt|%T is %T|] d p)d p
                  )
                , ("cwd-basename-is-prefixed",
                   const $ do p ← __pwd__
                              pfx ← prefix
                              let name = [fmt|%T is pfx of basename of %T|] pfx p
                                  p'   = toText $ basename p
                              assertBool name (toText pfx `T.isPrefixOf` p')
                  )

                , ("temp-dir-is-only-one",
                   \ (d,()) → do

                    t ∷ AbsDir ← __tempdir__
                    pfx ← prefix

                    ѥ (glob' pfx t) ≫ \ case
                      𝓛 e → assertFailure $ [fmt|error globbing %T: %T|] t e
                      𝓡 (fs,ds,es) → do
                        if es ≠ []
                        then let es' = (\ (f,e) → [fmtT|%T: %T|] f e) ⊳ es
                             in  assertFailure $
                                   [fmt|error(s) found globbing %T: %L|] t es'
                        else do assertEqual([fmt|no files matching %T|] pfx)[] fs
                                assertEqual ([fmt|only %T|] d) [d] (fst ⊳ ds)
                  )

                , ("write-file", const $ do
                     let fn = [relfile|testfile|]
                     ѥ @IOError (writeFile (𝓙 0o0640) fn ("foo"∷𝕋)) ≫ \ case
                       𝓛 e  → assertFailure $ [fmt|failed writing %T: %T|]
                                               fn e
                       𝓡 () → assertSuccess $ [fmt|wrote %T|] fn
                  )

                , ("written-file-is-only-one",
                   \ (d,()) → do

                    t ∷ AbsDir ← __tempdir__

                    ѥ (glob' (""∷𝕊) d) ≫ \ case
                      𝓛 e → assertFailure $ [fmt|error globbing %T: %T|] t e
                      𝓡 (fs,ds,es) → do
                        if es ≠ []
                        then let es' = (\ (f,e) → [fmtT|%T: %T|] f e) ⊳ es
                             in  assertFailure $
                                   [fmt|error(s) found globbing %T: %L|] t es'
                        else do let testfile' = d ⫻ testfile
                                case fs of
                                  []        → assertFailure $
                                                [fmt|no files found in %T|] d
                                  [(fn,st)] → do assertEqual "only file testfile"
                                                             testfile' fn
                                                 assertEqual "file size ≠ 3"
                                                             3 (FStat.size st)
                                  _         → 
                                    assertFailure $
                                      [fmt|too many files found in %T: %L|]
                                      d (fst ⊳ fs)
                                assertEqual "no directories" [] (ds)
                  )
                ]

            , testCase "temp-dir-doesnt-post-exist" $ do

                t ∷ AbsDir ← __tempdir__
                pfx ← prefix

                ѥ (glob' pfx t) ≫ \ case
                  𝓛 e → assertFailure $ [fmt|error globbing %T: %T|] t e
                  𝓡 (fs,ds,es) → do
                    if es ≠ []
                    then let es' = (\ (f,e) → [fmtT|%T: %T|] f e) ⊳ es
                         in  assertFailure$ [fmt|error(s) found globbing %T: %L|]
                                            t es'
                    else do assertEqual ([fmt|no files matching %T|] pfx) [] fs
                            assertEqual ([fmt|no directories matching %T|] pfx)
                                        [] ds

            ]

----------------------------------------

{-| specialization of `testsWithTempDir'', with empty acquire,setup,release
    and using the system temp directory
-}
testsWithTempDir' ∷ TestName
                  → IO PathComponent     {- ^ dirname to use within top dir   -}
                  → [(TestName, AbsDir → Assertion)]
                                         {- ^ each test to run                -}
                  → TestTree
testsWithTempDir' name dirnam tsts =
  let tmpdir = ӝ @FPathIOError tempdir
      nil    = const $ return ()
  in  testsWithTempDir'' name tmpdir dirnam nil nil nil
                         (second (\ f (d, _) → f d) ⊳ tsts)

----------------------------------------

{-| specialization of `testsWithTempDir'` using an automatically generated dir
    prefix -}
testsWithTempDir ∷ TestName
                 → [(TestName, AbsDir → Assertion)] {- ^ each test to run -}
                 → TestTree
testsWithTempDir name = testsWithTempDir' name ((◇ [pc|-|]) ⊳ __progNamePrefix__)

-- tests -----------------------------------------------------------------------

{-| unit tests -}
tests ∷ TestTree
tests = dependentTestGroup "Temp" AllSucceed
                           [ testsWithTempfileTests, testsWithTempfilesTests
                           , testsWithTempfiles'Tests, testsWithTempDir''Tests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
