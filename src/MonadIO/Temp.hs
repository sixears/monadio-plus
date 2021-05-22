{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module MonadIO.Temp
  ( mkTempDir
  , tempfile, tempfile', tempfile''
  , withTempDir'', withTempDirCD, withTempDirCD'
  , withTempfile, withTempfile', withTempfile'', withTempfile'''
  )
where

-- base --------------------------------

import qualified  System.IO

import Control.Monad           ( (>=>), join, forM_, mapM_, return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Either             ( either )
import Data.Function           ( ($), flip )
import Data.List               ( dropWhileEnd )
import Data.Tuple              ( uncurry )
import GHC.Stack               ( HasCallStack )
import System.Environment      ( getProgName )
import System.IO               ( FilePath, Handle, IO
                               , SeekMode( AbsoluteSeek )
                               , char8, hSeek, hSetEncoding, hSetNewlineMode
                               , nativeNewlineMode, noNewlineTranslation, utf8
                               )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- bytestring --------------------------

import qualified Data.ByteString  as  BS

import Data.ByteString  ( ByteString )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadMask, bracket )

-- fpath -------------------------------

import FPath.AbsDir            ( AbsDir )
import FPath.AbsFile           ( AbsFile )
import FPath.AsFilePath        ( filepath )
import FPath.Dir               ( DirAs( _Dir_ ) )
import FPath.Error.FPathError  ( AsFPathError )
import FPath.Parseable         ( Parseable( parse ) )
import FPath.PathComponent     ( PathComponent )

-- lens --------------------------------

import Control.Lens.Review  ( review )

-- monaderror-io -----------------------

import MonadError           ( ѥ )
import MonadError.IO        ( asIOError, asIOErrorT )
import MonadError.IO.Error  ( AsIOError, IOError )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⫥) )
import Data.MoreUnicode.Monad    ( (≫), (⪼) )
import Data.MoreUnicode.Text     ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )
import Control.Monad.State   ( MonadState, modify, runStateT )
import Control.Monad.Trans   ( lift )

-- temporary ---------------------------

import System.IO.Temp  ( createTempDirectory, getCanonicalTemporaryDirectory
                       , withTempDirectory )

-- text --------------------------------

import qualified  Data.Text.IO  as  TextIO

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO            ( warn )
import MonadIO.Base       ( hClose, unlink )
import MonadIO.Directory  ( inDir )

--------------------------------------------------------------------------------

type ℍ  = Handle
type 𝔹𝕊 = ByteString

----------------------------------------

parseAbsDir ∷ (AsFPathError ε, MonadError ε η) ⇒ FilePath → η AbsDir
parseAbsDir = parse ∘ (⊕ "/") ∘ dropWhileEnd (≡ '/')

----------------------------------------

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
     `ByteStrings` are written as bytes with no newline translation.  The
     `Handle` is left in whatever encoding & newline-translation is implied by
     the input type.
 -}

class OutputData τ where
  output ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack) ⇒ ℍ → τ → μ ()

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
tempfile t = progNamePrefix ≫ \ p → tempfile' p t

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
               → PathComponent       -- ^ Pattern to use for the temfile
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
withTempfile t io = progNamePrefix ≫ \ p → withTempfile' p t io


----------------------------------------

{- | Get the system temporary directory (TMPDIR, etc.) -}
tempdir ∷ ∀ ε μ .
          (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ,
           HasCallStack) ⇒
          μ AbsDir
tempdir = asIOError getCanonicalTemporaryDirectory ≫ parseAbsDir

----------------------------------------

{- | Create a temporary directory as a subdir of a given dir; return its name.
     It is the responsibility of the caller to arrange appropriate cleanup. -}
mkTempDir'' ∷ ∀ ε δ μ .
              (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ,
               HasCallStack, DirAs δ) ⇒
              δ → PathComponent → μ AbsDir
mkTempDir'' (review $ filepath ∘ _Dir_ → t) (review filepath → r) = do
  d ← liftIO $ createTempDirectory t r
  parseAbsDir d

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
mkTempDir = progNamePrefix ≫ mkTempDir'

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

{- | A prefix (suitable for, e.g., temp files or dirs) in the form of a
     `RelFile` (which is the programme name, plus a '-' character. -}
progNamePrefix ∷ ∀ ε μ .
                 (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ,
                  HasCallStack) ⇒
                 μ PathComponent
progNamePrefix = asIOError getProgName ≫ parse ∘ (⊕ "-")

{- | Like `withTempDir'`, with the prefix being the program name plus `"-"`. -}
withTempDir ∷ ∀ ε ω μ .
              (MonadIO μ, MonadMask μ, AsFPathError ε, AsIOError ε,
               MonadError ε μ, HasCallStack) ⇒
              (AbsDir → ExceptT ε IO ω) → μ ω
withTempDir io = progNamePrefix ≫ \ p → withTempDir' p io

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

-- that's all, folks! ----------------------------------------------------------
