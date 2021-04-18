{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE ViewPatterns        #-}

module MonadIO.FPath
  ( PResolvable( pResolveDir, pResolve ), getCwd, inDir, inDirT

  , tests
  )
where

import Debug.Trace  ( trace, traceShow )

-- base --------------------------------

import qualified  Data.List

import Control.Monad           ( filterM, join, return )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Bool               ( Bool )
import Data.Either             ( Either( Left, Right ) )
import Data.Functor            ( fmap )
import Data.Function           ( ($), const )
import Data.List               ( dropWhileEnd, head, reverse, scanl, scanr,zip )
import Data.String             ( String )
import Data.Tuple              ( fst )
import GHC.Exts                ( toList )
import System.Exit             ( ExitCode )
import System.IO               ( IO )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- containers --------------------------

import Data.Sequence  ( Seq( Empty ), breakr, fromList )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString, toText )

-- directory ---------------------------

import System.Directory  ( withCurrentDirectory )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadMask, bracket )

-- filepath ----------------------------

import System.FilePath  ( FilePath, (</>), splitPath )

-- fpath -------------------------------

import FPath.Abs               ( Abs( AbsD, AbsF ), absT )
import FPath.AbsDir            ( AbsDir, absdir, __parseAbsDirP__, root )
import FPath.AbsFile           ( AbsFile, absfileT )
import FPath.AppendableFPath   ( (⫻) )
import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.Basename          ( basename )
import FPath.Dirname           ( dirname )
import FPath.Error.FPathError  ( AsFPathError, FPathIOError, _FPathEmptyE
                               , __FPathEmptyE__, __FPathNotAFileE__ )
import FPath.Parseable         ( parse )
import FPath.RelDir            ( RelDir, reldir )
import FPath.RelFile           ( relfile )

-- lens --------------------------------

import Control.Lens.Review   ( (#) )

-- monaderror-io -----------------------

import MonadError           ( ѥ )
import MonadError.IO        ( asIOError )
import MonadError.IO.Error  ( AsIOError, (~~) )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳), (⩺) )
import Data.MoreUnicode.Lens     ( (⊣), (##) )
import Data.MoreUnicode.Monad    ( (≫), (⪼) )
import Data.MoreUnicode.Natural  ( ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError, throwError )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

-- temporary ---------------------------

import System.IO.Temp ( getCanonicalTemporaryDirectory,withSystemTempDirectory )

-- text --------------------------------

import Data.Text  ( Text, last )

-- unix --------------------------------

import System.Posix.Directory  ( changeWorkingDirectory, getWorkingDirectory )
import System.Posix.Files      ( fileExist )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.FStat  ( FExists( FExists ), fexists, fexists' )

--------------------------------------------------------------------------------

{- | Current working directory -}
getCwd ∷ (AsIOError ε, AsFPathError ε, MonadError ε μ, MonadIO μ) ⇒ μ AbsDir
getCwd = let addSlash "" = ""
             addSlash t@(last → '/') = t
             addSlash t = t ⊕ "/"
          in asIOError getWorkingDirectory ≫ parse ∘ addSlash ∘ toText

getCwdTests ∷ TestTree
getCwdTests =
  let getCwd_ ∷ IO (Either FPathIOError AbsDir)
      getCwd_ = ѥ getCwd

      inTmp = inSystemTempDirectory "FPath.IO.getCwdTests"
   in testCase "getCwd" $ inTmp $ \ d → getCwd_ ≫ \ cwd → Right d @=? cwd

----------------------------------------

{- | Perform IO within a directory, with declared errors. -}
_inDir ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, Printable τ) ⇒ τ → IO α → μ α
_inDir (toString → d) io =
  -- ensure that the path is attached to the error
  (ѥ ∘ asIOError $ withCurrentDirectory d io) ≫ \ case
    Left e' → join $ throwError (e' ~~ d)-- (ioEWithPath d e')
    Right r → return r

{- | like `inDirT`, but takes IO that already throws some error(s). -}
_inDirT ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, Printable τ) ⇒
          τ → ExceptT ε IO α → μ α
_inDirT d io = join $ _inDir d (ѥ io)

{- | Perform IO in the context of a given directory. -}
inDir ∷ (MonadIO μ, AsIOError ε, MonadError ε μ) ⇒ AbsDir → IO α → μ α
inDir d = _inDir (d ## filepath)

{- | Perform MonadError-IO in the context of a given directory.  This is for
     when you have IO already bound with errors; e.g.,
     `As*Error ε, MonadError ε μ` on the input IO.
 -}
inDirT ∷ (MonadIO μ,AsIOError ε,MonadError ε μ) ⇒ AbsDir → ExceptT ε IO α → μ α
inDirT d io = join $ inDir d (ѥ io)

scan ∷ (α → α → α) → α → [α] → [(α,α)]
scan f b xs = zip (scanl f b xs) (scanr f b xs)

{- | List of splits of a file path in two, at each directory along the path;
     e.g.,

     @
       λ> splitPoints "foo/bar/"
       [("","foo/bar"),("foo/","bar"),("foo/bar","")]
     @

     Note that any trailing '/' is dropped.
 -}
splitPoints ∷ FilePath → [(FilePath,FilePath)]
splitPoints f =
  let noSlash ∷ FilePath → FilePath
      noSlash p = let noSlash' ∷ FilePath → FilePath
                      noSlash' "/" = "/"
                      noSlash' t = dropWhileEnd (≡ '/') t
                      -- protect against //…
                   in case noSlash' p of
                        "" → "/"
                        _  → p
   in scan (</>) "" (splitPath $ noSlash f)

splitPointsTests ∷ TestTree
splitPointsTests =
  testGroup "splitPoints" $
    let check sp exp = testCase sp $ exp @=? splitPoints sp
     in [ check ""          [("","/"),("/","")]
        , check "/"         [("","/"),("/","")]
        , check "/foo"      [("","/foo"),("/","foo"),("/foo","")]
        , check "./"        [("","./"),("./","")]
        , check "./foo"     [("","./foo"),("./","foo"),("./foo","")]
        , check "bar/./foo" [("","bar/./foo"),("bar/","./foo"),("bar/./","foo")
                            ,("bar/./foo","")]
        , check "///"       [("","/"),("/","")]
        , check ".///"      [("",".///"),(".///","")]
        , check "//./"      [("","//./"),("//","./"),("//./","")]
        , check "///."      [("","///."),("///","."),("///.","")]
        , check "/foo/bar/" [("","/foo/bar/"),("/","foo/bar/")
                            ,("/foo/","bar/"),("/foo/bar/","")]
        ]

{- | Given an absdir and a subsequent filepath (which might be absolute),
     return a pair of the initial filepath that exists, *resolved* (symlinks,
     ., .., all resolved); and a part that does not exist.  Note that the
     extant part must have read-and-execute permission for the user for every
     dir along the way, and read for the final file (if any); else an
     `AsIOError` will be raised.
 -}
resolve ∷ (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒
          AbsDir → FilePath → μ (FilePath, FilePath)
resolve d fp =
  let -- prepend `d`, note this is a no-op for input abs functions
      prepend ∷ FilePath → FilePath
      prepend = (filepath # d </>)
      fexist ∷ (MonadIO μ, AsIOError ε, MonadError ε μ) ⇒ FilePath → μ Bool
      fexist  = asIOError ∘ fileExist
      -- Given an AbsDir, `resolve` must resolve to *something* valid, since the
      -- top of an AbsDir is the root dir, and that always exists.
--   in head ⩺ filterM (fexist ∘ fst) ∘ reverse ∘ splitPoints ∘ prepend
--   in head ⩺ filterM ((FExists ≡) ∘ fexists ∘ fst) ∘ reverse ∘ splitPoints ∘ prepend
   in traceShow ("resolve", d,fp,(reverse ∘ splitPoints ∘ prepend) fp) $ (head ⩺ filterM ((fmap (FExists ≡)) ∘ fexists ∘ fst) ∘ reverse ∘ splitPoints ∘ prepend) fp

------------------------------------------------------------

{- | Things which are physically resolvable -}
class PResolvable α where
  {- | Given a path, which might well be relative and include '..' and/or '.',
       physically resolve that to an α by starting at a given `AbsDir`.
       This involves trying to @chdir@ to the target directory; so will only
       work for directories that you have permission to @chdir@ into (for files,
       you need to be able to @chdir@ into the parent directory).
   -}
  pResolveDir ∷ (Printable τ, AsIOError ε, AsFPathError ε, MonadError ε μ,
                 MonadIO μ) ⇒
                AbsDir → τ → μ α

  {- | `pResolveDir`, taking the current working directory as the starting point
   -}
  pResolve ∷ (Printable τ, AsIOError ε, AsFPathError ε, MonadError ε  μ,
              MonadIO μ) ⇒
             τ → μ α
  pResolve f = getCwd ≫ \ d → pResolveDir d f

{- | Physically resolve the whole path, thus every directory (including the last
     must) exist.  Treats the lack of a trailing '/' on the input stringlike
     kindly; that is, even without a trailing '/', it is considered as a
     directory.
 -}
instance PResolvable AbsDir where
  pResolveDir ∷ (Printable τ, AsIOError ε, AsFPathError ε, MonadError ε μ,
                 MonadIO μ) ⇒
                AbsDir → τ → μ AbsDir
  pResolveDir d (toString → p) = traceShow ("_pResolveDir", d, p) $ trace "pResolveDir⫽AbsDir" $ do
    (extant,non_extant) ← trace "resolved" $ resolve d p
    d' ← traceShow ("_inDirT") $ _inDirT extant getCwd
    let -- add a trailing / so reldir parses it
        toDir ∷ FilePath → FilePath
        toDir "" = "./"
        toDir t  = case Data.List.last t of -- last is safe, t is non-empty
                     '/' → t
                     _   → t ⊕ "/"
    p' ← traceShow ("-> parse", non_extant, toDir non_extant) $ parse @RelDir (toDir non_extant)
    return $ d' ⫻ p'

----------

pResolveAbsDirTests ∷ TestTree
pResolveAbsDirTests =
  let tName   = "pResolveTests.AbsDir"
      inTmp   = inSystemTempDirectory tName
      withTmp ∷ (MonadIO μ, MonadMask μ) ⇒ (AbsDir → μ α) → μ α
      withTmp = withSystemTempDirectory tName ∘ (∘ __parseAbsDirP__)

      pResolve_ ∷ Text → IO (Either FPathIOError AbsDir)
      pResolve_ = ѥ ∘ pResolve

      getTmpdir ∷ IO AbsDir
      getTmpdir = __parseAbsDirP__ ⊳ getCanonicalTemporaryDirectory

   in testGroup "AbsDir"
        [ testCase "inTmp ./" $ inTmp $ \ d → pResolve_ "./" ≫ (Right d @=?)
        , testCase "inTmp . (forgiveness of pResolve wrt trailing /)" $
            inTmp $ \ d → pResolve_ "."  ≫ (Right d @=?)
        , testCase "inTmp .." $
            inTmp ∘ const $
                    getTmpdir ≫ \ tmpdir → pResolve_ ".."  ≫ (Right tmpdir @=?)

        , testCase "inTmp cwd" $
            -- value is an abs dir, e.g., /tmp/<user>/d7b66efeebbcf249/
            inTmp $ \ d → pResolve_ (toText d) ≫ (Right d @=?)
        , testCase "root" $
            inTmp $ \ _ → pResolve_ "/" ≫ (Right [absdir|/|] @=?)
        , testCase "/nonsuch/" $
            inTmp $ \ _ → pResolve_ "/nonsuch/" ≫
                            (Right [absdir|/nonsuch/|] @=?)
        , testCase "inTmp nonsuch" $
            inTmp $ \ d → pResolve_ "nonsuch" ≫
                          (Right (d ⫻ [reldir|nonsuch/|]) @=?)
        , testCase "inTmp nonesuch" $
            inTmp $ \ _ → pResolve_ "/nonsuch" ≫
                          (Right [absdir|/nonsuch/|] @=?)
        , testCase "inTmp nonsuch/" $
            inTmp $ \ d → pResolve_ "nonsuch/" ≫
                          (Right (d ⫻ [reldir|nonsuch/|]) @=?)

        , testCase "inTmp ./" $ inTmp $ \ d → do
            d' <- pResolve_ "nonsuch/nonsuch"
            Right (d ⫻ [reldir|nonsuch/nonsuch/|]) @=? d'

        , testCase "inTmp ../ (dirname)" $
            inTmp $ \ d → pResolve_ "../" ≫ ((Right (d ⊣ dirname) @=?))
        , testCase "inTmp ../ (basename)" $
            inTmp $ \ d → pResolve_ "../" ≫
                            ((Right d @=?) ∘ fmap (⫻ basename d))
        ]

----------

pResolveDirAbsDirTests ∷ TestTree
pResolveDirAbsDirTests =
  let tName   = "pResolveDirTests.AbsDir"
      inTmp   = inSystemTempDirectory tName
      withTmp ∷ (MonadIO μ, MonadMask μ) ⇒ (AbsDir → μ α) → μ α
      withTmp = withSystemTempDirectory tName ∘ (∘ __parseAbsDirP__)

      pResolveDir_ ∷ AbsDir → Text → IO (Either FPathIOError AbsDir)
      pResolveDir_ d = ѥ ∘ pResolveDir d

      getTmpdir ∷ IO AbsDir
      getTmpdir = __parseAbsDirP__ ⊳ getCanonicalTemporaryDirectory

   in testGroup "AbsDir"
        [ testCase "withTmp ./" $
            withTmp $ \ d → pResolveDir_ d "./" ≫ (Right d @=?)
        , testCase "withTmp .//" $
            withTmp $ \ d → pResolveDir_ d ".//" ≫ (Right d @=?)
        , testCase "withTmp /" $
            withTmp $ \ d → pResolveDir_ d "/" ≫ (Right root @=?)
        , testCase "withTmp //" $
            withTmp $ \ d → pResolveDir_ d "//" ≫ (Right root @=?)
        , testCase "withTmp ." $
            withTmp $ \ d → pResolveDir_ d "." ≫ (Right d @=?)
        , testCase "withTmp .." $
            withTmp $ \ d → getTmpdir ≫ \ tmpdir →
                      pResolveDir_ d ".." ≫ (Right tmpdir @=?)
        ]

----------------------------------------

{- | Physically resolve every directory up to and including the dirname of the
     input stringlike; and then tacks the file basename onto the end.  Treats a
     trailing '/' as a dir, and thus fails.
 -}
instance PResolvable AbsFile where
  pResolveDir ∷ (Printable τ, AsIOError ε, AsFPathError ε, MonadError ε μ,
                 MonadIO μ)⇒
                AbsDir → τ → μ AbsFile
  pResolveDir d (toString → f) =
    -- we can't simply use parseRelFile, etc., here, as we want to accept
    -- paths with '..' and '.' in them (and resolve them)
    case breakr (≡ '/') $ fromList f of
      -- first element of tuple is suffix of seq (a little counterintuitively)
      (Empty, Empty) → -- f was empty
                       __FPathEmptyE__    absfileT
      (Empty, _    ) → -- f had a trailing /
                       __FPathNotAFileE__ absfileT (toText f)


      (_, Empty    ) → -- just a file, no dir part
                       do c ∷ AbsDir ← pResolveDir d ("."∷Text)
                          (c ⫻) ⊳ parse f

      (x    , y    ) → -- dir + file
                       do c ← pResolveDir d (toList y)
                          (c ⫻) ⊳ parse (toList x)

pResolveAbsFileTests ∷ TestTree
pResolveAbsFileTests =
  let tName   = "FPath.IO.pResolveTests.AbsFile"
      inTmp   = inSystemTempDirectory tName
      withTmp ∷ (MonadIO μ, MonadMask μ) ⇒ (AbsDir → μ α) → μ α
      withTmp = withSystemTempDirectory tName ∘ (∘ __parseAbsDirP__)

      pResolve_ ∷ Text → IO (Either FPathIOError AbsFile)
      pResolve_ = ѥ ∘ pResolve

      pResolveDir_ ∷ AbsDir → Text → IO (Either FPathIOError AbsFile)
      pResolveDir_ d = ѥ ∘ pResolveDir d

   in testGroup "AbsFile"
        [ testCase "inTmp '' x" $
            inTmp $ \ d → pResolve_ "x" ≫
                            (Right (d ⫻ [relfile|x|] ∷ AbsFile) @=?)
        , testCase "withTmp '' x" $
            withTmp $ \ d → pResolveDir_ d "x" ≫
                          (Right (d ⫻ [relfile|x|] ∷ AbsFile) @=?)
        , testCase "inTmp ./ x" $
            inTmp $ \ d → pResolve_ "./x" ≫
                          (Right (d ⫻ [relfile|x|] ∷ AbsFile) @=?)
        , testCase "withTmp ./ x" $
            withTmp $ \ d → pResolveDir_ d "./x" ≫
                          (Right (d ⫻ [relfile|x|] ∷ AbsFile) @=?)
        , testCase "inTmp ../ x" $
            inTmp $ \ d → pResolve_ "../x" ≫
                          (Right (d ⊣ dirname ⫻ [relfile|x|] ∷ AbsFile) @=?)
        , testCase "withTmp ../ x" $
            withTmp $ \ d → pResolveDir_ d "../x" ≫
                          (Right (d ⊣ dirname ⫻ [relfile|x|] ∷ AbsFile) @=?)
        ]


{- | Given a path, which might well relative include '..' and/or '.', physically
     resolve that to an Abs.  Relative paths are contextual to the cwd.
     Input with a trailing '/', "/.", or "/.."; or the special cases "." and
     ".." are resolved to directories; without are resolved to files.  Empty
     input strings cause a failure.
 -}
instance PResolvable Abs where
  pResolveDir ∷ (Printable τ, AsIOError ε, AsFPathError ε, MonadError ε μ,
                 MonadIO μ)⇒
                AbsDir → τ → μ Abs
  pResolveDir _ (toString → [])                       = __FPathEmptyE__ absT
  pResolveDir d t@(toString → ".")                    = AbsD ⊳ pResolveDir d t
  pResolveDir d t@(toString → "..")                   = AbsD ⊳ pResolveDir d t
  pResolveDir d t@(reverse ∘ toString → '/' : _)      = AbsD ⊳ pResolveDir d t
  pResolveDir d t@(reverse ∘ toString → '.' : '/' : _)= AbsD ⊳ pResolveDir d t
  pResolveDir d t@(reverse ∘ toString → '.':'.':'/':_)= AbsD ⊳ pResolveDir d t
  pResolveDir d t                                     = AbsF ⊳ pResolveDir d t

pResolveAbsTests ∷ TestTree
pResolveAbsTests =
  let tName   = "FPath.IO.pResolveTests.Abs"
      withTmp ∷ (MonadIO μ, MonadMask μ) ⇒ (AbsDir → μ α) → μ α
      withTmp = withSystemTempDirectory tName ∘ (∘ __parseAbsDirP__)

      pResolveDir_ ∷ AbsDir → Text → IO (Either FPathIOError Abs)
      pResolveDir_ d = ѥ ∘ pResolveDir d

   in testGroup "Abs"
        [ testCase "withTmp ''" $
            withTmp $ \ d → pResolveDir_ d "" ≫ (Left (_FPathEmptyE absT) @=?)
        , testCase "withTmp ./" $
            withTmp $ \ d → pResolveDir_ d "./" ≫ (Right (AbsD d) @=?)
        , testCase "withTmp ." $
            withTmp $ \ d → pResolveDir_ d "." ≫ (Right (AbsD d) @=?)
        , testCase "withTmp .." $
            withTmp $ \ d → pResolveDir_ d ".." ≫
                              (Right (AbsD (d ⊣ dirname)) @=?)
        , testCase "withTmp ../" $
            withTmp $ \ d → pResolveDir_ d "../" ≫
                              (Right (AbsD (d ⊣ dirname)) @=?)
        , testCase "withTmp ../." $
            withTmp $ \ d → pResolveDir_ d "../." ≫
                              (Right (AbsD (d ⊣ dirname)) @=?)
        , testCase "withTmp ./../." $
            withTmp $ \ d →
                      pResolveDir_ d "./../."≫(Right (AbsD (d ⊣ dirname)) @=?)
        , testCase "withTmp .././." $
            withTmp $ \ d →
                      pResolveDir_ d ".././."≫(Right (AbsD (d ⊣ dirname)) @=?)

        , testCase "withTmp ''" $
            withTmp $ \ d → pResolveDir_ d "" ≫ (Left (_FPathEmptyE absT) @=?)
        , testCase "withTmp '' x" $
            withTmp $ \ d → pResolveDir_ d "x" ≫
                          (Right (AbsF (d ⫻ [relfile|x|])) @=?)
        , testCase "withTmp ./ x" $
            withTmp $ \ d → pResolveDir_ d "./x" ≫
                          (Right (AbsF (d ⫻ [relfile|x|])) @=?)
        , testCase "withTmp ../ x" $
            withTmp $ \ d → pResolveDir_ d "../x" ≫
                          (Right (AbsF (d ⊣ dirname ⫻ [relfile|x|])) @=?)
        ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

inSystemTempDirectory ∷ String → (AbsDir → IO α) → IO α
inSystemTempDirectory t io =
  withSystemTempDirectory t $ \ d →
    bracket (getWorkingDirectory ≫  \ o → changeWorkingDirectory d ⪼ return o)
            changeWorkingDirectory
            (\ _ → io $ __parseAbsDirP__ d)

pResolveDirTests ∷ TestTree
pResolveDirTests = testGroup "pResolveDir" [ pResolveDirAbsDirTests ]

pResolveTests ∷ TestTree
pResolveTests = testGroup "pResolve" [ pResolveAbsDirTests
                                     , pResolveAbsFileTests, pResolveAbsTests ]

tests ∷ TestTree
tests = testGroup "MonadIO.FPath" [ getCwdTests, splitPointsTests
                                  , pResolveTests, pResolveDirTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
