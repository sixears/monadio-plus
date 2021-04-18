{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

-- Split here so that FPath, File can both use it

module MonadIO.FStat
  ( FExists(..), fexists, fexists', lfexists, lfexists', lstat, stat, tests )
where

-- base --------------------------------

import qualified  System.IO

import Control.Monad           ( Monad, (>=>), filterM, forM_, join, return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bool               ( Bool( False, True ), bool )
import Data.Eq                 ( Eq )
import Data.Function           ( ($) )
import Data.Functor            ( fmap )
import Data.Maybe              ( Maybe( Just, Nothing ), isJust )
import System.Exit             ( ExitCode )
import System.IO               ( FilePath, IO )
import Text.Show               ( Show )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode      ( (∧), (∨) )
import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString, toText )

-- fpath -------------------------------

import FPath.Abs               ( Abs( AbsD, AbsF ) )
import FPath.AbsDir            ( AbsDir, NonRootAbsDir, absdir, root )
import FPath.AbsFile           ( AbsFile, absfile, absfileT )
import FPath.AppendableFPath   ( (⫻) )
import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.AsFilePath'       ( exterminate )

-- fstat -------------------------------

import FStat  ( FStat, FileType( Directory, SymbolicLink ), ftype, mkfstat )

-- monadio-error -----------------------

import MonadError           ( ѥ, splitMError )
import MonadError.IO        ( ӝ, asIOError, asIOErrorY )
import MonadError.IO.Error  ( AsIOError, IOError, (~~)
                            , _IOErr, squashInappropriateTypeT )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool     ( 𝔹 )
import Data.MoreUnicode.Functor  ( (⊳), (⊳⊳), (⊳⊳⊳), (⩺) )
import Data.MoreUnicode.Lens     ( (⊣), (⫥), (⊢) )
import Data.MoreUnicode.Maybe    ( 𝕄 )
import Data.MoreUnicode.Monad    ( (≫), (⪼) )
import Data.MoreUnicode.Natural  ( ℕ )
import Data.MoreUnicode.String   ( 𝕊 )
import Data.MoreUnicode.Text     ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError, throwError )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup, withResource )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), assertIsLeft, assertRight, runTestsP, runTestsReplay
                  , runTestTree )

-- safe --------------------------------

import Safe  ( headMay, lastDef, lastMay )

-- unix --------------------------------

import System.Posix.Files  ( FileStatus, fileExist, getFileStatus
                           , getSymbolicLinkStatus, readSymbolicLink, removeLink
                           , setFileMode
                           )
--------------------------------------------------------------------------------

data FExists = FExists | NoFExists
  deriving (Eq,Show)

{- | Does this 𝕄 FStat refer to a directory? -}
mIsDir ∷ 𝕄 FStat → 𝔹
mIsDir (fmap ftype → Just Directory) = True
mIsDir _                             = False

fexists_ ∷ (Monad η, AsFilePath ρ) ⇒ 𝔹 → (ρ → η (𝕄 FStat)) → ρ → η FExists
fexists_ checkDir g f = bool NoFExists FExists ⊳ do
  s ← g f
  if checkDir ∧ '/' ≡ lastDef '\0' (f ⫥ filepath)
  then return (mIsDir s)
  else return (isJust s)

{- | Does file exist.  Note that "does /etc/passwd/ exist?", where /etc/passwd
     exists but is a file, will return `NoFExists`; but "does /etc exist?" where
     /etc exists but is a directory will return `FExists`.  See also `fexists'`.

     Symlinks are dereferenced; so dangling symlinks are considered to not
     exist.
 -}
fexists ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath τ) ⇒ τ → μ FExists
fexists = fexists_ True stat

{- | Like `fexists`; but for symlinks, checks the symlink rather than
     dereferencing; so dangling symlinks are considered to exist. -}
lfexists ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath τ) ⇒
           τ → μ FExists
lfexists = fexists_ True lstat

----------

fexistsTests ∷ TestTree
fexistsTests =
  let testFExists expect input =
        testCase (toString input) $
          (ѥ @IOError (fexists input)) ≫ assertRight (expect @=?)
   in testGroup "fexists"
                [ testFExists FExists   [absdir|/etc/|]
                , testFExists NoFExists [absdir|/nonsuch/|]
                , testFExists NoFExists [absdir|/etc/nonsuch/|]
                , testFExists FExists   [absfile|/etc/passwd|]
                , testFExists NoFExists [absdir|/etc/passwd/|]
                , testFExists NoFExists [absfile|/etc/passwd/nonsuch|]
                , testFExists NoFExists [absdir|/etc/passwd/nonsuch/|]
                ]

--------------------

{- | Does file exist.  Note that "does /etc/passwd/ exist?", where /etc/passwd
     exists but is a file, will return `FExists`.  See also `fexists`.  This is
     more symmetric, since "does /etc exist?" where /etc exists but is a
     directory will return `FExists`; but at a cost of being arguably less
     accurate.
 -}
fexists' ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath τ)⇒ τ → μ FExists
fexists' = fexists_ False stat

lfexists' ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath τ) ⇒
            τ → μ FExists
lfexists' = fexists_ False lstat

----------

fexists'Tests ∷ TestTree
fexists'Tests =
  let testFExists' expect input =
        testCase (toString input) $
          (ѥ @IOError (fexists' input)) ≫ assertRight (expect @=?)
   in testGroup "fexists'"
                [ testFExists' FExists   [absdir|/etc/|]
                , testFExists' NoFExists [absdir|/nonsuch/|]
                , testFExists' NoFExists [absdir|/etc/nonsuch/|]
                , testFExists' FExists   [absfile|/etc/passwd|]
                , testFExists' FExists   [absdir|/etc/passwd/|]
                , testFExists' NoFExists [absfile|/etc/passwd/nonsuch|]
                , testFExists' NoFExists [absdir|/etc/passwd/nonsuch/|]
                ]

----------------------------------------

{- | File stat; returns Nothing if file does not exist.  Note that `stat`-ing
     a "directory" that is really a file (e.g., `/etc/passwd/`) will just stat
     the file (`/etc/passwd` in our example).
  -}
_stat ∷ ∀ ε ρ μ . (MonadIO μ, AsFilePath ρ, AsIOError ε, MonadError ε μ) ⇒
        (FilePath → IO FileStatus) → ρ → μ (𝕄 FStat)
_stat s fn = do
  let fp = exterminate $ fn ⫥ filepath
   in join ⊳⊳ squashInappropriateTypeT ∘ asIOErrorY ∘ fmap mkfstat ∘ s $ fp

-- | file stat; returns Nothing if file does not exist
stat ∷ ∀ ε ρ μ . (MonadIO μ, AsFilePath ρ, AsIOError ε, MonadError ε μ) ⇒
       ρ → μ (𝕄 FStat)
stat = _stat getFileStatus

----------------------------------------

{- | File stat; returns Nothing if file does not exist.  If the file is a
     symlink, return the stat of the symlink; cf. `stat`, which looks "through"
     the symlink to the file itself.
 -}
lstat ∷ ∀ ε ρ μ . (MonadIO μ, AsFilePath ρ, AsIOError ε, MonadError ε μ) ⇒
        ρ → μ (𝕄 FStat)
lstat = _stat getSymbolicLinkStatus

----------

statTests ∷ TestTree
statTests =
  let testStat expect input f =
        testCase (toString input) $
          f (ѥ @IOError (stat input)) ≫ assertRight (expect @=?)
      isDirectory = ((Directory ≡) ∘ ftype)
   in testGroup "stat"
                [ testStat (Just True)  [absdir|/etc/|]        (isDirectory ⊳⊳⊳)
                , testStat (Just False) [absfile|/etc/passwd|] (isDirectory ⊳⊳⊳)
                , testStat (Just False) [absdir|/etc/passwd/|] (isDirectory ⊳⊳⊳)
                , testStat Nothing      [absfile|/nonsuch|]    (isDirectory ⊳⊳⊳)
                , testStat Nothing      [absfile|/etc/passwd/nonsuch|]
                                                               (isDirectory ⊳⊳⊳)
                , testStat Nothing      [absdir|/nonsuch/|]    (isDirectory ⊳⊳⊳)
                ]

----------------------------------------

tests ∷ TestTree
tests = testGroup "MonadIO.FStat" [ fexistsTests, fexists'Tests, statTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
