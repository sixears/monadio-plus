{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

-- Split here so that FPath, File can both use it

module MonadIO.FStat
  ( FExists(..), extantP, extantP', fexists, fexists', lfexists, lfexists'
  , lstat, stat, tests )
where

-- base --------------------------------

import Control.Monad           ( Monad, join, return, sequence )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Bool               ( Bool( False, True ), bool )
import Data.Eq                 ( Eq )
import Data.Function           ( ($) )
import Data.Functor            ( fmap )
import Data.Maybe              ( Maybe( Just, Nothing ), isJust )
import System.Exit             ( ExitCode )
import System.IO               ( FilePath, IO )
import Text.Show               ( Show )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode      ( (∧) )
import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString )

-- fpath -------------------------------

import FPath.AbsDir       ( absdir )
import FPath.AbsFile      ( absfile )
import FPath.AsFilePath   ( AsFilePath( filepath ) )
import FPath.AsFilePath'  ( exterminate )
import FPath.DirType      ( DirType )
import FPath.Parent       ( HasParentMay, parents' )

-- fstat -------------------------------

import FStat  ( FStat, FileType( Directory ), ftype, mkfstat )

-- monadio-error -----------------------

import MonadError           ( ѥ, eFromMaybe )
import MonadError.IO        ( asIOErrorY )
import MonadError.IO.Error  ( AsIOError, IOError
                            , squashInappropriateTypeT, userE )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool     ( 𝔹 )
import Data.MoreUnicode.Functor  ( (⊳), (⊳⊳), (⊳⊳⊳) )
import Data.MoreUnicode.Lens     ( (⫥) )
import Data.MoreUnicode.Maybe    ( 𝕄 )
import Data.MoreUnicode.Monad    ( (≫) )
import Data.MoreUnicode.Natural  ( ℕ )
import Data.MoreUnicode.String   ( 𝕊 )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( assertRight, runTestsP, runTestsReplay
                  , runTestTree )

-- safe --------------------------------

import Safe  ( lastDef, lastMay )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- unix --------------------------------

import System.Posix.Files  ( FileStatus, getFileStatus, getSymbolicLinkStatus )

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

{- | Like `extantP`, but gives a Maybe lest there be no valid parent.  Honestly,
     I'm not even sure how to make that happen for the sake of testing. -}
extantP' ∷ ∀ ε α μ .
          (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath (DirType α),
           HasParentMay α, HasParentMay (DirType α),
           DirType α ~ DirType (DirType α), DirType (DirType α) ~ α) ⇒
          α -> μ (Maybe (DirType α))
extantP' f = do
  fex ← (sequence $ (\ d -> (d,) ⊳ fexists d) ⊳ parents' f)
  return $ lastMay [ d | (d,g) ← fex, g ≡ FExists ]

--------------------

{- | Find the closest ancestor (longest prefix subpath) of `f` that exists
     (possibly including `f` itself).
     The complex type signature in practice roughly equates `δ` to `Dir` or
     `AbsDir` or `RelDir`.
     This should always give a result, since at a minimum, `/` (for `AbsDir`)
     or `./` (for `RelDir`) should exist (even if pwd is a since-deleted
     directory, `./` should still exist); if that is somehow violated, an
     `IOError` will be thrown.
 -}
extantP ∷ ∀ ε δ μ .
          (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath (DirType δ),
           HasParentMay δ, HasParentMay (DirType δ),
           Printable δ, DirType δ ~ δ,
           DirType δ ~ DirType (DirType δ), DirType (DirType δ) ~ δ) ⇒
          δ -> μ (DirType δ)
extantP f = extantP' f ≫ eFromMaybe (userE $ [fmt|'%T' has no extant parent|] f)

--------------------------------------------------------------------------------

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
