{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

-- Split here so that FPath, File can both use it

module MonadIO.FStat
  ( FExists(..), extantP, extantP', fexists, fexists', isDir, lisDir, lfexists
  , lfexists', lstat, stat, tests )
where

import Base1T

-- base --------------------------------

import Data.Bool   ( bool )
import Data.Maybe  ( isJust )
import System.IO   ( FilePath )

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

import MonadError           ( eFromMaybe )
import MonadError.IO        ( asIOErrorY )
import MonadError.IO.Error  ( IOError, squashInappropriateTypeT )

-- safe --------------------------------

import Safe  ( lastDef, lastMay )

-- unix --------------------------------

import System.Posix.Files  ( FileStatus, getFileStatus, getSymbolicLinkStatus )

--------------------------------------------------------------------------------

data FExists = FExists | NoFExists
  deriving (Eq,Show)

{- | Does this 𝕄 FStat refer to a directory? -}
mIsDir ∷ 𝕄 FStat → 𝔹
mIsDir (fmap ftype → 𝕵 Directory) = 𝕿
mIsDir _                          = 𝕱

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
fexists = fexists_ 𝕿 stat

{- | Like `fexists`; but for symlinks, checks the symlink rather than
     dereferencing; so dangling symlinks are considered to exist. -}
lfexists ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath τ) ⇒
           τ → μ FExists
lfexists = fexists_ 𝕿 lstat

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
fexists' = fexists_ 𝕱 stat

lfexists' ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath τ) ⇒
            τ → μ FExists
lfexists' = fexists_ 𝕱 lstat

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
                [ testStat (𝕵 𝕿) [absdir|/etc/|]               (isDirectory ⊳⊳⊳)
                , testStat (𝕵 𝕱) [absfile|/etc/passwd|]        (isDirectory ⊳⊳⊳)
                , testStat (𝕵 𝕱) [absdir|/etc/passwd/|]        (isDirectory ⊳⊳⊳)
                , testStat 𝕹     [absfile|/nonsuch|]           (isDirectory ⊳⊳⊳)
                , testStat 𝕹     [absfile|/etc/passwd/nonsuch|](isDirectory ⊳⊳⊳)
                , testStat 𝕹     [absdir|/nonsuch/|]           (isDirectory ⊳⊳⊳)
                ]

----------------------------------------

{- | Like `extantP`, but gives a Maybe lest there be no valid parent.  Honestly,
     I'm not even sure how to make that happen for the sake of testing. -}
extantP' ∷ ∀ ε α μ .
          (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath (DirType α),
           HasParentMay α, HasParentMay (DirType α),
           DirType α ~ DirType (DirType α), DirType (DirType α) ~ α) ⇒
          α -> μ (𝕄 (DirType α))
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

----------------------------------------

{- | Find if a path is a directory, by checking the filesystem.  If the path
     is a symlink which resolves to a directory, then `True` is returned.
 -}
isDir ∷ ∀ ε ρ μ . (MonadIO μ, AsFilePath ρ, AsIOError ε, MonadError ε μ) ⇒
         ρ → μ 𝔹
isDir = fmap (maybe 𝕱 (≡Directory) ∘ fmap ftype) ∘ stat

{- | Find if a path is a directory, by checking the filesystem.  If the path
     is a , then `False` is returned.
 -}
lisDir ∷ ∀ ε ρ μ . (MonadIO μ, AsFilePath ρ, AsIOError ε, MonadError ε μ) ⇒
         ρ → μ 𝔹
lisDir = fmap (maybe 𝕱 (≡Directory) ∘ fmap ftype) ∘ lstat

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
