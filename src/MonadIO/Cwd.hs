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

module MonadIO.Cwd
  ( getCwd, getCwdY, getCwd', getCwd'Y

  , tests
  )
where

import Base1T  hiding  ( last )

-- base --------------------------------

import Data.List  ( isSuffixOf, length, take )

-- exceptions --------------------------

import Control.Monad.Catch  ( bracket )

-- fpath -------------------------------

import FPath.AbsDir            ( AbsDir, __parseAbsDirP__ )
import FPath.AbsFile           ( AbsFile )
import FPath.Error.FPathError  ( AsFPathError, FPathIOError )
import FPath.Parseable         ( parse )

-- temporary ---------------------------

import System.IO.Temp ( withSystemTempDirectory )

-- text --------------------------------

import Data.Text  ( last )

-- unix --------------------------------

import System.Posix.Directory  ( changeWorkingDirectory, getWorkingDirectory )
import System.Posix.Files      ( readSymbolicLink )
import System.Posix.Process    ( getProcessID )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.FStat  ( isDir )

--------------------------------------------------------------------------------

addSlash ∷ 𝕋 → 𝕋
addSlash ""             = ""
addSlash t@(last → '/') = t
addSlash t              = t ⊕ "/"

----------------------------------------

{- | Find pwd using /proc/<PID>/cwd.  We do this because
     `System.Posix.Directory.getWorkingDirectory` and
     `System.Directory.getCurrentDirectory` throw errors are not caught even
     with `catch`/`catchIO`.
 -}
getCwd ∷ ∀ ε μ . (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒
         μ AbsDir
getCwd = asIOError getWorkingDirectory ≫ parse ∘ addSlash ∘ toText

----------------------------------------

{- | Find pwd using /proc/<PID>/cwd.  This version of `getCwd` will try to
     return the name of the dir we changed to, even if that dir has since been
     deleted.
 -}
getCwd' ∷ ∀ ε μ . (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ)⇒
          μ AbsDir
getCwd' = do
  pid      ← liftIO getProcessID
  proc_cwd ← parse @AbsFile $ [fmtT|/proc/%d/cwd|] pid
  fpdir    ← liftIO ∘ readSymbolicLink $ toString proc_cwd
  is_dir   ← isDir fpdir
  let fpdir' = if not is_dir ∧ (" (deleted)" `isSuffixOf` fpdir)
               then take (length fpdir - 10) fpdir
               else fpdir
  parse @AbsDir ∘ addSlash $ toText fpdir'

----------------------------------------

{- | Get the pwd; in case of error, return `Nothing`. -}
getCwdY ∷ ∀ μ . MonadIO μ ⇒ μ (𝕄 AbsDir)
getCwdY = either (const 𝕹) 𝕵 ⊳ (ѥ @FPathIOError $ getCwd)

----------------------------------------

{- | Get the pwd; in case of error, return `Nothing`; uses `getCwd'`. -}
getCwd'Y ∷ ∀ μ . MonadIO μ ⇒ μ (𝕄 AbsDir)
getCwd'Y = either (const 𝕹) 𝕵 ⊳ (ѥ @FPathIOError $ getCwd')

--------------------

getCwdTests ∷ TestTree
getCwdTests =
  let getCwd_ ∷ IO (𝔼 FPathIOError AbsDir)
      getCwd_ = ѥ getCwd

      inTmp = inSystemTempDirectory "MonadIO.Cwd.getCwdTests"
   in testGroup "getCwd" [
        testCase "getCwd" $ inTmp $ \ d → getCwd_ ≫ \ cwd → 𝕽 d @=? cwd
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

tests ∷ TestTree
tests = testGroup "MonadIO.FPath" [ getCwdTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
