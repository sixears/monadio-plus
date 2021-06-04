module MonadIO.Process
 ( system )
where

import Prelude  ( (-), fromIntegral )

-- base --------------------------------

import Data.Bool               ( otherwise )
import Data.Ord                ( (>) )
import Control.Monad           ( join, return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Function           ( ($) )
import GHC.Stack               ( HasCallStack )
import System.Exit             ( ExitCode( ExitFailure, ExitSuccess ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError )

-- monaderror-io -----------------------

import MonadError           ( ѥ )
import MonadError.IO.Error  ( AsIOError )

-- more-unicode ------------------------

import Data.MoreUnicode.Either  ( pattern 𝕷, pattern 𝕽 )
import Data.MoreUnicode.Monad   ( (≫) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- process -----------------------------

import System.Process  ( ProcessHandle, waitForProcess )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.Error.CreateProcError   ( AsCreateProcError )
import MonadIO.Process.CmdSpec         ( CmdSpec )
import MonadIO.Process.ExitStatus      ( ExitStatus( ExitSig, ExitVal ) )
import MonadIO.Process.MakeProc        ( MakeProc, makeProc )
import MonadIO.Process.MkStream        ( MkStream )
import MonadIO.Process.Signal          ( Signal( Signal ) )
import MonadIO.Process.OutputHandles   ( OutputHandles( slurp ) )

--------------------------------------------------------------------------------

exitCode ∷ ExitCode → ExitStatus
exitCode ExitSuccess     = ExitVal 0
exitCode (ExitFailure i) | i > 0     = ExitVal $ fromIntegral i
                         | otherwise = ExitSig ∘ Signal ∘ fromIntegral $ 256-i

----------------------------------------

{- | Take an opened process and its output handles, wait for the process, slurp
     the handles, and return the exit val and any output (as Text). -}

procWait ∷ ∀ ζ ω μ . (MonadIO μ, OutputHandles ζ ω) ⇒
           μ (ProcessHandle, ζ) → μ (ExitStatus, ω)
procWait prox = do
  (handle, hs) ← prox
  ex ← liftIO $ waitForProcess handle
  texts ← slurp hs
  return (exitCode ex, texts)

----------------------------------------

system ∷ (MonadIO μ, HasCallStack, MkStream σ,
          MakeProc ζ, OutputHandles ζ ω,
          AsCreateProcError ε, AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒
         σ → CmdSpec → μ (ExitStatus, ω)

system inh cspec = do
  x ← ѥ $ makeProc inh cspec
  ѥ x ≫ \case
    𝕷 e → join ∘ return $ throwError e
    𝕽 r → procWait (return r)

-- $ system defCPOpts (""∷ Text) (CmdSpec (CmdExe [absfile|/usr/bin/env|]) (CmdArgs []))

-- splitMError @ProcError @(Either _) $ system ("" :: Text) (mkCmd [absfile|/usr/bin/env|]  [])

-- that's all, folks! ----------------------------------------------------------
