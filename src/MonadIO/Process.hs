module MonadIO.Process
 ( system )
where

import Prelude  ( (-), fromIntegral )

-- base --------------------------------

import Data.Bool               ( otherwise )
import Data.Ord                ( (>) )
import Control.Monad           ( join, return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Function           ( ($), id )
import GHC.Stack               ( HasCallStack )
import System.Exit             ( ExitCode( ExitFailure, ExitSuccess ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError )

-- lens --------------------------------

import Control.Lens.Lens  ( lens )

-- monaderror-io -----------------------

import MonadError           ( ѥ )
import MonadError.IO.Error  ( AsIOError )

-- more-unicode ------------------------

import Data.MoreUnicode.Either  ( pattern 𝕷, pattern 𝕽 )
import Data.MoreUnicode.Lens    ( (⊣) )
import Data.MoreUnicode.Monad   ( (≫) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- process -----------------------------

import System.Process  ( ProcessHandle, waitForProcess )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.Error.CreateProcError   ( AsCreateProcError )
import MonadIO.Process.CmdSpec         ( CmdSpec, HasCmdSpec( cmdSpec ) )
import MonadIO.Process.CreateProcOpts  ( CreateProcOpts
                                       , HasCreateProcOpts( createProcOpts ) )
import MonadIO.Process.ExitStatus      ( ExitStatus( ExitSig, ExitVal ) )
import MonadIO.Process.MakeProc        ( MakeProc, makeProc )
import MonadIO.Process.MkStream        ( MkStream )
import MonadIO.Process.Signal          ( Signal( Signal ) )
import MonadIO.Process.OutputHandles   ( OutputHandles( slurp ) )

--------------------------------------------------------------------------------

data Cmd = Cmd CmdSpec CreateProcOpts

instance HasCmdSpec Cmd where
  cmdSpec = lens (\ (Cmd spec _) → spec) (\ (Cmd _ cpo) spec → Cmd spec cpo)

instance HasCreateProcOpts Cmd where
  createProcOpts = lens (\ (Cmd _ cpo) → cpo) (\ (Cmd spec _) cpo→ Cmd spec cpo)

------------------------------------------------------------

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
          MakeProc ζ, OutputHandles ζ ω, HasCreateProcOpts φ,
          AsCreateProcError ε, AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒
         σ → (φ,CmdSpec) → μ (ExitStatus, ω)

system inh (opts,cspec) = do
  x ← ѥ $ makeProc (opts ⊣ createProcOpts) inh cspec
  ѥ x ≫ \case
    𝕷 e → join ∘ return $ throwError e
    𝕽 r → procWait (return r)

-- $ system defCPOpts (""∷ Text) (CmdSpec (CmdExe [absfile|/usr/bin/env|]) (CmdArgs []))


-- that's all, folks! ----------------------------------------------------------
