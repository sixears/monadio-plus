module MonadIO.Process
 ( system, systemx )
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

-- containers-plus ---------------------

import ContainersPlus.Member  ( HasMember( (∉) ) )

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError )

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

import MonadIO.Error.CreateProcError  ( AsCreateProcError )
import MonadIO.Error.ProcExitError    ( AsProcExitError, asProcExitError )
import MonadIO.Process.CmdSpec        ( CmdSpec, expExit )
import MonadIO.Process.ExitStatus     ( ExitStatus( ExitSig, ExitVal ) )
import MonadIO.Process.MakeProc       ( MakeProc, makeProc )
import MonadIO.Process.MkInputStream  ( MkInputStream )
import MonadIO.Process.Signal         ( Signal( Signal ) )
import MonadIO.Process.OutputHandles  ( OutputHandles( slurp ) )
import MonadIO.Process.ToMaybeTexts   ( ToMaybeTexts( toMaybeTexts ) )

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

{- | Execute an external process, wait for termination, return exit status and
     whichever of stderr/stdout were implicitly requested by the return type. -}
systemx ∷ ∀ ε ζ ω σ μ .
         (MonadIO μ, HasCallStack, MkInputStream σ,
          MakeProc ζ, OutputHandles ζ ω, HasCallStack,
          AsCreateProcError ε, AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒
         σ       -- ^ stdin specification
       → CmdSpec -- ^ cmd + args
       → μ (ExitStatus, ω)

systemx inh cspec = do
  x ← ѥ $ makeProc inh cspec
  ѥ x ≫ \case
    𝕷 e → join ∘ return $ throwError e
    𝕽 r → procWait (return r)

-- $ system defCPOpts (""∷ Text) (CmdSpec (CmdExe [absfile|/usr/bin/env|])
--          (CmdArgs []))

-- :m + Data.Either Data.Text MonadIO.Error.CreateProcError FPath.AbsFile
--      MonadError MonadIO.Process.CmdSpec
-- splitMError @ProcError @(Either _) $
--             system ("" :: Text) (mkCmd [absfile|/usr/bin/env|]  [])

----------------------------------------

{- | Like `systemx`, but throws an `AsProcExitError` if the process exits with
     an unexpected value/signal (see `CmdSpec`), -}

system ∷ ∀ ε ζ ω σ μ .
         (MonadIO μ, MkInputStream σ, ToMaybeTexts ω,
          MakeProc ζ, OutputHandles ζ ω, HasCallStack,
          AsCreateProcError ε, AsFPathError ε, AsIOError ε, AsProcExitError ε,
          MonadError ε μ, HasCallStack) ⇒
         σ       -- ^ stdin specification
       → CmdSpec -- ^ cmd + args
       → μ (ExitStatus, ω)

system inh cspec = do
  (exit,w) ← systemx inh cspec

  if exit ∉ (cspec ⊣ expExit)
  then throwError $ asProcExitError cspec exit (toMaybeTexts w)
  else return (exit,w)


-- that's all, folks! ----------------------------------------------------------
