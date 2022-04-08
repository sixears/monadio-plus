module MonadIO.Process
 ( doProc, procWait, system, systemx, systemN, systemS, throwSig, throwSig' )
where

import Base1T  hiding  ( (∉) )

-- containers-plus ---------------------

import ContainersPlus.Member  ( HasMember( (∉) ) )

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError )

-- lens --------------------------------

import Control.Lens.Tuple   ( _1 )

-- monaderror-io -----------------------

import MonadError     ( fromRight )
import MonadError.IO  ( ioThrow )

-- process -----------------------------

import System.Process  ( ProcessHandle, getPid, waitForProcess )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.Error.CreateProcError  ( AsCreateProcError )
import MonadIO.Error.ProcExitError    ( AsProcExitError, asProcExitError )
import MonadIO.NamedHandle            ( stdin )
import MonadIO.OpenFile               ( devnull )
import MonadIO.Process.CmdSpec        ( CmdSpec, expExit )
import MonadIO.Process.ExitInfo       ( ExitInfo, exitInfo )
import MonadIO.Process.ExitStatus     ( ExitStatus( ExitSig, ExitVal )
                                      , exitVal, exitWasSignalled )
import MonadIO.Process.MakeProc       ( MakeProc, makeProc )
import MonadIO.Process.MkInputStream  ( MkInputStream )
import MonadIO.Process.Signal         ( Signal( Signal ) )
import MonadIO.Process.OutputHandles  ( OutputHandles( slurp ) )
import MonadIO.Process.Pid            ( Pid( Pid ), pid )
import MonadIO.Process.ToMaybeTexts   ( ToMaybeTexts( toMaybeTexts ) )

--------------------------------------------------------------------------------

exitCode ∷ ExitCode → ExitStatus
exitCode ExitSuccess     = ExitVal 0
exitCode (ExitFailure i) | i > 0     = ExitVal $ fromIntegral i
                         | otherwise = ExitSig ∘ Signal ∘ fromIntegral $ 256-i

----------------------------------------

{- | Take an opened process and its output handles, wait for the process, slurp
     the handles, and return the exit val and any output (as Text). -}

procWait' ∷ ∀ ζ ω μ . (MonadIO μ, OutputHandles ζ ω) ⇒
           μ (ProcessHandle, ζ) → μ (ExitStatus, ω)
procWait' prox = do
  (handle, hs) ← prox
  ex ← liftIO $ waitForProcess handle
  texts ← slurp hs
  return (exitCode ex, texts)

----------------------------------------

{- | Take a process handle & stdin specification; wait for termination, return
     exit status and whichever of stderr/stdout were implicitly requested by the
     return type.
-}
procWait ∷ ∀ ζ ω μ . (MonadIO μ, OutputHandles ζ ω) ⇒
           CmdSpec → μ (ProcessHandle, ζ) → μ (ExitInfo, ω)
procWait cspec x =
  let go r = do
        pid_ ← getPid' (r ⊣ _1)
        procWait' (return r) ≫ \ (t,w) → return (exitInfo t cspec pid_, w)
   in join $ go ⊳ x

----------------------------------------

{- | Execute an external process, wait for termination, return exit status and
     whichever of stderr/stdout were implicitly requested by the return type. -}
systemx ∷ ∀ ε ζ ω σ μ .
         (MonadIO μ, HasCallStack, MkInputStream σ,
          MakeProc ζ, OutputHandles ζ ω, HasCallStack,
          AsCreateProcError ε, AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒
         σ       -- ^ stdin specification
       → CmdSpec -- ^ cmd + args
       → μ (ExitInfo, ω)

systemx inh cspec =
 ѥ (makeProc inh cspec) ≫ procWait cspec

{- | Get pid where we're sure the pid should be available; throws errors into
     IO.
 -}
getPid' ∷ MonadIO μ ⇒ ProcessHandle → μ Pid
getPid' h =
  liftIO $ getPid h ≫ \ case
    𝕹   → ioThrow ("failed to getPid from handle; already closed" ∷ 𝕋)
    𝕵 p → return $ Pid p

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
       → μ (ExitInfo, ω)

system inh cspec = do
  (einfo,w) ← systemx inh cspec

  if (einfo ⊣ exitVal) ∉ (cspec ⊣ expExit)
  then let x = einfo ⊣ exitVal
        in throwError $ asProcExitError cspec (einfo ⊣ pid) x (toMaybeTexts w)
  else return (einfo,w)

--------------------

{- | Like `system`, but implicitly takes `/dev/null` for input. -}
systemN ∷ ∀ ε ζ ω μ .
          (MonadIO μ, ToMaybeTexts ω,
           MakeProc ζ, OutputHandles ζ ω, HasCallStack,
           AsCreateProcError ε, AsFPathError ε, AsIOError ε, AsProcExitError ε,
           MonadError ε μ, HasCallStack) ⇒
          CmdSpec → μ (ExitInfo, ω)
systemN c = devnull ≫ \ l → system l c

--------------------

{- | Like `system`, but implicitly takes `stdin` for input. -}
systemS ∷ ∀ ε ζ ω μ .
          (MonadIO μ, ToMaybeTexts ω,
           MakeProc ζ, OutputHandles ζ ω, HasCallStack,
           AsCreateProcError ε, AsFPathError ε, AsIOError ε, AsProcExitError ε,
           MonadError ε μ, HasCallStack) ⇒
          CmdSpec → μ (ExitInfo, ω)
systemS c = system stdin c

----------------------------------------

{- | Given an exit status (and possibly, stdout, stderr, etc); throw iff the
     exit status is of a signal received. -}

throwSig ∷ ∀ ε β η . (AsProcExitError ε, MonadError ε η) ⇒
           CmdSpec → Pid → (ExitInfo, β) → η (ExitInfo, β)

{-
throwSig cspec pd (view exitVal → ex@(ExitSig _),_)=
  throwError $ asProcExitError cspec pd ex (𝕹,𝕹)
throwSig _     _   (exstat@(view exitVal → ExitVal _), w) = return (exstat,w)
-}

throwSig cspec pd (exstat, w) =
  let exVal = exstat ⊣ exitVal
   in if exitWasSignalled exVal
      then throwError $ asProcExitError cspec pd exVal (𝕹,𝕹)
      else return (exstat,w)

----------

throwSig' ∷ ∀ ε β η . (AsProcExitError ε, MonadError ε η) ⇒
            CmdSpec → Pid → 𝔼 ε (ExitInfo, β) → η (ExitInfo, β)
throwSig' cspec pd = fromRight ∘ join ∘ fmap (throwSig cspec pd)

----------------------------------------

{- | Spawn a process; return the exit value, throw on signal.  The `finally`
     argument is always executed immediately after the process returns (whatever
     the exit value).
 -}

doProc ∷ ∀ ε ζ ω σ μ .
         (MonadIO μ, MkInputStream σ,
          ToMaybeTexts ω, MakeProc ζ, OutputHandles ζ ω,
          AsProcExitError ε, AsCreateProcError ε, AsFPathError ε,
          AsIOError ε, Printable ε, MonadError ε μ) ⇒
         μ () → σ → CmdSpec → μ (ExitInfo, ω)
doProc finally input cspec = do
  result ← systemx input cspec
  finally
  throwSig' cspec (result ⊣ _1 ∘ pid) (𝕽 result)

-- that's all, folks! ----------------------------------------------------------
