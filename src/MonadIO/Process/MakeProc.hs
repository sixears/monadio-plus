module MonadIO.Process.MakeProc
  ( CreateProc, MakeProc( makeProc )
  , cmd_spec, std_err, std_in, std_out )
where

import Prelude  ( error )

import Base1T

-- base --------------------------------

import System.IO  ( Handle )

-- env-plus ----------------------------

import Env.Types  ( strsEnv )

-- fpath -------------------------------

import FPath.AsFilePath        ( filepath )
import FPath.Error.FPathError  ( AsFPathError )

-- monaderror-io -----------------------

import MonadError  ( mapMError )

-- process -----------------------------

import qualified  System.Process  as  SysProc

import System.Process  ( CreateProcess( CreateProcess )
                       , ProcessHandle
                       , StdStream( CreatePipe, Inherit, NoStream )
                       , createProcess_
                       )

-- text --------------------------------

import Data.Text  ( unpack )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.Error.CreateProcError  ( AsCreateProcError( _CreateProcErr ) )
import MonadIO.Process.CmdSpec        ( CmdSpec, CreateGroup( CreateGroup )
                                      , HasCmdExe( cmdExe )
                                      , HasCmdSpec( cmdName, createGroup )
                                      , cmdArgsS, cwd, env
                                      )
import MonadIO.Process.CreateProc     ( CreateProc(..)
                                      , cmd_spec, std_err, std_in, std_out )
import MonadIO.Process.MkInputStream  ( MkInputStream( mkIStream ) )

--------------------------------------------------------------------------------

-- MakeProc ------------------------------------------------

{- | Create a process handle from a `CreateProc` specification. -}
createProc_ ∷ (MonadIO μ, AsCreateProcError ε, MonadError ε μ, HasCallStack) ⇒
              CreateProc
            → μ (𝕄 Handle, 𝕄 Handle, 𝕄 Handle, ProcessHandle)
createProc_ cp = do
  let exe = (cp ⊣ cmdExe ) ⫥ filepath
  p ← splitMError ∘ asIOError $
           createProcess_ (maybe exe unpack (cp ⊣ cmdName)) $
               CreateProcess { SysProc.cmdspec =
                                 SysProc.RawCommand exe (cp ⊣ cmdArgsS)
                             , SysProc.cwd     = (review filepath) ⊳ (cp ⊣ cwd)

                             , SysProc.env     = strsEnv <$> cp ⊣ env
                             , SysProc.std_in  = cp ⊣ std_in
                             , SysProc.std_out = cp ⊣ std_out
                             , SysProc.std_err = cp ⊣ std_err

                             , SysProc.create_group  =
                                   cp ⊣ createGroup == CreateGroup

                             , SysProc.close_fds     = 𝕿
                             , SysProc.delegate_ctlc = 𝕿
                             , SysProc.new_session   = 𝕱
                             , SysProc.child_group   = 𝕹
                             , SysProc.child_user    = 𝕹

                             , SysProc.detach_console     = 𝕱 -- windoze only
                             , SysProc.create_new_console = 𝕱 -- windoze only
                             , SysProc.use_process_jobs   = 𝕱 -- windoze only
                             }

  let p' = mapMError (_CreateProcErr #) p
  join (return p')

class MakeProc ω where
  {- | Create a process handle from a `CreateProc` specification. -}
  makeProc ∷ (MonadIO μ, MkInputStream σ,
              AsCreateProcError ε, AsFPathError ε, AsIOError ε, MonadError ε μ,
              HasCallStack) ⇒
             σ → CmdSpec → μ (ProcessHandle, ω)

instance MakeProc () where
  makeProc stdIn c = do
    inH ← mkIStream stdIn
    cp  ← createProc_ CreateProc { _cmd_spec = c
                                 , _std_in   = inH
                                 , _std_out  = Inherit
                                 , _std_err  = Inherit
                                 }
    case cp of
      (𝕹, 𝕹, 𝕹, h) → return (h, ())
      _                              → error "MakeProc: cannot happen (())"

instance MakeProc Handle where
  makeProc stdIn c = do
    inH ← mkIStream stdIn
    cp  ← createProc_ CreateProc { _cmd_spec = c
                                 , _std_in   = inH
                                 , _std_out  = CreatePipe
                                 , _std_err  = Inherit
                                 }
    case cp of
      (𝕹, 𝕵 outH, 𝕹, h) → return (h, outH)
      _                                → error "MakeProc: cannot happen (H)"

instance MakeProc (Handle,()) where
  makeProc stdIn c = do
    inH ← mkIStream stdIn
    cp  ← createProc_ CreateProc { _cmd_spec = c
                                 , _std_in   = inH
                                 , _std_out  = CreatePipe
                                 , _std_err  = NoStream
                                 }
    case cp of
      (𝕹, 𝕵 outH, 𝕹, h) → return (h, (outH,()))
      _                                → error "MakeProc: cannot happen (H,())"

instance MakeProc ((),Handle) where
  makeProc stdIn c = do
    inH ← mkIStream stdIn
    cp  ← createProc_ CreateProc { _cmd_spec = c
                                 , _std_in   = inH
                                 , _std_out  = NoStream
                                 , _std_err  = CreatePipe
                                 }
    case cp of
      (𝕹, 𝕹, 𝕵 errH, h) → return (h, ((),errH))
      _                                → error "MakeProc: cannot happen ((),H)"

instance MakeProc ((),()) where
  makeProc stdIn c = do
    inH ← mkIStream stdIn
    cp  ← createProc_ CreateProc { _cmd_spec = c
                                 , _std_in   = inH
                                 , _std_out  = NoStream
                                 , _std_err  = NoStream
                                 }
    -- I had originally had irrefutable patterns here, e.g.,
    --   ~(𝕹, 𝕹, 𝕵 errH, h) ← createProc_ ...
    -- but later GHC8 complained:
    --   Pattern match has inaccessible right hand side
    --   In a pattern binding…
    case cp of
      (𝕹, 𝕹, 𝕹, h) → return (h, ((),()))
      _                              → error "MakeProc: cannot happen ((),())"

instance MakeProc (Handle,Handle) where
  makeProc stdIn c = do
    inH ← mkIStream stdIn
    cp  ← createProc_ CreateProc { _cmd_spec = c
                                 , _std_in   = inH
                                 , _std_out  = CreatePipe
                                 , _std_err  = CreatePipe
                                 }
    case cp of
      (𝕹, 𝕵 outH, 𝕵 errH, h) → return (h, (outH,errH))
      _                                  → error "MakeProc: cannot happen (HH)"

-- that's all, folks! ----------------------------------------------------------
