module MonadIO.Process.MakeProc
  ( CreateProc, MakeProc( makeProc )
  , cmd_spec, std_err, std_in, std_out )
where

import Prelude  ( error )

-- base --------------------------------

import Control.Monad           ( join, return )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Bool               ( Bool( False, True ) )
import Data.Eq                 ( (==) )
import Data.Function           ( ($) )
import Data.Functor            ( (<$>) )
import Data.Maybe              ( maybe )
import GHC.Stack               ( HasCallStack )
import System.IO               ( Handle )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- env-plus ----------------------------

import Env.Types  ( strsEnv )

-- fpath -------------------------------

import FPath.AsFilePath        ( filepath )
import FPath.Error.FPathError  ( AsFPathError )

-- lens --------------------------------

import Control.Lens.Review  ( (#), review )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- monaderror-io -----------------------

import MonadError           ( mapMError, splitMError )
import MonadError.IO        ( asIOError )
import MonadError.IO.Error  ( AsIOError )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Maybe    ( 𝕄, pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.Lens     ( (⊣), (⫥) )

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

                             , SysProc.close_fds     = True
                             , SysProc.delegate_ctlc = True
                             , SysProc.new_session   = False
                             , SysProc.child_group   = 𝕹
                             , SysProc.child_user    = 𝕹

                             , SysProc.detach_console     = False -- windoze only
                             , SysProc.create_new_console = False -- windoze only
                             , SysProc.use_process_jobs   = False -- windoze only
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
