module MonadIO.Process.CreateProc
  ( CreateProc(..), cp_opts, cmd_spec, std_in, std_out, std_err )
where

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( ( ∘ ) )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- process -----------------------------

import System.Process  ( StdStream )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.Process.CreateProcOpts  ( CreateProcOpts
                                       , HasCreateProcOpts( createProcOpts ) )
import MonadIO.Process.CmdSpec         ( CmdSpec, HasCmdArgs( cmdArgs )
                                       , HasCmdExe( cmdExe )
                                       , HasCmdSpec( cmdSpec )
                                       )

--------------------------------------------------------------------------------

-- CreateProc ----------------------------------------------

data CreateProc = CreateProc { _cmd_spec ∷ CmdSpec
                             , _std_in   ∷ StdStream
                             , _std_out  ∷ StdStream
                             , _std_err  ∷ StdStream
                             , _cp_opts  ∷ CreateProcOpts
                             }


cmd_spec ∷ Lens' CreateProc CmdSpec
cmd_spec = lens _cmd_spec (\ cp cs → cp { _cmd_spec = cs })

std_in ∷ Lens' CreateProc StdStream
std_in = lens _std_in (\ cp st → cp { _std_in = st })

std_out ∷ Lens' CreateProc StdStream
std_out = lens _std_out (\ cp st → cp { _std_out = st })

std_err ∷ Lens' CreateProc StdStream
std_err = lens _std_err (\ cp st → cp { _std_err = st })

cp_opts ∷ Lens' CreateProc CreateProcOpts
cp_opts = lens _cp_opts (\ cp cpo → cp { _cp_opts = cpo })

instance HasCreateProcOpts CreateProc where
  createProcOpts = cp_opts

instance HasCmdSpec CreateProc where
  cmdSpec = cmd_spec

instance HasCmdArgs CreateProc where
  cmdArgs = cmd_spec ∘ cmdArgs

instance HasCmdExe CreateProc where
  cmdExe = cmd_spec ∘ cmdExe

