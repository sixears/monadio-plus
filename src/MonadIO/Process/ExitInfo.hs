{- | Exit status, original cmdspec, pid for an exited process. -}
module MonadIO.Process.ExitInfo
  ( ExitInfo, exitInfo )
where

import Base1T

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.Process.CmdSpec     ( CmdSpec, HasCmdArgs( cmdArgs )
                                   , HasCmdExe( cmdExe ), HasCmdSpec( cmdSpec ))
import MonadIO.Process.ExitStatus  ( ExitStatus, HasExitStatus( exitVal ) )
import MonadIO.Process.Pid         ( HasPid( pid ), Pid )

--------------------------------------------------------------------------------

data ExitInfo = ExitInfo { _exitStatus ∷ ExitStatus
                         , _cmdspec    ∷ CmdSpec
                         , _pid        ∷ Pid
                         }
  deriving Show

instance HasCmdArgs ExitInfo where
  cmdArgs = cmdSpec ∘ cmdArgs

instance HasCmdExe ExitInfo where
  cmdExe = cmdSpec ∘ cmdExe

instance HasCmdSpec ExitInfo where
  cmdSpec = lens _cmdspec (\ i c → i { _cmdspec = c })

instance HasExitStatus ExitInfo where
  exitVal = lens _exitStatus (\ i s → i { _exitStatus = s })

instance HasPid ExitInfo where
  pid = lens _pid (\ i p → i { _pid = p })

exitInfo ∷ ExitStatus → CmdSpec → Pid → ExitInfo
exitInfo x c p = ExitInfo x c p

-- that's all, folks! ----------------------------------------------------------
