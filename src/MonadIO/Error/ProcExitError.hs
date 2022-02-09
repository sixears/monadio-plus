module MonadIO.Error.ProcExitError
  ( AsProcExitError( _ProcExitError ), ProcExitError
  , asProcExitError, procExitError, stdErr, stdOut )
where

import Base1T

-- base --------------------------------

import GHC.Generics  ( Generic )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.Process.CmdSpec     ( CmdSpec, HasCmdArgs( cmdArgs )
                                   , HasCmdExe( cmdExe )
                                   , HasCmdSpec( cmdSpec )
                                   )
import MonadIO.Process.ExitStatus  ( ExitStatus, HasExitStatus( exitVal ) )

--------------------------------------------------------------------------------

{-| Process exited with a signal or unexpected exit value. -}
data ProcExitError = ProcExitError { _cmdspec   ∷ CmdSpec
                                   , _exStat    ∷ ExitStatus
                                   , _stdout    ∷ 𝕄 𝕋
                                   , _stderr    ∷ 𝕄 𝕋
                                   , _callstack ∷ CallStack
                                   }
  deriving (Generic,NFData,Show)

instance Eq ProcExitError where
  ProcExitError cp1 es1 so1 se1 _ == ProcExitError cp2 es2 so2 se2 _ =
    (cp1,es1,so1,se1) == (cp2,es2,so2,se2)

instance HasCallstack ProcExitError where
  callstack = lens _callstack
                   (\ (ProcExitError cp es so se _) cs →
                      ProcExitError cp es so se cs)

instance Printable ProcExitError where
  print (ProcExitError cs es (𝕵 so) (𝕵 se) _) =
    P.text $ [fmt|PROCESS FAILED: CMD> %T\nEXIT: %T\nSTDOUT: %T\nSTDERR: %T|]
             cs es so se
  print (ProcExitError cs es (𝕵 so) 𝕹 _) =
    P.text $ [fmt|PROCESS FAILED: CMD> %T\nEXIT: %T\nSTDOUT: %T|]
             cs es so
  print (ProcExitError cs es 𝕹 (𝕵 se) _) =
    P.text $ [fmt|PROCESS FAILED: CMD> %T\nEXIT: %T\nSTDERR: %T|]
             cs es se
  print (ProcExitError cs es 𝕹 𝕹 _) =
    P.text $ [fmt|PROCESS FAILED: CMD> %T\nEXIT: %T|]
             cs es

--------------------

instance HasExitStatus ProcExitError where
  exitVal = lens _exStat (\ pe es → pe { _exStat = es })

--------------------

instance HasCmdArgs ProcExitError where
  cmdArgs = cmdSpec ∘ cmdArgs

--------------------

instance HasCmdExe ProcExitError where
  cmdExe = cmdSpec ∘ cmdExe

--------------------

instance HasCmdSpec ProcExitError where
  cmdSpec = lens _cmdspec (\ pe cs → pe { _cmdspec = cs })

----------------------------------------

procExitError ∷ HasCallStack ⇒
                CmdSpec → ExitStatus → (𝕄 𝕋, 𝕄 𝕋) → ProcExitError
procExitError cspec exit (stdo,stde) =
  ProcExitError cspec exit stdo stde callStack

----------------------------------------

stdErr ∷ Lens' ProcExitError (𝕄 𝕋)
stdErr = lens _stderr (\ pe t → pe { _stderr = t })

----------------------------------------

stdOut ∷ Lens' ProcExitError (𝕄 𝕋)
stdOut = lens _stdout (\ pe t → pe { _stdout = t })

------------------------------------------------------------

class AsProcExitError χ where
  _ProcExitError ∷ Prism' χ ProcExitError

instance AsProcExitError ProcExitError where
  _ProcExitError = id

asProcExitError ∷ (AsProcExitError ε, HasCallStack) ⇒
                  CmdSpec → ExitStatus → (𝕄 𝕋, 𝕄 𝕋) → ε
asProcExitError cspec exit stdoe =
  _ProcExitError # procExitError cspec exit stdoe

-- that's all, folks! ----------------------------------------------------------
