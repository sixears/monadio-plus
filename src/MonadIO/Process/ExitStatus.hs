module MonadIO.Process.ExitStatus
  ( ExitStatus(..), HasExitStatus(..), HasStdErrT(..), HasStdOutT(..)
  , evOK, evAbnormal, evHelp, evExecFail
  , exitOkay, throwNotOkay
  )
where

import Base1T

-- base --------------------------------

import GHC.Generics   ( Generic )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- exited ------------------------------

import Exited  ( ToExitCode( toExitCode ) )

-- lens --------------------------------

import Control.Lens        ( view )
import Control.Lens.Tuple  ( _1 )

-- text --------------------------------

import Data.Text  ( Text )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.Process.Signal  ( Signal )

-------------------------------------------------------------------------------

{- | Process exit status;  -}

data ExitStatus = ExitVal Word8 | ExitSig Signal
  deriving (Eq,Generic,NFData,Show)

instance ToExitCode ExitStatus where
  toExitCode (ExitVal e) = case e of
                             0 → ExitSuccess
                             _ → ExitFailure $ fromIntegral e
  toExitCode (ExitSig _) = ExitFailure 255

exitOkay ∷ HasExitStatus ev ⇒ ev → 𝔹
exitOkay (view exitVal → ExitVal 0) = 𝕿
exitOkay _                          = 𝕱

evOK       ∷ ExitStatus
evOK       =  ExitVal 0

evAbnormal ∷ ExitStatus
evAbnormal = ExitVal 1

evHelp ∷ ExitStatus
evHelp = ExitVal 2

evExecFail ∷ ExitStatus
evExecFail = ExitVal 254

instance Printable ExitStatus where
  print (ExitVal ev) = P.text $ [fmt|Execution exit %d|] ev
  print (ExitSig es) = P.text $ [fmt|Execution exit on signal %T|] es

class HasExitStatus ev where
  exitVal ∷ Lens' ev ExitStatus

instance HasExitStatus ExitStatus where
  exitVal = id

instance HasExitStatus (ExitStatus, α) where
  exitVal = _1

{- | Given a datum α which HasExitStatus, throw an 'error' (created by f) iff
     the exit is not okay - that is, if it's non-zero. -}
throwNotOkay ∷ (HasExitStatus α, MonadError ε μ) ⇒ (α → ε) → α → μ ()
throwNotOkay f ev = when (not $ exitOkay ev) $ throwError (f ev)

class HasStdOutT s where
  stdoutT ∷ Lens' s Text

class HasStdErrT s where
  stderrT ∷ Lens' s Text

-- that's all, folks! ---------------------------------------------------------
