module MonadIO.Error.ProcExitError
  ( AsProcExitError( _ProcExitError ), ProcExitError
  , asProcExitError, procExitError )
where

-- base --------------------------------

import Data.Eq        ( Eq( (==) ) )
import Data.Function  ( ($), id )
import GHC.Stack      ( CallStack, HasCallStack, callStack )
import Text.Show      ( Show )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- has-callstack -----------------------

import HasCallstack  ( HasCallstack( callstack ) )

-- lens --------------------------------

import Control.Lens.Lens    ( lens )
import Control.Lens.Prism   ( Prism' )
import Control.Lens.Review  ( (#) )

-- more-unicode ------------------------

import Data.MoreUnicode.Maybe  ( 𝕄, pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.Text   ( 𝕋 )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.Process.CmdSpec     ( CmdSpec )
import MonadIO.Process.ExitStatus  ( ExitStatus )

--------------------------------------------------------------------------------

{-| Process exited with a signal or unexpected exit value. -}
data ProcExitError = ProcExitError { _cmdspec   ∷ CmdSpec
                                   , _exStat    ∷ ExitStatus
                                   , _stdout    ∷ 𝕄 𝕋
                                   , _stderr    ∷ 𝕄 𝕋
                                   , _callstack ∷ CallStack
                                   }
  deriving Show

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

procExitError ∷ HasCallStack ⇒
                CmdSpec → ExitStatus → (𝕄 𝕋, 𝕄 𝕋) → ProcExitError
procExitError cspec exit (stdo,stde) =
  ProcExitError cspec exit stdo stde callStack

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
