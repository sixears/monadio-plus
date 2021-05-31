{- | POSIX process signals, with nice formatting -}

module MonadIO.Process.Signal
  ( Signal( Signal ) )
where

-- base --------------------------------

import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import Data.Word      ( Word8 )
import Prelude        ( fromIntegral )
import Text.Show      ( Show )

-- containers --------------------------

import qualified  Data.Map  as  Map

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- fmt ---------------------------------

import Text.Fmt  ( fmt )

-- text --------------------------------

import Data.Text  ( Text, append )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- unix --------------------------------

import qualified  System.Posix.Signals  as  Sig

-------------------------------------------------------------------------------

newtype Signal = Signal Word8
  deriving (Eq, Show)

sigMap ∷ Map.Map Sig.Signal Text
sigMap = Map.fromList [ ( Sig.nullSignal            , "NULL" ) -- "0"
                      , ( Sig.lostConnection        , "HUP"  ) --  1 on Linux
                      , ( Sig.keyboardSignal        , "INT"  ) --  2 on Linux
                      , ( Sig.keyboardTermination   , "QUIT" ) --  3 on Linux
                      , ( Sig.illegalInstruction    , "ILL"  ) --  4 on Linux
                      , ( Sig.breakpointTrap        , "TRAP" ) --  5 on Linux
                      , ( Sig.internalAbort         , "ABRT" ) --  6 on Linux
                      , ( Sig.busError              , "BUS"  ) --  7 on Linux
                      , ( Sig.floatingPointException, "FPE"  ) --  8 on Linux
                      , ( Sig.killProcess           , "KILL" ) --  9 on Linux
                      , ( Sig.userDefinedSignal1    , "USR1" ) -- 10 on Linux
                      , ( Sig.segmentationViolation , "SEGV" ) -- 11 on Linux
                      , ( Sig.userDefinedSignal2    , "USR2" ) -- 12 on Linux
                      , ( Sig.openEndedPipe         , "PIPE" ) -- 13 on Linux
                      , ( Sig.realTimeAlarm         , "ALRM" ) -- 14 on Linux
                      , ( Sig.softwareTermination   , "TERM" ) -- 15 on Linux
                      , ( Sig.processStatusChanged  , "CHLD" ) -- 17 on Linux
                      , ( Sig.continueProcess       , "CONT" ) -- 18 on Linux
                      , ( Sig.softwareStop          , "STOP" ) -- 19 on Linux
                      , ( Sig.keyboardStop          , "TSTP" ) -- 20 on Linux
                      , ( Sig.backgroundRead        , "TTIN" ) -- 21 on Linux
                      , ( Sig.backgroundWrite       , "TTOU" ) -- 22 on Linux
                      , ( Sig.urgentDataAvailable   , "URG"  ) -- 23 on Linux
                      , ( Sig.cpuTimeLimitExceeded  , "XCPU" ) -- 24 on Linux
                      , ( Sig.fileSizeLimitExceeded , "XFSZ" ) -- 25 on Linux
                      , ( Sig.virtualTimerExpired   , "VTALRM") --26 on Linux
                      , ( Sig.profilingTimerExpired , "PROF" ) -- 27 on Linux
                      -- sigPOLL A.K.A. sigIO
                      , ( Sig.pollableEvent         , "POLL" ) -- 29 on Linux
                      , ( Sig.badSystemCall         , "SYS"  ) -- 31 on Linux
                      ]



instance Printable Signal where
  print (Signal s) = let sigText s' =
                             case fromIntegral s' `Map.lookup` sigMap of
                                      Just t  → t
                                      Nothing → [fmt|#%d|] s
                        in P.text $ "sig" `append` sigText s

-- that's all, folks! ---------------------------------------------------------
