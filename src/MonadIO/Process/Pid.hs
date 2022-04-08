{- | Process Pid, with NFData implemented. -}
module MonadIO.Process.Pid
  ( Pid( Pid ), HasPid(..) )
where

import Base1T

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData( rnf ) )

-- lens --------------------------------

import Control.Lens.Tuple  ( _1 )

-- process -----------------------------

import qualified  System.Process

--------------------------------------------------------------------------------

newtype Pid = Pid System.Process.Pid
  deriving (Eq,Show)

instance NFData Pid where
  rnf = const $ ()

------------------------------------------------------------

class HasPid ev where
  pid ∷ Lens' ev Pid

instance HasPid Pid where
  pid = id

instance HasPid (Pid, α) where
  pid = _1

-- that's all, folks! ----------------------------------------------------------
