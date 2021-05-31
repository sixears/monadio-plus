module MonadIO.Process.MkStream
  ( MkStream( mkStream ) )
where

-- base --------------------------------

import Control.Monad           ( return )
import Control.Monad.IO.Class  ( MonadIO )
import GHC.Stack               ( HasCallStack )

-- bytestring --------------------------

import Data.ByteString  ( ByteString )

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import MonadIO.Temp  ( tempfile )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Text     ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- process -----------------------------

import System.Process  ( StdStream( UseHandle ) )

--------------------------------------------------------------------------------

type 𝔹𝕊 = ByteString

class MkStream α where
  mkStream ∷ ∀ ε μ .
              (MonadIO μ,
               AsIOError ε, AsFPathError ε, MonadError ε μ, HasCallStack) ⇒
              α → μ StdStream

instance MkStream StdStream where
  mkStream s = return s

instance MkStream 𝕋 where
  mkStream t = UseHandle ⊳ tempfile t

instance MkStream 𝔹𝕊 where
  mkStream b = UseHandle ⊳ tempfile b

-- that's all, folks! ----------------------------------------------------------
