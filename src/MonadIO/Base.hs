{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE UnicodeSyntax #-}

module MonadIO.Base
  ( hClose )
where

-- base --------------------------------

import qualified System.IO

import Control.Monad.IO.Class  ( MonadIO )
import System.IO               ( Handle )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- monaderror-io -----------------------

import MonadError.IO        ( asIOError )
import MonadError.IO.Error  ( AsIOError )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

--------------------------------------------------------------------------------

hClose ∷ ∀ ε μ . (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒ Handle → μ ()
hClose = asIOError ∘ System.IO.hClose

-- that's all, folks! ----------------------------------------------------------
