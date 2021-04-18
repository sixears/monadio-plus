{-# LANGUAGE UnicodeSyntax #-}

module MonadError.IO'
  ( asIOErrorT )
where

-- base --------------------------------

import Control.Monad           ( join )
import Control.Monad.IO.Class  ( MonadIO )
import System.IO               ( IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadError           ( splitMError )
import MonadError.IO        ( asIOError )
import MonadError.IO.Error  ( AsIOError )

--------------------------------------------------------------------------------

{- | Take some IO action, in a context which has other exceptions; catch any IO
     exceptions, and `join` them with the context exceptions. -}
asIOErrorT ∷ (MonadIO μ, AsIOError ε, MonadError ε μ) ⇒ ExceptT ε IO α → μ α
asIOErrorT = join ∘ asIOError ∘ splitMError

-- that's all, folks! ----------------------------------------------------------
