{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module MonadIO.Directory
  ( inDir )
where

-- base --------------------------------

import Control.Monad           ( join )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Function           ( ($) )
import System.IO               ( IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- directory ---------------------------

import System.Directory  ( withCurrentDirectory )

-- fpath -------------------------------

import FPath.AsFilePath  ( filepath )
import FPath.Dir         ( DirAs( _Dir_ ) )

-- lens --------------------------------

import Control.Lens.Review  ( review )

-- monaderror-io -----------------------

import MonadError           ( ѥ )
import MonadError.IO        ( asIOError )
import MonadError.IO.Error  ( AsIOError )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )

--------------------------------------------------------------------------------

{- | Perform IO with the dir *temporarily* changed to a given directory. -}
inDir ∷ (MonadIO μ, DirAs δ, AsIOError ε, MonadError ε μ) ⇒
         δ → ExceptT ε IO α → μ α
inDir (review $ filepath ∘ _Dir_ → d) io =
  join ∘ asIOError $ withCurrentDirectory d (ѥ io)

-- that's all, folks! ----------------------------------------------------------
