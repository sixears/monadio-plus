{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE UnicodeSyntax #-}

module MonadIO.Base
  ( chmod, hClose )
where

-- base --------------------------------

import qualified System.IO

import Control.Monad.IO.Class  ( MonadIO )
import Data.Function           ( ($) )
import System.IO               ( Handle )
import System.Posix.Types      ( FileMode )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- fpath -------------------------------

import FPath.AsFilePath  ( AsFilePath, filepath )

-- monaderror-io -----------------------

import MonadError.IO        ( asIOError )
import MonadError.IO.Error  ( AsIOError )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⫥) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- unix --------------------------------

import System.Posix.Files  ( setFileMode )

--------------------------------------------------------------------------------

hClose ∷ ∀ ε μ . (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒ Handle → μ ()
hClose = asIOError ∘ System.IO.hClose

----------------------------------------

chmod ∷ ∀ ε ρ μ . (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath ρ) ⇒
        FileMode → ρ → μ ()
chmod perms fn = asIOError $ setFileMode (fn ⫥ filepath) perms

-- that's all, folks! ----------------------------------------------------------
