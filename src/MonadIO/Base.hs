{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module MonadIO.Base
  ( chmod, hClose, unlink )
where

-- base --------------------------------

import qualified System.IO

import Control.Monad.IO.Class  ( MonadIO )
import Data.Function           ( ($) )
import GHC.Stack               ( HasCallStack )
import System.IO               ( Handle )
import System.Posix.Types      ( FileMode )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- fpath -------------------------------

import FPath.AsFilePath  ( AsFilePath, filepath )
import FPath.File        ( FileAs( _File_ ) )

-- lens --------------------------------

import Control.Lens.Review  ( review )

-- monaderror-io -----------------------

import MonadError.IO        ( asIOError )
import MonadError.IO.Error  ( AsIOError )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⫥) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- unix --------------------------------

import System.Posix.Files  ( removeLink, setFileMode )

--------------------------------------------------------------------------------

hClose ∷ ∀ ε μ .
         (AsIOError ε, MonadError ε μ, HasCallStack, MonadIO μ) ⇒ Handle → μ ()
hClose = asIOError ∘ System.IO.hClose

----------------------------------------

chmod ∷ ∀ ε ρ μ .
        (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack, AsFilePath ρ) ⇒
        FileMode → ρ → μ ()
chmod perms fn = asIOError $ setFileMode (fn ⫥ filepath) perms

----------------------------------------

unlink ∷ ∀ ε γ μ .
         (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack, FileAs γ) ⇒
         γ → μ ()
unlink (review _File_ → fn) = asIOError $ removeLink (fn ⫥ filepath)

-- that's all, folks! ----------------------------------------------------------
