{-# LANGUAGE UnicodeSyntax #-}

module MonadIO
  ( MonadIO, liftIO, say, warn )
where

-- base --------------------------------

import Control.Monad.IO.Class  ( MonadIO, liftIO )
import System.IO               ( Handle, stdout, stderr )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toText )

-- text --------------------------------

import Data.Text.IO  ( hPutStrLn )

--------------------------------------------------------------------------------

say_ ∷ (MonadIO μ, Printable τ) ⇒ Handle → τ → μ ()
say_ h = liftIO ∘ hPutStrLn h ∘ toText

say ∷ (MonadIO μ, Printable τ) ⇒ τ → μ ()
say = say_ stdout

warn ∷ (MonadIO μ, Printable τ) ⇒ τ → μ ()
warn = say_ stderr


-- that's all, folks! ----------------------------------------------------------
