{- | A class of things that collect many (including none) process output Handles,
     and a way to slurp them all in to provide the Text (as a blob, or as lines)
     or a binary string for each.
-}

module MonadIO.Process.OutputHandles
  ( OutputHandles( slurp ) )
where

-- base --------------------------------

import Control.Monad           ( return )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Function           ( ($) )
import System.IO               ( Handle )

-- bytestring --------------------------

import Data.ByteString  ( ByteString )

-- more-unicode ------------------------

import Data.MoreUnicode.Text  ( 𝕋 )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.Handle  ( HGetContents( hGetContents ) )

--------------------------------------------------------------------------------

type ℍ  = Handle
type 𝔹𝕊 = ByteString

class OutputHandles ζ ω | ω → ζ where
  slurp ∷ MonadIO μ ⇒ ζ → μ ω

instance OutputHandles () () where
  -- | no handles, no text slurped
  slurp () = return ()

-- We need to declare instances of ℍ → {𝕋,[𝕋],𝔹𝕊} explicitly, lest we see
-- errors of the form
--     src/MonadIO/Process/OutputHandles.hs:42:10: error:
--         Functional dependencies conflict between instance declarations:
--           instance forall α. HGetContents α => OutputHandles ℍ α
--             -- Defined at src/MonadIO/Process/OutputHandles.hs:42:10
--           instance forall α β.
--                    (HGetContents α, HGetContents β) =>
--                    OutputHandles (ℍ, ℍ) (α, β)
--             -- Defined at src/MonadIO/Process/OutputHandles.hs:48:10
--        |
--     42 | instance HGetContents α ⇒ OutputHandles ℍ α where

instance OutputHandles ℍ 𝕋 where
  -- | slurped output for stdout (but not stderr, which is untouched)
  slurp h0 = do
    t0 <- hGetContents h0
    return t0

instance OutputHandles ℍ [𝕋] where
  -- | slurped output for stdout (but not stderr, which is untouched)
  slurp h0 = do
    t0 <- hGetContents h0
    return t0

instance OutputHandles ℍ 𝔹𝕊 where
  -- | slurped output for stdout (but not stderr, which is untouched)
  slurp h0 = do
    t0 <- hGetContents h0
    return t0

instance (HGetContents α, HGetContents β) ⇒ OutputHandles (ℍ,ℍ) (α,β) where
  slurp (h0, h1) = do
    t0 <- hGetContents h0
    t1 <- hGetContents h1
    return $ (t0, t1)

-- that's all, folks! ----------------------------------------------------------
