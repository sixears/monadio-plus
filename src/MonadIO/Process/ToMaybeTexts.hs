module MonadIO.Process.ToMaybeTexts
  ( ToMaybeTexts( toMaybeTexts ) )
where

import Base1T

-- bytestring --------------------------

import qualified  Data.ByteString

-- text --------------------------------

import Data.Text           ( unlines )
import Data.Text.Encoding  ( decodeUtf8 )

--------------------------------------------------------------------------------

type 𝔹𝕊 = Data.ByteString.ByteString

------------------------------------------------------------

class ToMaybeText τ where
  toMaybeText ∷ τ → 𝕄 𝕋

instance ToMaybeText () where
  toMaybeText = const 𝕹

instance ToMaybeText 𝕋 where
  toMaybeText = 𝕵

instance ToMaybeText [𝕋] where
  toMaybeText = 𝕵 ∘ unlines

instance ToMaybeText 𝔹𝕊 where
  toMaybeText = 𝕵 ∘ decodeUtf8

------------------------------------------------------------

{- | Convert any of the numerous ways in which a process' output (stdout+stderr)
     may be captured & interpreted, to (𝕄 𝕋, 𝕄 𝕋). -}

class ToMaybeTexts ω where
  toMaybeTexts ∷ ω → (𝕄 𝕋, 𝕄 𝕋)

----------

instance ToMaybeTexts () where
  toMaybeTexts _ = (𝕹, 𝕹)

----------

instance ToMaybeTexts 𝕋 where
  toMaybeTexts t = (𝕵 t, 𝕹)

instance ToMaybeTexts [𝕋] where
  toMaybeTexts t = (𝕵 $ unlines t, 𝕹)

instance ToMaybeTexts 𝔹𝕊 where
  toMaybeTexts t = (𝕵 $ decodeUtf8 t, 𝕹)

----------

instance (ToMaybeText τ, ToMaybeText τ') ⇒ ToMaybeTexts (τ,τ') where
  toMaybeTexts (t,t') = (toMaybeText t, toMaybeText t')

-- that's all, folks! ----------------------------------------------------------
