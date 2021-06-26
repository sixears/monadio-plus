module MonadIO.Process.ToMaybeTexts
  ( ToMaybeTexts( toMaybeTexts ) )
where

-- base --------------------------------

import Data.Function  ( ($) )

-- more-unicode ------------------------

import Data.MoreUnicode.Maybe  ( 𝕄, pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.Text   ( 𝕋 )

-- text --------------------------------

import Data.Text  ( unlines )

--------------------------------------------------------------------------------

{- | Convert any of the numerous ways in which a process' output (stdout+stderr)
     may be captured & interpreted, to (𝕄 𝕋, 𝕄 𝕋). -}

class ToMaybeTexts ω where
  toMaybeTexts ∷ ω → (𝕄 𝕋, 𝕄 𝕋)

instance ToMaybeTexts () where
  toMaybeTexts _ = (𝕹, 𝕹)

instance ToMaybeTexts 𝕋 where
  toMaybeTexts t = (𝕵 t, 𝕹)

instance ToMaybeTexts [𝕋] where
  toMaybeTexts t = (𝕵 $ unlines t, 𝕹)

instance ToMaybeTexts (𝕋,𝕋) where
  toMaybeTexts (out,err) = (𝕵 out, 𝕵 err)

instance ToMaybeTexts (𝕋,()) where
  toMaybeTexts (out,()) = (𝕵 out, 𝕹)

instance ToMaybeTexts ((),()) where
  toMaybeTexts ((),()) = (𝕹, 𝕹)

instance ToMaybeTexts ([𝕋],[𝕋]) where
  toMaybeTexts (out,err) = (𝕵 $ unlines out, 𝕵 $ unlines err)

-- that's all, folks! ----------------------------------------------------------
