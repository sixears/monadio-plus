{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE UnicodeSyntax      #-}

{- | Default values for processes (which typically return some combination
     of exit values and Texts/ByteStreams). -}

module MonadIO.Process.OutputDefault
  ( OutputDefault( outDef ) )
where

-- more-unicode --------------------------------

import Data.MoreUnicode.Text  ( 𝕋 )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.Process.ExitStatus( ExitStatus( ExitVal ) )

--------------------------------------------------------------------------------

class OutputDefault ξ where
  outDef ∷ ξ

instance OutputDefault ExitStatus where
  outDef = ExitVal 0

instance OutputDefault () where
  outDef = ()

instance OutputDefault 𝕋 where
  outDef = ""

instance OutputDefault (𝕋,()) where
  outDef = ("",())

instance OutputDefault ((),𝕋) where
  outDef = ((),"")

instance OutputDefault ((),()) where
  outDef = ((),())

instance OutputDefault (𝕋,𝕋) where
  outDef = ("","")

instance OutputDefault ([𝕋]) where
  outDef = ([])

instance OutputDefault ([𝕋],[𝕋]) where
  outDef = ([],[])

instance OutputDefault ξ ⇒ OutputDefault (ExitStatus, ξ) where
  outDef = (outDef, outDef)

-- that's all, folks! ----------------------------------------------------------
