{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE UnicodeSyntax      #-}

{- | Default values for processes (which typically return some combination
     of exit values and Texts/ByteStreams). -}

module MockIO.MockDefault
  ( MockDefault( mockDef ) )
where

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO.ExitVal( ExitVal( ExitVal ) )

--------------------------------------------------------------------------------

class MockDefault ξ where
  mockDef ∷ ξ

instance MockDefault ExitVal where
  mockDef = ExitVal 0

instance MockDefault () where
  mockDef = ()

instance MockDefault Text where
  mockDef = ""

instance MockDefault (Text,()) where
  mockDef = ("",())

instance MockDefault ((),Text) where
  mockDef = ((),"")

instance MockDefault ((),()) where
  mockDef = ((),())

instance MockDefault (Text,Text) where
  mockDef = ("","")

instance MockDefault ([Text]) where
  mockDef = ([])

instance MockDefault ([Text],[Text]) where
  mockDef = ([],[])

instance MockDefault ξ ⇒ MockDefault (ExitVal, ξ) where
  mockDef = (mockDef, mockDef)

-- that's all, folks! ----------------------------------------------------------
