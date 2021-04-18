{-# LANGUAGE UnicodeSyntax #-}

-- base --------------------------------

import System.IO  ( IO )

-- tasty -------------------------------

import Test.Tasty           ( defaultIngredients )
import Test.Tasty.Runners   ( defaultMainWithIngredients )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.T.MonadIO  ( tests )

--------------------------------------------------------------------------------

main ∷ IO ()
main = defaultMainWithIngredients defaultIngredients tests

-- that's all, folks! ----------------------------------------------------------
