{-# LANGUAGE UnicodeSyntax #-}

{- | Holder module for all tests in monadio-plus -}
module MonadIO.T.MonadIO
  ( tests )
where

import Base1T

-- tasty -------------------------------

import Test.Tasty  ( DependencyType( AllSucceed ), dependentTestGroup )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified MonadIO.File
import qualified MonadIO.FPath
import qualified MonadIO.FStat
import qualified MonadIO.OpenFile
import qualified MonadIO.Temp
import qualified MonadIO.T.Process

--------------------------------------------------------------------------------

tests ∷ TestTree
tests = dependentTestGroup "MonadIO" AllSucceed [ MonadIO.FStat.tests
                                                , MonadIO.FPath.tests
                                                , MonadIO.File.tests
                                                , MonadIO.OpenFile.tests
                                                , MonadIO.Temp.tests
                                                , MonadIO.T.Process.tests
                                                ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------

