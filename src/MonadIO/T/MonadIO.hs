{-# LANGUAGE UnicodeSyntax #-}

{- | Holder module for all tests in monadio-plus -}
module MonadIO.T.MonadIO
  ( tests )
where

-- base --------------------------------

import Data.String    ( String )
import System.Exit    ( ExitCode )
import System.IO      ( IO )

-- more-unicode ------------------------

import Data.MoreUnicode.Natural  ( ℕ )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified MonadIO.File
import qualified MonadIO.FPath
import qualified MonadIO.FStat
import qualified MonadIO.OpenFile
import qualified MonadIO.T.Process

--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "MonadIO" [ MonadIO.FStat.tests, MonadIO.FPath.tests
                            , MonadIO.File.tests, MonadIO.OpenFile.tests
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

