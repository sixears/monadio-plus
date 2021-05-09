{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax    #-}

module MonadIO.Tasty
  ( TestFileSpec(..), testInTempDir, testInTempDirFS )
where

-- base --------------------------------

import Control.Monad           ( (>=>), forM_ )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Foldable           ( Foldable )
import Data.Function           ( ($) )
import GHC.Stack               ( HasCallStack )
import System.IO               ( FilePath, IO )
import System.Posix.Types      ( FileMode )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadCatch )

-- fpath -------------------------------

import FPath.AbsDir            ( AbsDir, parseAbsDirP )
import FPath.AbsFile           ( AbsFile )
import FPath.AppendableFPath   ( (⫻) )
import FPath.AsFilePath        ( filepath )
import FPath.Error.FPathError  ( FPathError )
import FPath.Parent            ( parent )
import FPath.RelDir            ( RelDir )
import FPath.RelFile           ( RelFile )

-- monaderror-io -----------------------

import MonadError.IO        ( ӝ, asIOError, eitherIOThrow )
import MonadError.IO.Error  ( AsIOError, IOError )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens   ( (⊣), (⫥) )
import Data.MoreUnicode.Monad  ( (≪), (⪼) )
import Data.MoreUnicode.Text   ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- tasty -------------------------------

import Test.Tasty  ( TestTree )

-- tasty-plus --------------------------

import qualified  TastyPlus

-- unix --------------------------------

import qualified  System.Posix.Files  as  Files

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.Directory   ( mkdir, mkpath )
import MonadIO.OpenFile    ( writeExFileUTF8 )

--------------------------------------------------------------------------------

data TestFileSpec = TFSFile RelFile FileMode 𝕋
                  | TFSDir  RelDir  FileMode
                  -- symlinks can point to any old filepath, not just to valid
                  -- ones, hence we allow that here (mostly for testing
                  -- readlink); in practice, most uses should use a converted
                  -- FPath
                  | TFSSymL RelFile FilePath

------------------------------------------------------------

{- | Perform tests within a temporary directory, with a bespoke temp dir setup
     function.  The setup function is called with the name of the temp dir
     (as a filepath: FPath is not available here, as FPath uses TastyPlus); and
     the tempdir is also provided to the test (as an IO FilePath as a Tasty
     quirk).
 -}

testInTempDir ∷ (AbsDir → IO()) → (IO AbsDir → TestTree) → TestTree
testInTempDir setup test =
  let -- | Parse an `AbsDir`, throwing any errors into IO.
      parseAbsDirIO ∷ FilePath → IO AbsDir
      parseAbsDirIO = eitherIOThrow ∘ parseAbsDirP @FPathError
   in TastyPlus.testInTempDir (parseAbsDirIO >=> setup) (test ∘ (parseAbsDirIO ≪))

testInTempDirFS ∷ Foldable φ ⇒
                  φ TestFileSpec → (AbsDir → IO ()) → (IO AbsDir → TestTree)
                → TestTree
testInTempDirFS fs setup =
  let mkTFS ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack,
               MonadCatch μ) ⇒
              AbsDir → TestFileSpec → μ ()
      mkTFS p (TFSFile f m t) = do
        mkpath (f ⊣ parent) 0o700
        writeExFileUTF8 @AbsFile m (p ⫻ f) t
      mkTFS p (TFSDir  d m)   = mkdir @_ @AbsDir (p ⫻ d) m
      mkTFS p (TFSSymL f t)   =
        asIOError $ Files.createSymbolicLink t ((p ⫻ f ∷ AbsFile) ⫥ filepath)

      mkTFSes ∷ ∀ ε φ μ .
                (Foldable φ, MonadIO μ, AsIOError ε, MonadError ε μ,
                 MonadCatch μ) ⇒
                AbsDir -> φ TestFileSpec -> μ ()
      mkTFSes d fs' = forM_ fs' (mkTFS d)

   in testInTempDir (\ d → ӝ (mkTFSes @IOError d fs) ⪼ setup d)

-- that's all, folks! ----------------------------------------------------------
