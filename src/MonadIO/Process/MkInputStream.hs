{-# LANGUAGE UndecidableInstances #-} -- required for FileAs ⇒ MkInputStream

module MonadIO.Process.MkInputStream
  ( MkInputStream( mkIStream ) )
where

-- base --------------------------------

import Control.Monad           ( return )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Function           ( ($) )
import GHC.Stack               ( HasCallStack )
import System.IO               ( Handle )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- bytestring --------------------------

import Data.ByteString  ( ByteString )

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError )
import FPath.File              ( FileAs )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import MonadIO.Temp  ( tempfile )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⊣) )
import Data.MoreUnicode.Text     ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- process -----------------------------

import System.Process  ( StdStream( UseHandle ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.NamedHandle  ( HEncoding( NoEncoding ), ℍ, handle )
import MonadIO.OpenFile     ( FileOpenMode( FileR ), openFile )

--------------------------------------------------------------------------------

type 𝔹𝕊 = ByteString

class MkInputStream α where
  mkIStream ∷ ∀ ε μ .
              (MonadIO μ,
               AsIOError ε, AsFPathError ε, MonadError ε μ, HasCallStack) ⇒
              α → μ StdStream

instance MkInputStream StdStream where
  mkIStream s = return s

instance MkInputStream 𝕋 where
  mkIStream t = UseHandle ⊳ tempfile t

instance MkInputStream 𝔹𝕊 where
  mkIStream b = UseHandle ⊳ tempfile b

instance MkInputStream Handle where
  mkIStream h = return $ UseHandle h

instance MkInputStream ℍ where
  mkIStream h = return $ UseHandle (h ⊣ handle)

instance {-# OVERLAPPABLE #-} FileAs γ ⇒ MkInputStream γ where
  mkIStream fn = UseHandle ∘ view handle ⊳ openFile NoEncoding FileR fn

-- !!! If adding new instances here, consider adding them to !!!
-- !!! MockIO.Process.MLMakeIStream in mockio-plus too       !!!

-- that's all, folks! ----------------------------------------------------------
