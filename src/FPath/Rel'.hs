{-# LANGUAGE UnicodeSyntax #-}

module FPath.Rel'
  ( RelAs(..) )
where

-- base --------------------------------

import Data.Function  ( id )

-- lens --------------------------------

import Control.Lens  ( Prism' )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.Rel      ( Rel )
import FPath.RelDir   ( AsRelDir( _RelDir ), RelDir )
import FPath.RelFile  ( AsRelFile( _RelFile ), RelFile )

--------------------------------------------------------------------------------

class RelAs α where
  _Rel_ ∷ Prism' Rel α
instance RelAs Rel where
  _Rel_ = id
instance RelAs RelFile where
  _Rel_ = _RelFile
instance RelAs RelDir where
  _Rel_ = _RelDir

-- that's all, folks! ----------------------------------------------------------
