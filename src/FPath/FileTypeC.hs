{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE UnicodeSyntax #-}

module FPath.FileTypeC
  ( FileTypeC(..) )
where

-- base --------------------------------

import Data.Either  ( Either( Left, Right ) )

-- lens --------------------------------

import Control.Lens.Prism  ( prism )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.Abs      ( Abs )
import FPath.AbsDir   ( AbsDir, NonRootAbsDir )
import FPath.AbsFile  ( AbsFile )
import FPath.Dir      ( Dir )
import FPath.File     ( File( FileR ), FileAs( _File_ ) )
import FPath.FPath    ( FPath )
import FPath.Rel      ( Rel )
import FPath.RelDir   ( RelDir )
import FPath.RelFile  ( RelFile )

--------------------------------------------------------------------------------

class FileTypeC α where
  {- | the file "version" of a type; e.g., `FileType RelDir = RelFile` -}
  type FileType α

instance FileTypeC AbsDir where
  type FileType AbsDir = AbsFile

instance FileTypeC NonRootAbsDir where
  type FileType NonRootAbsDir = AbsFile

instance FileTypeC AbsFile where
  type FileType AbsFile = AbsFile

instance FileTypeC RelDir where
  type FileType RelDir = RelFile

instance FileTypeC RelFile where
  type FileType RelFile = RelFile

instance FileTypeC Dir where
  type FileType Dir = File

instance FileTypeC File where
  type FileType File = File

instance FileTypeC Abs where
  type FileType Abs = AbsFile

instance FileTypeC Rel where
  type FileType Rel = RelFile

instance FileTypeC FPath where
  type FileType FPath = File

instance FileAs RelFile where
  _File_ = prism FileR (\ case (FileR r) → Right r; f → Left f)

-- that's all, folks! ----------------------------------------------------------
