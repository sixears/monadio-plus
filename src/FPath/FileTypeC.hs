{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE UnicodeSyntax #-}

module FPath.FileTypeC
  ( FileTypeC(..) )
where

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.Abs      ( Abs )
import FPath.AbsDir   ( AbsDir, NonRootAbsDir )
import FPath.AbsFile  ( AbsFile )
import FPath.Dir      ( Dir )
import FPath.File     ( File )
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

-- that's all, folks! ----------------------------------------------------------
