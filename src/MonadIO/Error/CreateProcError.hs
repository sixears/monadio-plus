module MonadIO.Error.CreateProcError
  ( AsCreateProcError(..), CreateProcError(..), ProcError )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Eq            ( Eq )
import Data.Function      ( (.), ($), (&), id )
import Data.Maybe         ( Maybe( Just ) )
import Text.Show          ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError( _FPathError ), FPathIOError )

-- has-callstack -----------------------

import HasCallstack  ( HasCallstack( callstack ) )

-- lens --------------------------------

import Control.Lens.Lens    ( lens )
import Control.Lens.Prism   ( Prism', prism' )

-- monaderror-io -----------------------

import MonadError.IO.Error   ( AsIOError( _IOError ), IOError )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊣), (⊢) )
import Data.MoreUnicode.Maybe  ( pattern 𝕵, pattern 𝕹 )

--------------------------------------------------------------------------------

newtype CreateProcError = CreateProcError { unIOErr ∷ IOError }
  deriving Eq

instance Exception CreateProcError

instance Show CreateProcError where
  show (CreateProcError ioe) = show (show ioe)

instance AsIOError CreateProcError where
  _IOError = prism' CreateProcError (Just . unIOErr)

instance HasCallstack CreateProcError where
  callstack = lens (\ (CreateProcError ioe) → ioe ⊣ callstack)
                   (\ (CreateProcError ioe) cs →
                        CreateProcError $ ioe & callstack ⊢ cs)

------------------------------------------------------------

class AsCreateProcError ε where
  _CreateProcError :: Prism' ε CreateProcError
  _CreateProcErr   :: Prism' ε IOError
  _CreateProcErr   =  _CreateProcError . _IOError

instance AsCreateProcError CreateProcError where
  _CreateProcError = id

------------------------------------------------------------

data ProcError = PE_FPATH_IO_ERROR   FPathIOError
               | PE_CREATEPROC_ERROR CreateProcError
  deriving Show

_PE_FPATH_IO_ERROR ∷ Prism' ProcError FPathIOError
_PE_FPATH_IO_ERROR = prism' (\ e → PE_FPATH_IO_ERROR e)
                            (\ case PE_FPATH_IO_ERROR e → 𝕵 e; _ → 𝕹)

_PE_CREATEPROC_ERROR ∷ Prism' ProcError CreateProcError
_PE_CREATEPROC_ERROR = prism' (\ e → PE_CREATEPROC_ERROR e)
                            (\ case PE_CREATEPROC_ERROR e → 𝕵 e; _ → 𝕹)

instance Exception ProcError

instance HasCallstack ProcError where
  callstack = lens (\ case (PE_FPATH_IO_ERROR   fpioe) → fpioe ⊣ callstack
                           (PE_CREATEPROC_ERROR cpe)   → cpe   ⊣ callstack)
                   (\ pe cs →
                       case pe of
                         (PE_FPATH_IO_ERROR fpioe) →
                           PE_FPATH_IO_ERROR $ fpioe & callstack ⊢ cs
                         (PE_CREATEPROC_ERROR cpe) →
                           PE_CREATEPROC_ERROR $ cpe & callstack ⊢ cs
                   )

instance AsCreateProcError ProcError where
 _CreateProcError = _PE_CREATEPROC_ERROR

instance AsIOError ProcError where
  _IOError = _PE_FPATH_IO_ERROR ∘ _IOError

instance AsFPathError ProcError where
  _FPathError = _PE_FPATH_IO_ERROR ∘ _FPathError

-- that's all, folks! ----------------------------------------------------------
