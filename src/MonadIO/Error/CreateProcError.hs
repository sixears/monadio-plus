module MonadIO.Error.CreateProcError
  ( AsCreateProcError(..), CreateProcError(..), ProcError, ProcErrorX )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Eq            ( Eq )
import Data.Function      ( (.), ($), (&), id )
import Data.Maybe         ( Maybe( Just ) )
import GHC.Generics       ( Generic )
import Text.Show          ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

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

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.Error.ProcExitError  ( AsProcExitError( _ProcExitError )
                                    , ProcExitError )

--------------------------------------------------------------------------------

newtype CreateProcError = CreateProcError { unIOErr ∷ IOError }
  deriving (Eq,Generic,NFData)

instance Exception CreateProcError

instance Show CreateProcError where
  show (CreateProcError ioe) = show (show ioe)

instance AsIOError CreateProcError where
  _IOError = prism' CreateProcError (Just . unIOErr)

instance HasCallstack CreateProcError where
  callstack = lens (\ (CreateProcError ioe) → ioe ⊣ callstack)
                   (\ (CreateProcError ioe) cs →
                        CreateProcError $ ioe & callstack ⊢ cs)

instance Printable CreateProcError where
  print = print ∘ unIOErr

------------------------------------------------------------

class AsCreateProcError ε where
  _CreateProcError ∷ Prism' ε CreateProcError
  _CreateProcErr   ∷ Prism' ε IOError
  _CreateProcErr   =  _CreateProcError . _IOError

instance AsCreateProcError CreateProcError where
  _CreateProcError = id

------------------------------------------------------------

{- External process error before kick-off, due to create error or fpath error
   (with the arguments). -}
data ProcErrorX = PEX_FPATH_IO_ERROR   FPathIOError
                | PEX_CREATEPROC_ERROR CreateProcError
  deriving (Eq,Generic,NFData,Show)

_PEX_FPATH_IO_ERROR ∷ Prism' ProcErrorX FPathIOError
_PEX_FPATH_IO_ERROR = prism' (\ e → PEX_FPATH_IO_ERROR e)
                            (\ case PEX_FPATH_IO_ERROR e → 𝕵 e; _ → 𝕹)

_PEX_CREATEPROC_ERROR ∷ Prism' ProcErrorX CreateProcError
_PEX_CREATEPROC_ERROR = prism' (\ e → PEX_CREATEPROC_ERROR e)
                            (\ case PEX_CREATEPROC_ERROR e → 𝕵 e; _ → 𝕹)

instance Exception ProcErrorX

instance HasCallstack ProcErrorX where
  callstack = lens (\ case (PEX_FPATH_IO_ERROR   fpioe) → fpioe ⊣ callstack
                           (PEX_CREATEPROC_ERROR cpe)   → cpe   ⊣ callstack)
                   (\ pe cs →
                       case pe of
                         (PEX_FPATH_IO_ERROR fpioe) →
                           PEX_FPATH_IO_ERROR $ fpioe & callstack ⊢ cs
                         (PEX_CREATEPROC_ERROR cpe) →
                           PEX_CREATEPROC_ERROR $ cpe & callstack ⊢ cs
                   )

instance AsCreateProcError ProcErrorX where
 _CreateProcError = _PEX_CREATEPROC_ERROR

instance AsIOError ProcErrorX where
  _IOError = _PEX_FPATH_IO_ERROR ∘ _IOError

instance AsFPathError ProcErrorX where
  _FPathError = _PEX_FPATH_IO_ERROR ∘ _FPathError

instance Printable ProcErrorX where
  print (PEX_FPATH_IO_ERROR fpioe) = print fpioe
  print (PEX_CREATEPROC_ERROR cpe) = print cpe

------------------------------------------------------------

{- | Process error at kickoff or unexpected exit. -}
data ProcError = PE_PEX_ERROR        ProcErrorX
               | PE_PROC_EXIT_ERROR  ProcExitError
  deriving (Eq,Generic,NFData,Show)

_PE_PEX_ERROR ∷ Prism' ProcError ProcErrorX
_PE_PEX_ERROR = prism' (\ e → PE_PEX_ERROR e)
                            (\ case PE_PEX_ERROR e → 𝕵 e; _ → 𝕹)

_PE_PROC_EXIT_ERROR ∷ Prism' ProcError ProcExitError
_PE_PROC_EXIT_ERROR = prism' (\ e → PE_PROC_EXIT_ERROR e)
                             (\ case PE_PROC_EXIT_ERROR e → 𝕵 e; _ → 𝕹)

instance Exception ProcError

instance HasCallstack ProcError where
  callstack = lens (\ case (PE_PEX_ERROR   fpioe) → fpioe ⊣ callstack
                           (PE_PROC_EXIT_ERROR pee)   → pee   ⊣ callstack)
                   (\ pe cs →
                       case pe of
                         (PE_PEX_ERROR fpioe) →
                           PE_PEX_ERROR $ fpioe & callstack ⊢ cs
                         (PE_PROC_EXIT_ERROR pee) →
                           PE_PROC_EXIT_ERROR $ pee & callstack ⊢ cs
                   )

instance AsCreateProcError ProcError where
 _CreateProcError = _PE_PEX_ERROR ∘ _CreateProcError

instance AsIOError ProcError where
  _IOError = _PE_PEX_ERROR ∘ _IOError

instance AsFPathError ProcError where
  _FPathError = _PE_PEX_ERROR ∘ _FPathError

instance AsProcExitError ProcError where
  _ProcExitError = _PE_PROC_EXIT_ERROR

instance Printable ProcError where
  print (PE_PEX_ERROR       pexe) = print pexe
  print (PE_PROC_EXIT_ERROR pee)  = print pee

-- that's all, folks! ----------------------------------------------------------
