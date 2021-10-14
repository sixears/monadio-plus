module MonadIO.Flock
  ( HasNamedFileLock( flName, fileLock, shex ), NamedFileLock
  , flock, flockNB, unflock )
where

-- base --------------------------------

import Data.Function  ( ($) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toText )

-- filelock ----------------------------

import System.FileLock  ( FileLock, SharedExclusive
                        , lockFile, tryLockFile, unlockFile )

-- fpath -------------------------------

import FPath.AsFilePath  ( AsFilePath( filepath ) )
import FPath.File        ( FileAs )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳), (⊳⊳) )
import Data.MoreUnicode.Lens     ( (⊣), (⫥))
import Data.MoreUnicode.Maybe    ( 𝕄 )
import Data.MoreUnicode.Text     ( 𝕋 )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO  ( MonadIO, liftIO )

--------------------------------------------------------------------------------

data NamedFileLock = NamedFileLock { _flName   ∷ 𝕋
                                   , _shex     ∷ SharedExclusive
                                   , _fileLock ∷ FileLock
                                   }

class HasNamedFileLock α where
  flName   ∷ Lens' α 𝕋
  fileLock ∷ Lens' α FileLock
  shex     ∷ Lens' α SharedExclusive

instance HasNamedFileLock NamedFileLock where
  flName   = lens _flName   (\ nfl fn → nfl { _flName   = fn })
  fileLock = lens _fileLock (\ nfl fl → nfl { _fileLock = fl })
  shex     = lens _shex     (\ nfl sx → nfl { _shex     = sx })

-----------------------------------------------------------

flock ∷ (MonadIO μ, FileAs γ, AsFilePath γ, Printable γ) ⇒
        SharedExclusive → γ → μ NamedFileLock
flock mode fn = liftIO $
  NamedFileLock (toText fn) mode ⊳ lockFile (fn ⫥ filepath) mode

flockNB ∷ (MonadIO μ, FileAs γ, AsFilePath γ, Printable γ) ⇒
          SharedExclusive → γ → μ (𝕄 NamedFileLock)
flockNB mode fn = liftIO $
  NamedFileLock (toText fn) mode ⊳⊳ tryLockFile (fn ⫥ filepath) mode

unflock ∷ MonadIO μ ⇒ NamedFileLock → μ ()
unflock l = liftIO $ unlockFile (l ⊣ fileLock)


-- that's all, folks! ----------------------------------------------------------
