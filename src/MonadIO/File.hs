{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE UnicodeSyntax    #-}
{-# LANGUAGE ViewPatterns     #-}

module MonadIO.File
  ( getContentsUTF8, hClose, hGetContentsUTF8, readFileBinary, readFileUTF8
  , readFileUTF8Lenient, stat, writeFileUTF8, writeFileBinary
  )
where

-- base --------------------------------

import qualified  System.IO

import Control.Monad           ( join )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Function           ( ($) )
import Data.Functor            ( fmap )
import Data.Maybe              ( Maybe )
import System.IO               ( Handle, IOMode( ReadMode, WriteMode )
                               , hSetEncoding, stdin, utf8, withFile )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- bytestring --------------------------

import qualified Data.ByteString  as  BS

import Data.ByteString  ( ByteString )

-- fpath -------------------------------

import FPath.AsFilePath  ( filepath )
import FPath.File        ( File )
import FPath.FPath       ( FPath )

-- lens --------------------------------

import Control.Lens.Review  ( review )

-- monadio-error -----------------------

import MonadError           ( splitMError )
import MonadError.IO        ( asIOError )
import MonadError.IO.Error  ( AsIOError, squashNoSuchThing )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⩺) )
import Data.MoreUnicode.Lens     ( (⫥) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- text --------------------------------

import qualified  Data.Text.IO  as  TextIO

import Data.Text                 ( Text )
import Data.Text.Encoding        ( decodeUtf8With )
import Data.Text.Encoding.Error  ( lenientDecode )

-- unix --------------------------------

import System.Posix.Files  ( FileStatus, getFileStatus )

--------------------------------------------------------------------------------

-- | file stat; returns Nothing if file does not exist
stat ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, MonadError ε μ) ⇒
       FPath → μ (Maybe FileStatus)
stat = join ∘ fmap squashNoSuchThing
     ∘ splitMError ∘ asIOError ∘ getFileStatus ∘ (⫥ filepath)

----------------------------------------

-- cribbed shamelessly from RIO.Prelude.IO

{- | Read a file in UTF8 encoding using OS-specific line-ending handling.
     Throw an exception on invalid character.
 -}
readFileUTF8 ∷ (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒ File → μ Text
readFileUTF8 fn =
  asIOError $ withFile (fn ⫥ filepath) ReadMode $ \ h → do
    hSetEncoding h utf8
    TextIO.hGetContents h

--------------------

{- | Read a file in UTF8 encoding using OS-specific line-ending handling.
     Replace any invalid input bytes with the Unicode replacement character
     U+FFFD.
-}
-- plagiarized from https://www.snoyman.com/blog/2016/12/beware-of-readfile
readFileUTF8Lenient ∷ (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒ File → μ Text
readFileUTF8Lenient = decodeUtf8With lenientDecode ⩺ readFileBinary

----------------------------------------

{- | Read a filehandle of UTF8-encoded text. -}
hGetContentsUTF8 ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, MonadError ε μ) ⇒
                   Handle → μ Text
hGetContentsUTF8 h = asIOError $ do
  hSetEncoding h utf8
  liftIO $ TextIO.hGetContents h

----------------------------------------

{- | Read UTF8-encoded text from `stdin`. -}
getContentsUTF8  ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, MonadError ε μ) ⇒
                   μ Text
getContentsUTF8 = hGetContentsUTF8 stdin

----------------------------------------

-- | Same as 'BS.readFile', but generalized to 'MonadIO'
readFileBinary ∷ (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒ File → μ ByteString
readFileBinary = asIOError ∘ liftIO ∘ BS.readFile ∘ review filepath

----------------------------------------

-- XXX SHOULD TAKE OVERWRITE OPTION, AND FILE MODE

{- | Write a file in UTF8 encoding using OS-specific line-ending handling. -}
writeFileUTF8 ∷ (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒
                File → Text → μ ()
writeFileUTF8 fn text =
  asIOError $ withFile (fn ⫥ filepath) WriteMode $ \h → do
    hSetEncoding h utf8
    TextIO.hPutStr h text

----------------------------------------

-- XXX SHOULD TAKE OVERWRITE OPTION, AND FILE MODE

-- | Same as 'BS.writeFile', but generalized to 'MonadIO'
writeFileBinary ∷ ∀ ε μ . (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒
                  File → ByteString → μ ()
writeFileBinary fn = asIOError ∘ BS.writeFile (fn ⫥ filepath)

----------------------------------------

hClose ∷ ∀ ε μ . (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒ Handle → μ ()
hClose = asIOError ∘ System.IO.hClose

-- that's all, folks! ----------------------------------------------------------

