module MonadIO.Handle
  ( HEncoding(..), HGetContents( hGetContents )
  , HWriteContents( hWriteContents )
  , ImpliedEncoding( impliedEncoding, impliedEncodingM )
  , hSetEncoding
  )
where

-- base --------------------------------

import qualified  System.IO

import Control.Monad           ( return )
import Control.Monad.Identity  ( Identity( Identity ) )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Function           ( ($) )
import System.IO               ( Handle, char8, hSetNewlineMode
                               , nativeNewlineMode, noNewlineTranslation, utf8 )

-- bytestring --------------------------

import qualified Data.ByteString  as  BS

import Data.ByteString  ( ByteString )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⩺) )
import Data.MoreUnicode.Monad    ( (⪼) )
import Data.MoreUnicode.Text     ( 𝕋 )

-- text --------------------------------

import qualified  Data.Text.IO  as  TextIO

import Data.Text  ( lines )

--------------------------------------------------------------------------------

type ℍ  = Handle
type 𝔹𝕊 = ByteString

data HEncoding = UTF8 | Binary | NoEncoding

hSetEncoding ∷ MonadIO μ ⇒ ℍ → HEncoding → μ ()
hSetEncoding h UTF8 = liftIO $ do
  System.IO.hSetEncoding h utf8
  hSetNewlineMode        h nativeNewlineMode
hSetEncoding h Binary = liftIO $ do
  System.IO.hSetEncoding h char8
  hSetNewlineMode        h noNewlineTranslation
hSetEncoding _ NoEncoding = return ()

------------------------------------------------------------

class ImpliedEncoding α where
  impliedEncodingM ∷ η α → HEncoding
  impliedEncoding ∷ α → HEncoding

  impliedEncoding a = impliedEncodingM (Identity  a)

instance ImpliedEncoding 𝕋 where
  impliedEncodingM _ = UTF8

instance ImpliedEncoding [𝕋] where
  impliedEncodingM _ = UTF8

instance ImpliedEncoding 𝔹𝕊 where
  impliedEncodingM _ = Binary

instance ImpliedEncoding () where
  impliedEncodingM _ = NoEncoding


------------------------------------------------------------

class ImpliedEncoding α ⇒ HGetContents α where
  hGetContents ∷ MonadIO μ ⇒ ℍ → μ α

instance HGetContents 𝕋 where
  hGetContents h = liftIO $ hSetEncoding h UTF8 ⪼ TextIO.hGetContents h

instance HGetContents [𝕋] where
  hGetContents = lines ⩺ hGetContents

instance HGetContents 𝔹𝕊 where
  hGetContents h = liftIO $ hSetEncoding h Binary ⪼ BS.hGetContents h

------------------------------------------------------------

class ImpliedEncoding α ⇒ HWriteContents α where
  {- | Write some contents to a file with the appropriate encoding.  *Note that
       the handle setting on the encoding is left in place*. -}
  hWriteContents  ∷ MonadIO μ ⇒ ℍ → α → μ ()

instance HWriteContents 𝕋 where
  hWriteContents h t = liftIO $ hSetEncoding h UTF8 ⪼ TextIO.hPutStr h t

instance HWriteContents 𝔹𝕊 where
  hWriteContents h b = liftIO $ hSetEncoding h Binary ⪼ BS.hPutStr h b

-- that's all, folks! ----------------------------------------------------------
