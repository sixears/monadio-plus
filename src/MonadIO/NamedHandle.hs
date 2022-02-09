module MonadIO.NamedHandle
  ( HasNamedHandle( handle, hname, hiomode ), HEncoding(..)
  , HGetContents( hGetContents )
  , HWriteContents( hWriteContents )
  , ImpliedEncoding( impliedEncoding, impliedEncodingM )
  , NamedHandle( NamedHandle ), ℍ, pattern ℍ
  , hClose, hSetEncoding, stderr, stdin, stdout
  )
where

import Base1T

-- base --------------------------------

import qualified  System.IO

import Control.Monad.Identity  ( Identity( Identity ) )
import System.IO               ( Handle, IOMode( ReadMode, WriteMode )
                               , NewlineMode
                               , char8, nativeNewlineMode, noNewlineTranslation
                               , utf8
                               )

-- bytestring --------------------------

import qualified Data.ByteString  as  BS

import Data.ByteString  ( ByteString )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- text --------------------------------

import qualified  Data.Text.IO  as  TextIO

import Data.Text  ( lines )

--------------------------------------------------------------------------------

data NamedHandle = NamedHandle { _handle  ∷ Handle
                               , _hname   ∷ 𝕋
                               , _hiomode ∷ IOMode
                               }

type ℍ = NamedHandle
pattern ℍ ∷ Handle → 𝕋 → IOMode → ℍ
pattern ℍ h n i ← NamedHandle h n i
  where ℍ h n i = NamedHandle h n i

class HasNamedHandle α where
  hname   ∷ Lens' α 𝕋
  handle  ∷ Lens' α Handle
  hiomode ∷ Lens' α IOMode

instance HasNamedHandle ℍ where
  hname   = lens _hname   (\ h n → h { _hname   = n })
  handle  = lens _handle  (\ h l → h { _handle  = l })
  hiomode = lens _hiomode (\ h i → h { _hiomode = i })

----------------------------------------

type 𝔹𝕊 = ByteString

data HEncoding = UTF8 | Binary | NoEncoding

----------------------------------------

hClose ∷ MonadIO μ ⇒ ℍ → μ ()
hClose = liftIO ∘ System.IO.hClose ∘ view handle

------------------------------------------------------------

hSetNewlineMode ∷ MonadIO μ ⇒ Handle → NewlineMode → μ ()
hSetNewlineMode h = liftIO ∘ System.IO.hSetNewlineMode h

----------------------------------------

hSetEncoding ∷ MonadIO μ ⇒ Handle → HEncoding → μ ()
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

class ToHandle α where
  toHandle ∷ α → Handle

instance ToHandle Handle where
  toHandle = id

instance ToHandle ℍ where
  toHandle = view handle

class ImpliedEncoding α ⇒ HGetContents α where
  hGetContents ∷ (MonadIO μ, ToHandle δ) ⇒ δ → μ α

instance HGetContents 𝕋 where
  hGetContents h =
    liftIO $ hSetEncoding (toHandle h) UTF8 ⪼ TextIO.hGetContents (toHandle h)

instance HGetContents [𝕋] where
  hGetContents = lines ⩺ hGetContents

instance HGetContents 𝔹𝕊 where
  hGetContents h =
    liftIO $ hSetEncoding (toHandle h) Binary ⪼ BS.hGetContents (toHandle h)

------------------------------------------------------------

class ImpliedEncoding α ⇒ HWriteContents α where
  {- | Write some contents to a file with the appropriate encoding.  *Note that
       the handle setting on the encoding is left in place*. -}
  hWriteContents  ∷ MonadIO μ ⇒ ℍ → α → μ ()

instance HWriteContents 𝕋 where
  hWriteContents h t =
    liftIO $ hSetEncoding (toHandle h) UTF8 ⪼ TextIO.hPutStr (h ⊣ handle) t

instance HWriteContents 𝔹𝕊 where
  hWriteContents h b =
    liftIO $ hSetEncoding (toHandle h) Binary ⪼ BS.hPutStr (h ⊣ handle) b

----------------------------------------

stdin  ∷ ℍ
stdin  = ℍ System.IO.stdin "<STDIN>" ReadMode

stdout ∷ ℍ
stdout = ℍ System.IO.stdout "<STDOUT>" WriteMode

stderr ∷ ℍ
stderr = ℍ System.IO.stderr "<STDERR>" WriteMode

-- that's all, folks! ----------------------------------------------------------
