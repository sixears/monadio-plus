{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module MonadIO.OpenFile
  ( openFile', openFileBinary', openFileUTF8'
  , openFile, openFileBinary, openFileUTF8

  , openFileReadBinary', openFileReadWriteBinary', openFileReadWriteExBinary'
  , openFileReadWriteNoTruncBinary'
  , openFileWriteExBinary', openFileWriteBinary', openFileWriteNoTruncBinary'
  , openFileAppendBinary'

  , openFileReadUTF8', openFileReadWriteUTF8', openFileReadWriteExUTF8'
  , openFileReadWriteNoTruncUTF8'
  , openFileWriteExUTF8', openFileWriteUTF8', openFileWriteNoTruncUTF8'
  , openFileAppendUTF8'

  , openFileReadBinary, openFileReadWriteBinary, openFileReadWriteExBinary
  , openFileReadWriteNoTruncBinary
  , openFileWriteExBinary, openFileWriteBinary, openFileWriteNoTruncBinary
  , openFileAppendBinary

  , openFileReadUTF8, openFileReadWriteUTF8, openFileReadWriteExUTF8
  , openFileReadWriteNoTruncUTF8
  , openFileWriteExUTF8, openFileWriteUTF8, openFileWriteNoTruncUTF8
  , openFileAppendUTF8

  , readFileBinary, writeFileBinary, writeExFileBinary, writeNoTruncFileBinary
  , appendFileBinary

  , readFileUTF8, writeFileUTF8, writeNoTruncFileUTF8, writeExFileUTF8
  , appendFileUTF8

  , readFileUTF8Lenient

  , withFile, withFileME
  , withFileBinary, withFileBinaryME, withFileUTF8, withFileUTF8ME

  , withReadFileBinary, withReadWriteFileBinary, withReadWriteExFileBinary
  , withReadWriteNoTruncFileBinary
  , withWriteFileBinary, withWriteExFileBinary, withWriteNoTruncFileBinary
  , withAppendFileBinary

  , withReadFileBinaryME, withReadWriteFileBinaryME
  , withReadWriteExFileBinaryME, withReadWriteNoTruncFileBinaryME
  , withWriteFileBinaryME, withWriteExFileBinaryME
  , withWriteNoTruncFileBinaryME, withAppendFileBinaryME

  , withReadFileUTF8, withReadWriteFileUTF8, withReadWriteExFileUTF8
  , withReadWriteNoTruncFileUTF8
  , withWriteFileUTF8, withWriteExFileUTF8, withWriteNoTruncFileUTF8
  , withAppendFileUTF8

  , withReadFileUTF8ME, withReadWriteFileUTF8ME
  , withReadWriteExFileUTF8ME, withReadWriteNoTruncFileUTF8ME
  , withWriteFileUTF8ME, withWriteExFileUTF8ME
  , withWriteNoTruncFileUTF8ME, withAppendFileUTF8ME

  , tests
  )
where

-- base --------------------------------

import qualified  System.IO

import Control.Monad           ( join, return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bool               ( Bool( False, True ) )
import Data.Either             ( Either )
import Data.Function           ( ($), flip )
import Data.Maybe              ( Maybe( Just, Nothing ) )
import Data.String             ( String )
import GHC.Stack               ( HasCallStack )
import System.Exit             ( ExitCode )
import System.IO               ( Handle, IO
                               , IOMode( AppendMode, ReadMode, ReadWriteMode
                                       , WriteMode )
                               , NewlineMode, TextEncoding
                               , char8, hSetEncoding, hSetNewlineMode
                               , nativeNewlineMode, noNewlineTranslation, utf8
                               )
import System.Posix.Types      ( FileMode )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- bytestring --------------------------

import qualified Data.ByteString  as  BS

import Data.ByteString  ( ByteString )

-- exceptions --------------------------

import Control.Monad.Catch  ( bracket )

-- fpath -------------------------------

import FPath.AbsFile     ( absfile )
import FPath.AsFilePath  ( AsFilePath( filepath ) )
import FPath.File        ( FileAs( _File_ ) )

-- lens --------------------------------

import Control.Lens.Review  ( review )

-- monadio-error -----------------------

import MonadError           ( ѥ )
import MonadError.IO        ( asIOError )
import MonadError.IO.Error  ( AsIOError, IOError )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⫥) )
import Data.MoreUnicode.Maybe    ( 𝕄 )
import Data.MoreUnicode.Monad    ( (≫) )
import Data.MoreUnicode.Natural  ( ℕ )
import Data.MoreUnicode.Text     ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( assertIsLeft, assertRight, runTestsP, runTestsReplay
                  , runTestTree )

-- text --------------------------------

import qualified  Data.Text.IO  as  TextIO

import Data.Text                 ( drop, length )
import Data.Text.Encoding        ( decodeUtf8With )
import Data.Text.Encoding.Error  ( lenientDecode )

-- unix --------------------------------

import System.Posix.IO     ( OpenFileFlags( OpenFileFlags, append, exclusive
                                          , noctty, nonBlock, trunc ),
                             OpenMode( ReadOnly, ReadWrite, WriteOnly )
                           , fdToHandle, noctty, nonBlock, openFd
                           )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.Base      ( chmod, unlink )

--------------------------------------------------------------------------------

{- | OpenFileFlags suitable for reading. -}
readFlags ∷ OpenFileFlags
readFlags = OpenFileFlags { append = False, exclusive = False, noctty = False,
                             nonBlock = False, trunc = False }
--------------------

{- | OpenFileFlags suitable for read-write opens /with pre-truncation/
     (analogous to writeFlags) . -}
readWriteFlags ∷ OpenFileFlags
readWriteFlags = OpenFileFlags { append = False, exclusive = False
                               , noctty = False, nonBlock = False
                               , trunc = True
                               }

--------------------

{- | OpenFileFlags suitable for read-write opens /with pre-truncation/
     (analogous to writeFlags) . -}
readWriteNoTruncFlags ∷ OpenFileFlags
readWriteNoTruncFlags = OpenFileFlags { append = False, exclusive = False
                                      , noctty = False, nonBlock = False
                                      , trunc = False
                                      }

--------------------

{- | OpenFileFlags suitable for read-write opens, with exclusive (file must
     not pre-exist (man file(2):O_EXCL). -}
readWriteExFlags ∷ OpenFileFlags
readWriteExFlags = OpenFileFlags { append = False, exclusive = True
                                 , noctty = False, nonBlock = False
                                 , trunc = False
                                 }

--------------------

{- | OpenFileFlags suitable for writing /with pre-truncation/; this is just the
      `trunc` (man file(2):O_TRUNC) flag. -}
writeFlags ∷ OpenFileFlags
writeFlags = OpenFileFlags { append = False, exclusive = False, noctty = False
                           , nonBlock = False, trunc = True }

--------------------

{- | OpenFileFlags suitable for writing /without pre-truncating/. -}
writeNoTruncFlags ∷ OpenFileFlags
writeNoTruncFlags = OpenFileFlags { append = False, exclusive = False
                                  , noctty = False, nonBlock = False
                                  , trunc = False }

--------------------

{- | OpenFileFlags suitable for writing a new file; this is just the `exclusive`
     (man file(2):O_EXCL) flag.
     This seems redundant in practice, but I've added it here as a belt'n'braces
     thing.
-}
writeExFlags ∷ OpenFileFlags
writeExFlags = OpenFileFlags { append = False, exclusive = True, noctty = False,
                               nonBlock = False, trunc = False }

--------------------

{- | OpenFileFlags suitable for appending; this is just the `append`
     (man file(2):O_APPEND) flag. -}
appendFlags ∷ OpenFileFlags
appendFlags = OpenFileFlags { append = True, exclusive = False, noctty = False,
                              nonBlock = False, trunc = False }

----------------------------------------

openFile' ∷ (MonadIO μ, FileAs γ) ⇒
            TextEncoding → NewlineMode → IOMode → OpenFileFlags → 𝕄 FileMode
          → γ → μ Handle
openFile' enc nlm mode flags perms (review _File_ → fn) = liftIO $ do
  let openMode ReadMode      = ReadOnly
      openMode WriteMode     = WriteOnly
      openMode ReadWriteMode = ReadWrite
      openMode AppendMode    = WriteOnly
      flags'   = case mode of
                   AppendMode → flags { append = True }
                   _          → flags
  h ← openFd (fn ⫥ filepath) (openMode mode) perms flags' ≫ fdToHandle
  hSetEncoding h enc
  hSetNewlineMode h nlm
  return h

--------------------

openFileUTF8' ∷ (MonadIO μ, FileAs γ) ⇒
                IOMode → OpenFileFlags → 𝕄 FileMode → γ → μ Handle
openFileUTF8' = openFile' utf8 nativeNewlineMode

--------------------

openFileBinary' ∷ (MonadIO μ, FileAs γ) ⇒
                  IOMode → OpenFileFlags → 𝕄 FileMode → γ → μ Handle
openFileBinary' = openFile' char8 noNewlineTranslation

----------------------------------------

openFile ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ, HasCallStack) ⇒
           TextEncoding → NewlineMode → IOMode → OpenFileFlags → 𝕄 FileMode → γ
         → μ Handle
openFile enc nlm mode flags perms fn =
   asIOError $ openFile' enc nlm mode flags perms fn

--------------------

openFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ, HasCallStack)⇒
                  IOMode → OpenFileFlags → 𝕄 FileMode → γ → μ Handle
openFileUTF8 mode flags perms = asIOError ∘ openFileUTF8' mode flags perms

--------------------

openFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                  HasCallStack) ⇒
                  IOMode → OpenFileFlags → 𝕄 FileMode → γ → μ Handle
openFileBinary mode flags perms =
  asIOError ∘ openFileBinary' mode flags perms

----------------------------------------

openFileReadBinary' ∷ (MonadIO μ, FileAs γ) ⇒ γ → μ Handle
openFileReadBinary' = liftIO ∘ openFileBinary' ReadMode readFlags Nothing

openFileReadWriteBinary' ∷ (MonadIO μ, FileAs γ) ⇒ 𝕄 FileMode → γ → μ Handle
openFileReadWriteBinary' perms =
  openFileBinary' ReadWriteMode readWriteFlags perms

openFileReadWriteNoTruncBinary' ∷ (MonadIO μ, FileAs γ) ⇒
                                  𝕄 FileMode → γ → μ Handle
openFileReadWriteNoTruncBinary' perms =
  openFileBinary' ReadWriteMode readWriteNoTruncFlags perms

openFileReadWriteExBinary' ∷ (MonadIO μ, FileAs γ) ⇒ 𝕄 FileMode → γ → μ Handle
openFileReadWriteExBinary' perms =
  openFileBinary' ReadWriteMode readWriteExFlags perms

openFileWriteNoTruncBinary' ∷ (MonadIO μ, FileAs γ) ⇒ 𝕄 FileMode → γ → μ Handle
openFileWriteNoTruncBinary' perms =
  openFileBinary' WriteMode writeNoTruncFlags perms

openFileWriteExBinary' ∷ (MonadIO μ, FileAs γ) ⇒ FileMode → γ → μ Handle
openFileWriteExBinary' perms =
  openFileBinary' WriteMode writeExFlags (Just perms)

openFileWriteBinary' ∷ (MonadIO μ, FileAs γ) ⇒ 𝕄 FileMode → γ → μ Handle
openFileWriteBinary' perms =
  openFileBinary' WriteMode writeFlags perms

openFileAppendBinary' ∷ (MonadIO μ, FileAs γ) ⇒ 𝕄 FileMode → γ → μ Handle
openFileAppendBinary' perms = openFileBinary' AppendMode appendFlags perms

----------------------------------------

openFileReadUTF8' ∷ (MonadIO μ, FileAs γ) ⇒ γ → μ Handle
openFileReadUTF8' = liftIO ∘ openFileUTF8' ReadMode readFlags Nothing

openFileReadWriteUTF8' ∷ (MonadIO μ, FileAs γ) ⇒ 𝕄 FileMode → γ → μ Handle
openFileReadWriteUTF8' perms = openFileUTF8' ReadWriteMode readWriteFlags perms

openFileReadWriteNoTruncUTF8' ∷ (MonadIO μ, FileAs γ) ⇒
                                𝕄 FileMode → γ → μ Handle
openFileReadWriteNoTruncUTF8' perms =
  openFileUTF8' ReadWriteMode readWriteNoTruncFlags perms

openFileReadWriteExUTF8' ∷ (MonadIO μ, FileAs γ) ⇒ 𝕄 FileMode → γ → μ Handle
openFileReadWriteExUTF8' perms =
  openFileUTF8' ReadWriteMode readWriteExFlags perms

openFileWriteUTF8' ∷ (MonadIO μ, FileAs γ) ⇒ 𝕄 FileMode → γ → μ Handle
openFileWriteUTF8' perms = openFileUTF8' WriteMode writeFlags perms

openFileWriteNoTruncUTF8' ∷ (MonadIO μ, FileAs γ) ⇒ 𝕄 FileMode → γ → μ Handle
openFileWriteNoTruncUTF8' perms =
  openFileUTF8' WriteMode writeNoTruncFlags perms

openFileWriteExUTF8' ∷ (MonadIO μ, FileAs γ) ⇒ FileMode → γ → μ Handle
openFileWriteExUTF8' perms = openFileUTF8' WriteMode writeExFlags (Just perms)

openFileAppendUTF8' ∷ (MonadIO μ, FileAs γ) ⇒ 𝕄 FileMode → γ → μ Handle
openFileAppendUTF8' perms = openFileUTF8' AppendMode appendFlags perms

----------------------------------------

openFileReadBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack,
                      FileAs γ) ⇒
                       γ → μ Handle
openFileReadBinary = asIOError ∘ openFileReadBinary'

openFileReadWriteBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack,
                           FileAs γ) ⇒
                          𝕄 FileMode → γ → μ Handle
openFileReadWriteBinary perms = asIOError ∘ openFileReadWriteBinary' perms

openFileReadWriteNoTruncBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                                  HasCallStack, FileAs γ) ⇒
                                 𝕄 FileMode → γ → μ Handle
openFileReadWriteNoTruncBinary perms =
  asIOError ∘ openFileReadWriteNoTruncBinary' perms

openFileReadWriteExBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                             HasCallStack, FileAs γ) ⇒
                          𝕄 FileMode → γ → μ Handle
openFileReadWriteExBinary perms = asIOError ∘ openFileReadWriteExBinary' perms

openFileWriteBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack,
                       FileAs γ) ⇒
                      𝕄 FileMode → γ → μ Handle
openFileWriteBinary perms = asIOError ∘ openFileWriteBinary' perms

openFileWriteNoTruncBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                              HasCallStack, FileAs γ) ⇒
                      𝕄 FileMode → γ → μ Handle
openFileWriteNoTruncBinary perms = asIOError ∘ openFileWriteNoTruncBinary' perms

openFileWriteExBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack,
                         FileAs γ) ⇒
                        FileMode → γ → μ Handle
openFileWriteExBinary perms = asIOError ∘ openFileWriteExBinary' perms

openFileAppendBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack,
                        FileAs γ) ⇒
                       𝕄 FileMode → γ → μ Handle
openFileAppendBinary perms = asIOError ∘ openFileAppendBinary' perms

----------------------------------------

openFileReadUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack,
                    FileAs γ) ⇒
                   γ → μ Handle
openFileReadUTF8 = asIOError ∘ openFileReadUTF8'

openFileReadWriteUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack,
                         FileAs γ) ⇒
                        𝕄 FileMode → γ → μ Handle
openFileReadWriteUTF8 perms = asIOError ∘ openFileReadWriteUTF8' perms

openFileReadWriteNoTruncUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                                HasCallStack, FileAs γ) ⇒
                               𝕄 FileMode → γ → μ Handle
openFileReadWriteNoTruncUTF8 perms =
  asIOError ∘ openFileReadWriteNoTruncUTF8' perms

openFileReadWriteExUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack,
                           FileAs γ) ⇒
                          𝕄 FileMode → γ → μ Handle
openFileReadWriteExUTF8 perms = asIOError ∘ openFileReadWriteExUTF8' perms

openFileWriteUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack,
                     FileAs γ) ⇒
                    𝕄 FileMode → γ → μ Handle
openFileWriteUTF8 perms = asIOError ∘ openFileWriteUTF8' perms

openFileWriteNoTruncUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                            HasCallStack, FileAs γ) ⇒
                           𝕄 FileMode → γ → μ Handle
openFileWriteNoTruncUTF8 perms = asIOError ∘ openFileWriteNoTruncUTF8' perms

openFileWriteExUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack,
                       FileAs γ) ⇒
                      FileMode → γ → μ Handle
openFileWriteExUTF8 perms = asIOError ∘ openFileWriteExUTF8' perms

openFileAppendUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack,
                      FileAs γ) ⇒
                     𝕄 FileMode → γ → μ Handle
openFileAppendUTF8 perms = asIOError ∘ openFileAppendUTF8' perms

----------------------------------------

withFile ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ, HasCallStack) ⇒
           TextEncoding → NewlineMode → IOMode → OpenFileFlags → 𝕄 FileMode → γ
         → (Handle → IO ω) → μ ω
withFile enc nlm mode flags perms (review _File_ → fn) io = asIOError $
   bracket (openFile' enc nlm mode flags perms fn) System.IO.hClose io

--------------------

withFileME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ, HasCallStack) ⇒
             TextEncoding → NewlineMode → IOMode → OpenFileFlags → 𝕄 FileMode
           → γ → (Handle → ExceptT ε IO ω) → μ ω
withFileME enc nlm mode flags perms fn io =
  join $ withFile enc nlm mode flags perms fn (ѥ ∘ io)

----------------------------------------

withFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                  HasCallStack) ⇒
                 IOMode → OpenFileFlags → 𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withFileBinary = withFile char8 noNewlineTranslation

--------------------

withFileBinaryME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                    HasCallStack) ⇒
                   IOMode → OpenFileFlags → 𝕄 FileMode → γ
                 → (Handle → ExceptT ε IO ω) → μ ω
withFileBinaryME = withFileME char8 noNewlineTranslation

--------------------

withFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ, HasCallStack)⇒
               IOMode → OpenFileFlags → 𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withFileUTF8 = withFile utf8 nativeNewlineMode

--------------------

withFileUTF8ME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                  HasCallStack) ⇒
                 IOMode → OpenFileFlags → 𝕄 FileMode → γ
               → (Handle → ExceptT ε IO ω) → μ ω
withFileUTF8ME = withFileME utf8 nativeNewlineMode

----------------------------------------

withReadFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                      HasCallStack) ⇒
                   γ → (Handle → IO ω) → μ ω
withReadFileBinary = withFileBinary ReadMode readFlags Nothing

withReadWriteFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                           HasCallStack) ⇒
                          𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withReadWriteFileBinary perms =
  withFileBinary ReadWriteMode readWriteFlags perms

withReadWriteNoTruncFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε,
                                  MonadError ε μ, HasCallStack) ⇒
                                 𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withReadWriteNoTruncFileBinary perms =
  withFileBinary ReadWriteMode readWriteNoTruncFlags perms

withReadWriteExFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                             HasCallStack) ⇒
                            FileMode → γ → (Handle → IO ω) → μ ω
withReadWriteExFileBinary perms =
  withFileBinary ReadWriteMode readWriteExFlags (Just perms)

withWriteFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                       HasCallStack) ⇒
                      𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withWriteFileBinary perms = withFileBinary WriteMode writeFlags perms

withWriteNoTruncFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε,
                              MonadError ε μ, HasCallStack) ⇒
                             𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withWriteNoTruncFileBinary perms =
  withFileBinary WriteMode writeNoTruncFlags perms

withWriteExFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                         HasCallStack) ⇒
                        FileMode → γ → (Handle → IO ω) → μ ω
withWriteExFileBinary perms = withFileBinary WriteMode writeExFlags (Just perms)

withAppendFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                        HasCallStack) ⇒
                       𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withAppendFileBinary perms = withFileBinary AppendMode appendFlags perms

----------------------------------------

withReadFileBinaryME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                        HasCallStack) ⇒
                       γ → (Handle → ExceptT ε IO ω) → μ ω
withReadFileBinaryME = withFileBinaryME ReadMode readFlags Nothing

withReadWriteFileBinaryME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                             HasCallStack) ⇒
                            𝕄 FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withReadWriteFileBinaryME perms =
  withFileBinaryME ReadWriteMode readWriteFlags perms

withReadWriteNoTruncFileBinaryME ∷ (MonadIO μ, FileAs γ, AsIOError ε,
                                    MonadError ε μ, HasCallStack) ⇒
                                   𝕄 FileMode → γ → (Handle → ExceptT ε IO ω)
                                 → μ ω
withReadWriteNoTruncFileBinaryME perms =
  withFileBinaryME ReadWriteMode readWriteNoTruncFlags perms

withReadWriteExFileBinaryME ∷ (MonadIO μ, FileAs γ, AsIOError ε,
                               MonadError ε μ, HasCallStack) ⇒
                              FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withReadWriteExFileBinaryME perms =
  withFileBinaryME ReadWriteMode readWriteExFlags (Just perms)

withWriteFileBinaryME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                         HasCallStack) ⇒
                        𝕄 FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withWriteFileBinaryME perms = withFileBinaryME WriteMode writeFlags perms

withWriteNoTruncFileBinaryME ∷ (MonadIO μ, FileAs γ, AsIOError ε,
                                MonadError ε μ, HasCallStack) ⇒
                               𝕄 FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withWriteNoTruncFileBinaryME perms =
  withFileBinaryME WriteMode writeNoTruncFlags perms

withWriteExFileBinaryME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                           HasCallStack) ⇒
                          FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withWriteExFileBinaryME perms =
  withFileBinaryME WriteMode writeExFlags (Just perms)

withAppendFileBinaryME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                          HasCallStack) ⇒
                       𝕄 FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withAppendFileBinaryME perms = withFileBinaryME AppendMode appendFlags perms

----------------------------------------

withReadFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                    HasCallStack) ⇒
                   γ → (Handle → IO ω) → μ ω
withReadFileUTF8 = withFileUTF8 ReadMode readFlags Nothing

withReadWriteFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                         HasCallStack) ⇒
                        𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withReadWriteFileUTF8 perms = withFileUTF8 ReadWriteMode readWriteFlags perms

withReadWriteNoTruncFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε,
                                MonadError ε μ, HasCallStack) ⇒
                               𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withReadWriteNoTruncFileUTF8 perms =
  withFileUTF8 ReadWriteMode readWriteNoTruncFlags perms

withReadWriteExFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                           HasCallStack) ⇒
                          FileMode → γ → (Handle → IO ω) → μ ω
withReadWriteExFileUTF8 perms =
  withFileUTF8 ReadWriteMode readWriteExFlags (Just perms)

withWriteFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                     HasCallStack) ⇒
                    𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withWriteFileUTF8 perms = withFileUTF8 WriteMode writeFlags perms

withWriteNoTruncFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε,
                            MonadError ε μ, HasCallStack) ⇒
                           𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withWriteNoTruncFileUTF8 perms = withFileUTF8 WriteMode writeNoTruncFlags perms

withWriteExFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                       HasCallStack) ⇒
                      FileMode → γ → (Handle → IO ω) → μ ω
withWriteExFileUTF8 perms = withFileUTF8 WriteMode writeExFlags (Just perms)

withAppendFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                      HasCallStack) ⇒
                     𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withAppendFileUTF8 perms = withFileUTF8 AppendMode appendFlags perms

----------------------------------------

withReadFileUTF8ME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                      HasCallStack) ⇒
                     γ → (Handle → ExceptT ε IO ω) → μ ω
withReadFileUTF8ME = withFileUTF8ME ReadMode readFlags Nothing

withReadWriteFileUTF8ME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                           HasCallStack) ⇒
                          𝕄 FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withReadWriteFileUTF8ME perms =
  withFileUTF8ME ReadWriteMode readWriteFlags perms

withReadWriteNoTruncFileUTF8ME ∷ (MonadIO μ, FileAs γ, AsIOError ε,
                                  MonadError ε μ, HasCallStack) ⇒
                                 𝕄 FileMode → γ → (Handle → ExceptT ε IO ω)
                               → μ ω
withReadWriteNoTruncFileUTF8ME perms =
  withFileUTF8ME ReadWriteMode readWriteNoTruncFlags perms

withReadWriteExFileUTF8ME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                             HasCallStack) ⇒
                            FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withReadWriteExFileUTF8ME perms =
  withFileUTF8ME ReadWriteMode readWriteExFlags (Just perms)

withWriteFileUTF8ME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                       HasCallStack) ⇒
                    𝕄 FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withWriteFileUTF8ME perms = withFileUTF8ME WriteMode writeFlags perms

withWriteNoTruncFileUTF8ME ∷ (MonadIO μ, FileAs γ, AsIOError ε,
                              MonadError ε μ, HasCallStack) ⇒
                             𝕄 FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withWriteNoTruncFileUTF8ME perms =
  withFileUTF8ME WriteMode writeNoTruncFlags perms

withWriteExFileUTF8ME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                         HasCallStack) ⇒
                        FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withWriteExFileUTF8ME perms = withFileUTF8ME WriteMode writeExFlags (Just perms)

withAppendFileUTF8ME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                        HasCallStack) ⇒
                       𝕄 FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withAppendFileUTF8ME perms = withFileUTF8ME AppendMode appendFlags perms

----------------------------------------

{- | Read a file as bytes. -}
readFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack,
                  FileAs γ) ⇒
                 γ → μ ByteString
readFileBinary fn = withReadFileBinary fn BS.hGetContents

writeFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack,
                   FileAs γ) ⇒
                  𝕄 FileMode → γ → ByteString → μ ()
writeFileBinary perms fn t =
  withWriteFileBinary perms fn (flip BS.hPutStr t)

writeNoTruncFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack,
                          FileAs γ) ⇒
                         𝕄 FileMode → γ → ByteString → μ ()
writeNoTruncFileBinary perms fn t =
  withWriteNoTruncFileBinary perms fn (flip BS.hPutStr t)

writeExFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack,
                     FileAs γ) ⇒
                  FileMode → γ → ByteString → μ ()
writeExFileBinary perms fn t =
  withWriteExFileBinary perms fn (flip BS.hPutStr t)

appendFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack,
                    FileAs γ) ⇒
                𝕄 FileMode → γ → ByteString → μ ()
appendFileBinary perms fn t =
  withFileBinary AppendMode appendFlags perms fn (flip BS.hPutStr t)

{- | Read a file in UTF8 encoding using OS-specific line-ending handling.
     Throw an exception on invalid character.
 -}
readFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack,
                FileAs γ) ⇒
               γ → μ 𝕋
readFileUTF8 fn = withReadFileUTF8 fn TextIO.hGetContents

{- | Write a file in UTF8 encoding using OS-specific line-ending handling.
     `perms`, if not Nothing, will be used to create the file if it doesn't
     exist.  If it does exist, `perms` has no impact (use `chmod` to really
     force it).  If `perms is Nothing, and the file does not exist, then an
     exception shall be thrown.
 -}
writeFileUTF8 ∷ forall ε γ μ .
                (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack,
                 FileAs γ) ⇒
                𝕄 FileMode → γ → 𝕋 → μ ()
writeFileUTF8 perms fn t = withWriteFileUTF8 perms fn (flip TextIO.hPutStr t)

writeNoTruncFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack,
                        FileAs γ) ⇒
                        𝕄 FileMode → γ → 𝕋 → μ ()
writeNoTruncFileUTF8 perms fn t =
  withWriteNoTruncFileUTF8 perms fn (flip TextIO.hPutStr t)

writeExFileUTF8 ∷ ∀ γ ε μ . (MonadIO μ, AsIOError ε, MonadError ε μ,
                             HasCallStack, FileAs γ) ⇒
                  FileMode → γ → 𝕋 → μ ()
writeExFileUTF8 perms fn t = withWriteExFileUTF8 perms fn (flip TextIO.hPutStr t)

{- | Write a file in UTF8 encoding using OS-specific line-ending handling.
     `perms`, if not Nothing, will be used to create the file if it doesn't
     exist.  If it does exist, `perms` has no impact (use `chmod` to really
     force it).  If `perms is Nothing, and the file does not exist, then an
     exception shall be thrown.
 -}
appendFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack,
                  FileAs γ) ⇒
                𝕄 FileMode → γ → 𝕋 → μ ()
appendFileUTF8 perms fn t =
  withFileUTF8 AppendMode appendFlags perms fn (flip TextIO.hPutStr t)

withFileTests ∷ TestTree
withFileTests =
  let f = [absfile|/tmp/monadio-file-test.txt|]
      txt = "Swap twenty bottles for an aqua-walkman"
      t2  = "Medicine Show: "
      assertIsRight ∷ Either IOError () → Assertion
      assertIsRight = assertRight (\ _ → () @=? ())
      read ∷ FileAs γ ⇒ γ → IO (Either IOError 𝕋)
      read fn = ѥ $ readFileUTF8 fn
      write ∷ FileAs γ ⇒ 𝕄 FileMode → γ → 𝕋 → IO (Either IOError ())
      write perms fn t = ѥ $ writeFileUTF8 perms fn t
      writeNoTrunc ∷ FileAs γ ⇒ 𝕄 FileMode → γ → 𝕋 → IO (Either IOError ())
      writeNoTrunc perms fn t = ѥ $ writeNoTruncFileUTF8 perms fn t
      -- `append` is imported from System.Posix.IO, so don't shadow that
      appnd ∷ FileAs γ ⇒ 𝕄 FileMode → γ → 𝕋 → IO (Either IOError ())
      appnd perms fn t = ѥ $ appendFileUTF8 perms fn t
      testRead fn t =
        testCase "readFileUTF8" $ read fn ≫ assertRight (t @=?)
      testReadFail fn =
        testCase "readFileUTF8 fail" $ read fn ≫ assertIsLeft
      testWrite perms fn t =
        testCase "writeFileUTF8" $ write perms fn t ≫ assertIsRight
      testWriteNoTrunc perms fn t =
        testCase "writeNoTruncFileUTF8" $
          writeNoTrunc perms fn t ≫ assertIsRight
      testWriteFail perms fn t =
        testCase "writeFileUTF8 fail" $ write perms fn t ≫ assertIsLeft
      testAppend perms fn t =
        testCase "appendFileUTF8" $ appnd perms fn t ≫ assertIsRight
      testAppendFail perms fn t =
        testCase "appendFileUTF8 fail" $ appnd perms fn t ≫ assertIsLeft
   in testGroup "withFile"
                [ -- WRITE NEW FILE NO PERMS, CHECK FOR FAILURE
                  testWriteFail Nothing f txt
                , testWrite (Just 0o600) f txt
                , testRead f txt
                -- re-write, to check for lack of auto-truncation
                , testWriteNoTrunc (Just 0o600) f t2
                , testRead f (t2 ⊕ drop (length t2) txt)
                , testAppend (Just 0o600) f txt
                , testRead f (t2 ⊕ drop (length t2) txt ⊕ txt)
                -- DELETE
                , testCase "delete" $ ѥ (unlink f) ≫ assertIsRight
                -- TEST READ FAIL
                , testReadFail f
                -- APPEND NEW FAIL
                , testAppendFail Nothing f txt
                , testAppend (Just 0o000) f txt
                -- TEST READ FAIL
                , testReadFail f
                , testCase "chmod" $ ѥ (chmod 0400 f) ≫ assertIsRight
                -- DELETE
                , testCase "delete" $ ѥ (unlink f) ≫ assertIsRight
                ]

----------------------------------------

{- | Read a file in UTF8 encoding using OS-specific line-ending handling.
     Replace any invalid input bytes with the Unicode replacement character
     U+FFFD.
-}
-- plagiarized from https://www.snoyman.com/blog/2016/12/beware-of-readfile
readFileUTF8Lenient ∷ (AsIOError ε, MonadError ε μ, HasCallStack, MonadIO μ,
                       FileAs γ) ⇒
                      γ → μ 𝕋
readFileUTF8Lenient fn = decodeUtf8With lenientDecode ⊳ readFileBinary fn

--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "MonadIO.OpenFile" [ withFileTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------

