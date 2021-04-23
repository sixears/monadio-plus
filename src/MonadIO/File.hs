{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module MonadIO.File
  ( AccessMode(..), System.IO.IOMode(..)
  , hClose

  , devnull

  , access, writable

  , module MonadIO.FStat

  , chmod, readlink, unlink

  , fileWritable, isWritableFile, isWritableDir

  , openFile', openFileBinary', openFileUTF8'
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

  , readFileBinary, writeFileBinary, writeExFileBinary, writeNoTruncFileBinary
  , appendFileBinary

  , readFileUTF8, writeFileUTF8, writeNoTruncFileUTF8, writeExFileUTF8
  , appendFileUTF8

  , readFileUTF8Lenient

  , fileFoldLinesUTF8, fileFoldLinesH

  , readFlags, readWriteFlags, readWriteExFlags, readWriteNoTruncFlags
  , writeFlags, writeExFlags, writeNoTruncFlags, appendFlags

  , tests
  )
where

import Prelude  ( error, show )

-- base --------------------------------

import qualified  System.IO

import Control.Arrow           ( (>>>) )
import Control.Monad           ( (>=>), (=<<), forM_, join, return, when )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bool               ( Bool( False, True ) )
import Data.Either             ( Either )
import Data.Eq                 ( Eq )
import Data.Foldable           ( Foldable )
import Data.Function           ( ($), flip )
import Data.List               ( isSuffixOf, last, or )
import Data.Maybe              ( Maybe( Just, Nothing ) )
import Data.String             ( String )
import System.Environment      ( getProgName )
import System.Exit             ( ExitCode )
import System.IO               ( FilePath, Handle, IO, NewlineMode, TextEncoding
                               , IOMode( AppendMode, ReadMode, ReadWriteMode
                                       , WriteMode )
                               , char8, hIsEOF, hSetEncoding, hSetNewlineMode
                               , nativeNewlineMode, noNewlineTranslation, utf8
                               )
import System.Posix.Types      ( FileMode )
import Text.Show               ( Show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.List.Unicode      ( (∈) )
import Data.Monoid.Unicode    ( (⊕) )

-- bytestring --------------------------

import qualified Data.ByteString  as  BS

import Data.ByteString  ( ByteString )

-- data-textual ------------------------

import Data.Textual  ( toString )

-- directory ---------------------------

import System.Directory  ( createDirectory, getTemporaryDirectory
                         , removePathForcibly, withCurrentDirectory )

-- exceptions --------------------------

import Control.Monad.Catch  ( bracket, onException )

-- fpath -------------------------------

import FPath.Abs               ( Abs( AbsD, AbsF ) )
import FPath.AbsDir            ( AbsDir, absdir, parseAbsDirP, root )
import FPath.AbsFile           ( AbsFile, absfile )
import FPath.AppendableFPath   ( (⫻) )
import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.AsFilePath'       ( exterminate )
import FPath.Dir               ( DirAs( _Dir_ ) )
import FPath.Error.FPathError  ( AsFPathError, FPathError, FPathIOError )
import FPath.File              ( FileAs( _File_ ) )
import FPath.Parent            ( parent )
import FPath.Parseable         ( parse )
import FPath.RelDir            ( RelDir, reldir )
import FPath.RelFile           ( RelFile, relfile )

-- fstat -------------------------------

import FStat  ( FStat, FileType( Directory, SymbolicLink ), ftype )

-- lens --------------------------------

import Control.Lens.Review  ( review )

import qualified System.FilePath.Lens

-- monadio-error -----------------------

import MonadError           ( ѥ )
import MonadError.IO        ( ӝ, asIOError, asIOErrorY, eitherIOThrow )
import MonadError.IO.Error  ( AsIOError, IOError )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool     ( 𝔹 )
import Data.MoreUnicode.Functor  ( (⊳), (⊳⊳) )
import Data.MoreUnicode.Lens     ( (⊣), (⫥) )
import Data.MoreUnicode.Maybe    ( 𝕄 )
import Data.MoreUnicode.Monad    ( (≪), (≫), (⪼) )
import Data.MoreUnicode.Natural  ( ℕ )
import Data.MoreUnicode.String   ( 𝕊 )
import Data.MoreUnicode.Text     ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )

-- safe --------------------------------

import Safe  ( headMay )

-- tasty -------------------------------

import Test.Tasty  ( TestName, TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), assertIsLeft, assertRight, runTestsP, runTestsReplay
                  , runTestTree, withResourceCleanup )

-- temporary ---------------------------

import System.IO.Temp  ( createTempDirectory )

-- text --------------------------------

import qualified  Data.Text.IO  as  TextIO

import Data.Text                 ( drop, length, pack )
import Data.Text.Encoding        ( decodeUtf8With )
import Data.Text.Encoding.Error  ( lenientDecode )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- time --------------------------------

import Data.Time.Clock  ( getCurrentTime )

-- unix --------------------------------

import qualified  System.Posix.Files  as  Files
import System.Posix.Files  ( readSymbolicLink, removeLink, setFileMode )
import System.Posix.IO     ( OpenFileFlags( OpenFileFlags, append, exclusive
                                          , noctty, nonBlock, trunc ),
                             OpenMode( ReadOnly, ReadWrite, WriteOnly )
                           , fdToHandle, noctty, nonBlock, openFd
                           )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.Base   ( hClose )
import MonadIO.FPath  ( pResolve, pResolveDir )
import MonadIO.FStat  ( FExists( FExists ), lfexists, lstat, stat )
import MonadIO.Temp   ( mkTempDir )

import MonadIO.T.ReadlinkTestCases  ( readExp, readlinkTestCases, resolveExp
                                    , slName, slTarget )

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

openFile ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
           TextEncoding → NewlineMode → IOMode → OpenFileFlags → 𝕄 FileMode → γ
         → μ Handle
openFile enc nlm mode flags perms fn =
   asIOError $ openFile' enc nlm mode flags perms fn

--------------------

openFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                  IOMode → OpenFileFlags → 𝕄 FileMode → γ → μ Handle
openFileUTF8 mode flags perms = asIOError ∘ openFileUTF8' mode flags perms

--------------------

openFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
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

openFileReadBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                       γ → μ Handle
openFileReadBinary = asIOError ∘ openFileReadBinary'

openFileReadWriteBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                          𝕄 FileMode → γ → μ Handle
openFileReadWriteBinary perms = asIOError ∘ openFileReadWriteBinary' perms

openFileReadWriteNoTruncBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                                  FileAs γ) ⇒
                                 𝕄 FileMode → γ → μ Handle
openFileReadWriteNoTruncBinary perms =
  asIOError ∘ openFileReadWriteNoTruncBinary' perms

openFileReadWriteExBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                          𝕄 FileMode → γ → μ Handle
openFileReadWriteExBinary perms = asIOError ∘ openFileReadWriteExBinary' perms

openFileWriteBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                      𝕄 FileMode → γ → μ Handle
openFileWriteBinary perms = asIOError ∘ openFileWriteBinary' perms

openFileWriteNoTruncBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                      𝕄 FileMode → γ → μ Handle
openFileWriteNoTruncBinary perms = asIOError ∘ openFileWriteNoTruncBinary' perms

openFileWriteExBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                        FileMode → γ → μ Handle
openFileWriteExBinary perms = asIOError ∘ openFileWriteExBinary' perms

openFileAppendBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                       𝕄 FileMode → γ → μ Handle
openFileAppendBinary perms = asIOError ∘ openFileAppendBinary' perms

----------------------------------------

openFileReadUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                   γ → μ Handle
openFileReadUTF8 = asIOError ∘ openFileReadUTF8'

openFileReadWriteUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                        𝕄 FileMode → γ → μ Handle
openFileReadWriteUTF8 perms = asIOError ∘ openFileReadWriteUTF8' perms

openFileReadWriteNoTruncUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                                FileAs γ) ⇒
                               𝕄 FileMode → γ → μ Handle
openFileReadWriteNoTruncUTF8 perms =
  asIOError ∘ openFileReadWriteNoTruncUTF8' perms

openFileReadWriteExUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                          𝕄 FileMode → γ → μ Handle
openFileReadWriteExUTF8 perms = asIOError ∘ openFileReadWriteExUTF8' perms

openFileWriteUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                    𝕄 FileMode → γ → μ Handle
openFileWriteUTF8 perms = asIOError ∘ openFileWriteUTF8' perms

openFileWriteNoTruncUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                           𝕄 FileMode → γ → μ Handle
openFileWriteNoTruncUTF8 perms = asIOError ∘ openFileWriteNoTruncUTF8' perms

openFileWriteExUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                      FileMode → γ → μ Handle
openFileWriteExUTF8 perms = asIOError ∘ openFileWriteExUTF8' perms

openFileAppendUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                     𝕄 FileMode → γ → μ Handle
openFileAppendUTF8 perms = asIOError ∘ openFileAppendUTF8' perms

----------------------------------------

withFile ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
           TextEncoding → NewlineMode → IOMode → OpenFileFlags → 𝕄 FileMode → γ
         → (Handle → IO ω) → μ ω
withFile enc nlm mode flags perms (review _File_ → fn) io = asIOError $
   bracket (openFile' enc nlm mode flags perms fn) System.IO.hClose io

--------------------

withFileME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
             TextEncoding → NewlineMode → IOMode → OpenFileFlags → 𝕄 FileMode
           → γ → (Handle → ExceptT ε IO ω) → μ ω
withFileME enc nlm mode flags perms fn io =
  join $ withFile enc nlm mode flags perms fn (ѥ ∘ io)

----------------------------------------

withFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                 IOMode → OpenFileFlags → 𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withFileBinary = withFile char8 noNewlineTranslation

--------------------

withFileBinaryME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                   IOMode → OpenFileFlags → 𝕄 FileMode → γ
                 → (Handle → ExceptT ε IO ω) → μ ω
withFileBinaryME = withFileME char8 noNewlineTranslation

--------------------

withFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
               IOMode → OpenFileFlags → 𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withFileUTF8 = withFile utf8 nativeNewlineMode

--------------------

withFileUTF8ME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                 IOMode → OpenFileFlags → 𝕄 FileMode → γ
               → (Handle → ExceptT ε IO ω) → μ ω
withFileUTF8ME = withFileME utf8 nativeNewlineMode

----------------------------------------

withReadFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                   γ → (Handle → IO ω) → μ ω
withReadFileBinary = withFileBinary ReadMode readFlags Nothing

withReadWriteFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                          𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withReadWriteFileBinary perms =
  withFileBinary ReadWriteMode readWriteFlags perms

withReadWriteNoTruncFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε,
                                  MonadError ε μ) ⇒
                                 𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withReadWriteNoTruncFileBinary perms =
  withFileBinary ReadWriteMode readWriteNoTruncFlags perms

withReadWriteExFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                            FileMode → γ → (Handle → IO ω) → μ ω
withReadWriteExFileBinary perms =
  withFileBinary ReadWriteMode readWriteExFlags (Just perms)

withWriteFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                      𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withWriteFileBinary perms = withFileBinary WriteMode writeFlags perms

withWriteNoTruncFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε,
                              MonadError ε μ) ⇒
                             𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withWriteNoTruncFileBinary perms =
  withFileBinary WriteMode writeNoTruncFlags perms

withWriteExFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                        FileMode → γ → (Handle → IO ω) → μ ω
withWriteExFileBinary perms = withFileBinary WriteMode writeExFlags (Just perms)

withAppendFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                       𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withAppendFileBinary perms = withFileBinary AppendMode appendFlags perms

----------------------------------------

withReadFileBinaryME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                       γ → (Handle → ExceptT ε IO ω) → μ ω
withReadFileBinaryME = withFileBinaryME ReadMode readFlags Nothing

withReadWriteFileBinaryME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                            𝕄 FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withReadWriteFileBinaryME perms =
  withFileBinaryME ReadWriteMode readWriteFlags perms

withReadWriteNoTruncFileBinaryME ∷ (MonadIO μ, FileAs γ, AsIOError ε,
                                    MonadError ε μ) ⇒
                                   𝕄 FileMode → γ → (Handle → ExceptT ε IO ω)
                                 → μ ω
withReadWriteNoTruncFileBinaryME perms =
  withFileBinaryME ReadWriteMode readWriteNoTruncFlags perms

withReadWriteExFileBinaryME ∷ (MonadIO μ, FileAs γ, AsIOError ε,
                               MonadError ε μ) ⇒
                              FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withReadWriteExFileBinaryME perms =
  withFileBinaryME ReadWriteMode readWriteExFlags (Just perms)

withWriteFileBinaryME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                        𝕄 FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withWriteFileBinaryME perms = withFileBinaryME WriteMode writeFlags perms

withWriteNoTruncFileBinaryME ∷ (MonadIO μ, FileAs γ, AsIOError ε,
                                MonadError ε μ) ⇒
                               𝕄 FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withWriteNoTruncFileBinaryME perms =
  withFileBinaryME WriteMode writeNoTruncFlags perms

withWriteExFileBinaryME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                          FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withWriteExFileBinaryME perms =
  withFileBinaryME WriteMode writeExFlags (Just perms)

withAppendFileBinaryME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                       𝕄 FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withAppendFileBinaryME perms = withFileBinaryME AppendMode appendFlags perms

----------------------------------------

withReadFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                   γ → (Handle → IO ω) → μ ω
withReadFileUTF8 = withFileUTF8 ReadMode readFlags Nothing

withReadWriteFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                        𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withReadWriteFileUTF8 perms = withFileUTF8 ReadWriteMode readWriteFlags perms

withReadWriteNoTruncFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε,
                                MonadError ε μ) ⇒
                               𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withReadWriteNoTruncFileUTF8 perms =
  withFileUTF8 ReadWriteMode readWriteNoTruncFlags perms

withReadWriteExFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                          FileMode → γ → (Handle → IO ω) → μ ω
withReadWriteExFileUTF8 perms =
  withFileUTF8 ReadWriteMode readWriteExFlags (Just perms)

withWriteFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                    𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withWriteFileUTF8 perms = withFileUTF8 WriteMode writeFlags perms

withWriteNoTruncFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε,
                            MonadError ε μ) ⇒
                           𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withWriteNoTruncFileUTF8 perms = withFileUTF8 WriteMode writeNoTruncFlags perms

withWriteExFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                      FileMode → γ → (Handle → IO ω) → μ ω
withWriteExFileUTF8 perms = withFileUTF8 WriteMode writeExFlags (Just perms)

withAppendFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                     𝕄 FileMode → γ → (Handle → IO ω) → μ ω
withAppendFileUTF8 perms = withFileUTF8 AppendMode appendFlags perms

----------------------------------------

withReadFileUTF8ME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                     γ → (Handle → ExceptT ε IO ω) → μ ω
withReadFileUTF8ME = withFileUTF8ME ReadMode readFlags Nothing

withReadWriteFileUTF8ME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                          𝕄 FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withReadWriteFileUTF8ME perms =
  withFileUTF8ME ReadWriteMode readWriteFlags perms

withReadWriteNoTruncFileUTF8ME ∷ (MonadIO μ, FileAs γ, AsIOError ε,
                                  MonadError ε μ) ⇒
                                 𝕄 FileMode → γ → (Handle → ExceptT ε IO ω)
                               → μ ω
withReadWriteNoTruncFileUTF8ME perms =
  withFileUTF8ME ReadWriteMode readWriteNoTruncFlags perms

withReadWriteExFileUTF8ME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                            FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withReadWriteExFileUTF8ME perms =
  withFileUTF8ME ReadWriteMode readWriteExFlags (Just perms)

withWriteFileUTF8ME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                    𝕄 FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withWriteFileUTF8ME perms = withFileUTF8ME WriteMode writeFlags perms

withWriteNoTruncFileUTF8ME ∷ (MonadIO μ, FileAs γ, AsIOError ε,
                              MonadError ε μ) ⇒
                             𝕄 FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withWriteNoTruncFileUTF8ME perms =
  withFileUTF8ME WriteMode writeNoTruncFlags perms

withWriteExFileUTF8ME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                        FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withWriteExFileUTF8ME perms = withFileUTF8ME WriteMode writeExFlags (Just perms)

withAppendFileUTF8ME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                       𝕄 FileMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withAppendFileUTF8ME perms = withFileUTF8ME AppendMode appendFlags perms

----------------------------------------

{- | Read a file as bytes. -}
readFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                 γ → μ ByteString
readFileBinary fn = withReadFileBinary fn BS.hGetContents

writeFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                  𝕄 FileMode → γ → ByteString → μ ()
writeFileBinary perms fn t =
  withWriteFileBinary perms fn (flip BS.hPutStr t)

writeNoTruncFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                         𝕄 FileMode → γ → ByteString → μ ()
writeNoTruncFileBinary perms fn t =
  withWriteNoTruncFileBinary perms fn (flip BS.hPutStr t)

writeExFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                  FileMode → γ → ByteString → μ ()
writeExFileBinary perms fn t =
  withWriteExFileBinary perms fn (flip BS.hPutStr t)

appendFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                𝕄 FileMode → γ → ByteString → μ ()
appendFileBinary perms fn t =
  withFileBinary AppendMode appendFlags perms fn (flip BS.hPutStr t)

{- | Read a file in UTF8 encoding using OS-specific line-ending handling.
     Throw an exception on invalid character.
 -}
readFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒ γ → μ 𝕋
readFileUTF8 fn = withReadFileUTF8 fn TextIO.hGetContents

{- | Write a file in UTF8 encoding using OS-specific line-ending handling.
     `perms`, if not Nothing, will be used to create the file if it doesn't
     exist.  If it does exist, `perms` has no impact (use `chmod` to really
     force it).  If `perms is Nothing, and the file does not exist, then an
     exception shall be thrown.
 -}
writeFileUTF8 ∷ forall ε γ μ .
                (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                𝕄 FileMode → γ → 𝕋 → μ ()
writeFileUTF8 perms fn t = withWriteFileUTF8 perms fn (flip TextIO.hPutStr t)

writeNoTruncFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                        𝕄 FileMode → γ → 𝕋 → μ ()
writeNoTruncFileUTF8 perms fn t =
  withWriteNoTruncFileUTF8 perms fn (flip TextIO.hPutStr t)

writeExFileUTF8 ∷ ∀ γ ε μ . (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                  FileMode → γ → 𝕋 → μ ()
writeExFileUTF8 perms fn t = withWriteExFileUTF8 perms fn (flip TextIO.hPutStr t)

{- | Write a file in UTF8 encoding using OS-specific line-ending handling.
     `perms`, if not Nothing, will be used to create the file if it doesn't
     exist.  If it does exist, `perms` has no impact (use `chmod` to really
     force it).  If `perms is Nothing, and the file does not exist, then an
     exception shall be thrown.
 -}
appendFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
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

unlink ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒ γ → μ ()
unlink (review _File_ → fn) = asIOError $ removeLink (fn ⫥ filepath)

----------------------------------------

chmod ∷ ∀ ε ρ μ . (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath ρ) ⇒
        FileMode → ρ → μ ()
chmod perms fn = asIOError $ setFileMode (fn ⫥ filepath) perms

----------------------------------------

{- | Read a file in UTF8 encoding using OS-specific line-ending handling.
     Replace any invalid input bytes with the Unicode replacement character
     U+FFFD.
-}
-- plagiarized from https://www.snoyman.com/blog/2016/12/beware-of-readfile
readFileUTF8Lenient ∷ (AsIOError ε, MonadError ε μ, MonadIO μ, FileAs γ) ⇒
                      γ → μ 𝕋
readFileUTF8Lenient fn = decodeUtf8With lenientDecode ⊳ readFileBinary fn

-- fileAccess ----------------------------------------------

data AccessMode = ACCESS_R | ACCESS_WX | ACCESS_RWX
                | ACCESS_W | ACCESS_RX
                | ACCESS_X | ACCESS_RW
  deriving (Eq,Show)

access ∷ ∀ ε ρ μ . (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath ρ) ⇒
         AccessMode → ρ → μ (𝕄 𝔹)
access mode ((⫥ filepath) → fp) = asIOErrorY $ go mode fp
  where go ∷ AccessMode → FilePath → IO 𝔹
        go ACCESS_R   p = Files.fileAccess (p ⫥ filepath) True  False False
        go ACCESS_W   p = Files.fileAccess (p ⫥ filepath) False True  False
        go ACCESS_X   p = Files.fileAccess (p ⫥ filepath) False False True
        go ACCESS_RW  p = Files.fileAccess (p ⫥ filepath) True  True  False
        go ACCESS_RX  p = Files.fileAccess (p ⫥ filepath) True  False True
        go ACCESS_WX  p = Files.fileAccess (p ⫥ filepath) False True  True
        go ACCESS_RWX p = Files.fileAccess (p ⫥ filepath) True  True  True

{- | Simple shortcut for file (or directory) is writable by this user; `Nothing`
     is returned if file does not exist. -}
writable ∷ ∀ ε ρ μ . (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath ρ) ⇒
            ρ → μ (𝕄 𝔹)
writable = access ACCESS_W

----------------------------------------

{- | Is `f` an extant writable file? -}
_isWritableFile ∷ (MonadIO μ, FileAs γ, MonadError ε μ ,AsIOError ε) ⇒
                  γ → 𝕄 FStat -> μ (𝕄 𝕋)

_isWritableFile (review _File_ → f) st =
  let rJust = return ∘ Just
   in case st of
        Nothing  → rJust $ [fmt|%T does not exist|] f
        Just stp → if Directory ≡ ftype stp
                   then rJust $ [fmt|%T is a directory|] f
                   else writable f ≫ \ case
                          Nothing    → rJust $ [fmt|no such file %T|] f
                          Just True  → return Nothing
                          Just False → rJust $ [fmt|cannot write to %T|] f

----------------------------------------

{- | Is `f` an extant writable file? -}
isWritableFile ∷ (MonadIO μ, FileAs γ, MonadError ε μ, AsIOError ε) ⇒
                 γ -> μ (𝕄 𝕋)

isWritableFile (review _File_ → f) = stat f ≫ _isWritableFile f

----------------------------------------

{- | Is `d` an extant writable directory? -}
isWritableDir ∷ ∀ γ ε μ . (MonadIO μ, DirAs γ, MonadError ε μ, AsIOError ε) ⇒
                γ -> μ (𝕄 𝕋)

isWritableDir d =
  let rJust = return ∘ Just
   in stat d ≫ \ case
        Nothing  → rJust $ [fmt|%T does not exist|] d
        Just stp → if Directory ≡ ftype stp
                   then writable d ≫ \ case
                          Nothing    → rJust $ [fmt|no such directory %T|] d
                          Just True  → return Nothing
                          Just False → rJust $ [fmt|cannot write to %T|] d
                   else -- remove trailing '/', since the point is that d is
                        -- not a directory
                        rJust $ [fmt|%s is not a directory|]
                                (exterminate (d ⫥ filepath))

----------

isWritableDirTests ∷ TestTree
isWritableDirTests =
  let testE f e = testCase (toString f) $
                    ѥ (isWritableDir @_ @IOError f) ≫ assertRight (Just e @=?)
      testN f   = testCase (toString f) $
                    ѥ (isWritableDir @_ @IOError f) ≫ assertRight (Nothing @=?)
   in testGroup "isWritableDir"
            [ testN [absdir|/tmp/|]
            , testE [absdir|/nonsuch/|]
                    "/nonsuch/ does not exist"
            , testE [absdir|/nonsuch/passwd/|]
                    "/nonsuch/passwd/ does not exist"
            , testE [absdir|/etc/|]
                    "cannot write to /etc/"
            , testE [absdir|/etc/passwd/|]
                    "/etc/passwd is not a directory"
            ]

----------------------------------------

{- | Test that the given path is a writable (by this user) *file*, or does not
     exist but is in a directory that is writable & executable by this user.
     In case of not writable, some error text is returned to say why.
 -}
fileWritable ∷ ∀ γ ε μ .
               (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
               γ → μ (𝕄 𝕋)
fileWritable (review _File_ → fn) = do
  stat fn ≫ \ case
    Just st → _isWritableFile fn (Just st)
    Nothing → -- fn does not exist; does it have a writeable dir parent?
              isWritableDir (fn ⊣ parent) ≫ \ case
                   Nothing → return Nothing
                   Just e  → return ∘ Just $ [fmt|%t (%T)|] e fn

----------

fileWritableTests ∷ TestTree
fileWritableTests =
  let testE f e = testCase (toString f) $
                    ѥ (fileWritable @_ @IOError f) ≫ assertRight (Just e @=?)
      testE' f e = testCase (toString f) $
                     ѥ (fileWritable @_ @IOError f) ≫ assertRight (e @=?)

   in testGroup "fileWritable"
            [ testE [absfile|/etc/passwd|]
                    "cannot write to /etc/passwd"
            , testE [absfile|/nonsuch/passwd|]
                    "/nonsuch/ does not exist (/nonsuch/passwd)"
            , testE [absfile|/etc/nonsuch|]
                    "cannot write to /etc/ (/etc/nonsuch)"
            , testE [absfile|/etc/passwd/nonsuch|]
                    "/etc/passwd is not a directory (/etc/passwd/nonsuch)"
            , testE [absfile|/etc/pam.d|]
                    "/etc/pam.d is a directory"

            , testE' [absfile|/dev/null|] Nothing
            ]

----------------------------------------

{- | Work over a file, accumulating results, line-by-line. -}
fileFoldLinesUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                     α → (α → 𝕋 → IO α) → γ → μ α
fileFoldLinesUTF8 a io fn = withReadFileUTF8 fn $ fileFoldLinesH a io

fileFoldLinesH ∷ (MonadIO μ) ⇒ α → (α → 𝕋 → μ α) → Handle → μ α
fileFoldLinesH a io h = do
  eof ← liftIO $ hIsEOF h
  case eof of
    True  → return a
    False → do l ← liftIO $ TextIO.hGetLine h
               a' ← io a l
               fileFoldLinesH a' io h

----------------------------------------

{- | An open RW handle to /dev/null. -}
devnull ∷ (MonadIO μ, AsIOError ε, MonadError ε μ) ⇒ μ Handle
devnull = openFileReadWriteNoTruncBinary Nothing [absfile|/dev/null|]

----------------------------------------

nuke ∷ ∀ ε ρ μ . (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath ρ) ⇒
       ρ → μ ()
nuke (review filepath → fp) = asIOError $ removePathForcibly fp

mkdir ∷ ∀ ε δ μ . (MonadIO μ, AsIOError ε, MonadError ε μ, DirAs δ) ⇒
        δ → FileMode → μ ()
mkdir d p = do
  let _mkdir = asIOError ∘ createDirectory ∘ (review $ filepath ∘ _Dir_)
  pre_exists ← lfexists d
  asIOError $ onException (ӝ $ _mkdir d ⪼ chmod @IOError p d)
                          (ӝ $ when (FExists ≡ pre_exists) $ nuke @IOError d)

----------------------------------------

-- This has to return an absolute path, as the relative path might include
-- many '..' that can't be represented by FPath.  So we resolve it.
{- | Read a symlink, return the absolute path to the referent.  Note that as
     with readlink(2); a directory (any `filepath` ending in a '/'; including
     those from an `FPath.{Abs,Rel,}Dir` type) will give rise to an EINVAL. -}
readlink ∷ ∀ ε γ μ .
           (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ,
            AsFilePath γ) ⇒
           γ → μ Abs
readlink (review filepath → fp) = do
  r ← asIOError $ readSymbolicLink fp
  case headMay r of
    Nothing  → error $ [fmt|empty symlink found at '%s'|] fp
    Just '/' → -- last is safe, as fp is non-empty, given that headMay fp
               -- is not Nothing
               case last r of
                 '/' → AbsD ⊳ pResolveDir root r
                 _   → AbsF ⊳ pResolveDir root r
    Just _   → do d ← pResolve (fp ⊣ System.FilePath.Lens.directory)
                   -- last is safe, as fp is non-empty, given that headMay fp
                   -- is not Nothing
                  case last r of
                    '/' → AbsD ⊳ pResolveDir d r
                    _   → if or [ r ∈ [ ".", ".." ]
                                , "/." `isSuffixOf` r
                                , "/.." `isSuffixOf` r
                                ]
                          then AbsD ⊳ pResolveDir d r
                          else AbsF ⊳ pResolveDir d r

----------

{- | Perform tests within a temporary directory, with a bespoke temp dir setup
     function.  The setup function is called with the name of the temp dir
     (as a filepath: FPath is not available here, as FPath uses TastyPlus); and
     the tempdir is also provided to the test (as an IO FilePath as a Tasty
     quirk).
 -}
testInTempDir ∷ (FilePath → IO()) → (IO FilePath → TestTree) → TestTree
testInTempDir setup =
  withResourceCleanup
    (getTemporaryDirectory ≫ \ t → getProgName ≫ createTempDirectory t)
    setup removePathForcibly

{- | Parse an `AbsDir`, throwing any errors into IO. -}
parseAbsDirIO ∷ FilePath → IO AbsDir
parseAbsDirIO = eitherIOThrow ∘ parseAbsDirP @FPathError

testInTempDir' ∷ (AbsDir → IO()) → (IO AbsDir → TestTree) → TestTree
testInTempDir' setup test =
  testInTempDir (parseAbsDirIO >=> setup) (test ∘ (parseAbsDirIO ≪))

data TestFileSpec = TFSFile RelFile FileMode 𝕋
                  | TFSDir  RelDir  FileMode
                  -- symlinks can point to any old filepath, not just to valid
                  -- ones, hence we allow that here (mostly for testing
                  -- readlink); in practice, most uses should use a converted
                  -- FPath
                  | TFSSymL RelFile FilePath

-- make dirs in file names as needed
testInTempDirFS ∷ Foldable φ ⇒
                  φ TestFileSpec → (AbsDir → IO()) → (IO AbsDir → TestTree)
                → TestTree
testInTempDirFS fs setup =
  testInTempDir' (\ d → ӝ (mkTFSes @IOError d fs) ⪼ setup d)

mkTFS ∷ (MonadIO μ, AsIOError ε, MonadError ε μ) ⇒ AbsDir → TestFileSpec → μ ()
mkTFS p (TFSFile f m t) = writeExFileUTF8 @AbsFile m (p ⫻ f) t
mkTFS p (TFSDir  d m)   = mkdir @_ @AbsDir (p ⫻ d) m
mkTFS p (TFSSymL f t)   =
  asIOError $ Files.createSymbolicLink t ((p ⫻ f ∷ AbsFile) ⫥ filepath)

mkTFSes ∷ ∀ ε φ μ . (Foldable φ, MonadIO μ, AsIOError ε, MonadError ε μ) ⇒
          AbsDir -> φ TestFileSpec -> μ ()
mkTFSes d fs = forM_ fs (mkTFS d)


_readlinkTests ∷ TestName → (𝕊 → IO (Either FPathIOError Abs)) → (α → RelFile)
               → (α → FilePath) → (α → AbsDir → Abs) → [α] → TestTree
_readlinkTests name f getName getTarget getExp ts =
  let mkTempDir_ ∷ MonadIO μ ⇒ μ AbsDir
      mkTempDir_ = ӝ $ mkTempDir @FPathIOError

      {- | Write a file, perms 0700, throw IOException on error.  -}
      writeUTF8 ∷ FileAs γ ⇒ γ → 𝕋 → IO ()
      writeUTF8 fn = ӝ ∘ writeFileUTF8 @IOError (Just 0o700) fn

      {- | Write a file with the current time, throw IOException on error -}
      writeTime ∷ FileAs γ ⇒ γ → IO ()
      writeTime fn = (pack ∘ show ⊳ getCurrentTime) ≫ writeUTF8 fn

      {- | Write links, also a plain file & a dir, into the temp dir for
           testing. -}
      populateTemp ∷ (MonadIO μ, MonadError FPathIOError μ) ⇒ AbsDir → μ ()
      populateTemp d = liftIO $ do
        return ()

      -- We factor this out so it can be run on exception during creation;
      -- `withResource` does not run the resource-close step if there was an
      -- IOException during the resource-acquisition step
      delTemp ∷ AbsDir → IO ()
      delTemp = ӝ ∘ nuke @FPathIOError

    in testInTempDirFS ([TFSDir [reldir|directory/|] 0o700, TFSFile [relfile|plain|] 0o644 "some text"] ⊕ [TFSSymL (getName t) (getTarget t) | t ← ts]) (ӝ ∘ populateTemp) $
      \ tmpdir →
      let check ∷ 𝕊 → (AbsDir → Abs) → TestTree
          check fn exp = let path t = toString t ⊕ "/" ⊕ fn
                          in testCase fn $ tmpdir ≫ \ t →
                               f (path t) ≫ assertRight (exp t ≟)
       in testGroup name [ check (review filepath $ getName t) (getExp t) | t ← ts ]

----------

readlinkTests ∷ TestTree
readlinkTests =
  _readlinkTests "readlink" (ѥ ∘ readlink) slName slTarget readExp
                 readlinkTestCases

--------------------

{- | Recursively read a symbolic link, until it is a symbolic link no more.
     Anything other than a (readable) symbolic link is immediately returned
     intact (including non-existent files).  A symbolic link is deferenced, and
     the result re-examined.
 -}
resolvelink ∷ ∀ ε γ μ .
            (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ,
             AsFilePath γ) ⇒
            γ → μ Abs
resolvelink fp = do
  r ← readlink fp
  ftype ⊳⊳ lstat r ≫ \ case
    Just SymbolicLink → resolvelink r
    _                 → return r

----------

resolvelinkTests ∷ TestTree
resolvelinkTests = _readlinkTests "resolvelink" (ѥ ∘ resolvelink) slName
                   slTarget resolveExp readlinkTestCases


----------------------------------------

tests ∷ TestTree
tests = testGroup "MonadIO.File" [ isWritableDirTests, fileWritableTests
                                 , withFileTests, readlinkTests
                                 , resolvelinkTests
                                 ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------

