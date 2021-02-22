{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module MonadIO.File
  ( System.IO.IOMode(..)
  , hClose

  , devnull

  , access, stat, writable

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
  
  , withFile, withFileME, withFileUTF8, withFileBinary

  , withReadFileBinary, withReadWriteFileBinary, withReadWriteExFileBinary
  , withReadWriteNoTruncFileBinary
  , withWriteFileBinary, withWriteExFileBinary, withWriteNoTruncFileBinary
  , withAppendFileBinary

  , withReadFileUTF8, withReadWriteFileUTF8, withReadWriteExFileUTF8
  , withReadWriteNoTruncFileUTF8
  , withWriteFileUTF8, withWriteExFileUTF8, withWriteNoTruncFileUTF8
  , withAppendFileUTF8

  , readFileBinary, writeFileBinary, writeExFileBinary, writeNoTruncFileBinary
  , appendFileBinary

  , readFileUTF8, writeFileUTF8, writeNoTruncFileUTF8, writeExFileUTF8
  , appendFileUTF8

  , readFileUTF8Lenient
  
  , fileFoldLinesUTF8, fileFoldLinesH

  , readFlags, readWriteFlags, readWriteExFlags, readWriteNoTruncFlags
  , writeFlags, writeExFlags, writeNoTruncFlags, appendFlags
  )
where

-- base --------------------------------

import qualified  System.IO

import Control.Exception       ( bracket )
import Control.Monad           ( join, return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bool               ( Bool( False, True ), bool )
import Data.Either             ( Either)
import Data.Eq                 ( Eq )
import Data.Function           ( ($), flip )
import Data.Maybe              ( Maybe( Just, Nothing ), fromMaybe )
import Data.String             ( String )
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

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- bytestring --------------------------

import qualified Data.ByteString  as  BS

import Data.ByteString  ( ByteString )

-- data-textual ------------------------

import Data.Textual  ( toString )

-- fpath -------------------------------

import FPath.AbsDir       ( absdir )
import FPath.AbsFile      ( absfile )
import FPath.AsFilePath   ( AsFilePath( filepath ) )
import FPath.AsFilePath'  ( exterminate )
import FPath.DirLike      ( IsDir )
import FPath.File         ( FileAs( _File_ ) )
import FPath.Parent       ( parent )

-- lens --------------------------------

import Control.Lens.Review  ( review )

-- monadio-error -----------------------

import MonadError           ( ѥ )
import MonadError.IO        ( asIOError, asIOErrorY )
import MonadError.IO.Error  ( AsIOError, IOError, squashInappropriateTypeT )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool     ( 𝔹 )
import Data.MoreUnicode.Functor  ( (⊳), (⊳⊳⊳), (⩺) )
import Data.MoreUnicode.Lens     ( (⊣), (⫥) )
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

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- unix --------------------------------

import qualified  System.Posix.Files  as  Files
import System.Posix.Files  ( FileStatus, fileExist, getFileStatus, isDirectory
                           , removeLink, setFileMode )
import System.Posix.IO     ( OpenFileFlags( OpenFileFlags, append, exclusive
                                          , noctty, nonBlock, trunc ),
                             OpenMode( ReadOnly, ReadWrite, WriteOnly )
                           , fdToHandle, noctty, nonBlock, openFd
                           )

--------------------------------------------------------------------------------

data FExists = FExists | NoFExists
  deriving (Eq,Show)

{- | Does file exist.  Note that "does /etc/passwd/ exist?", where /etc/passwd
     exists but is a file, will return `NoFExists`; but "does /etc exist?" where
     /etc exists but is a directory will return `FExists`.  See also `fexists'`.
 -}
fexists ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath τ) ⇒ τ → μ FExists
-- fileExist throws an InappropriateType IOException if you ask about a file
-- in a non-existent directory.  I think that sucks, and should be a simple
-- False (NoFExists)
fexists f = fromMaybe NoFExists ⩺ squashInappropriateTypeT ∘ asIOError $
              bool NoFExists FExists ⊳ fileExist (f ⫥ filepath)

----------

fexistsTests ∷ TestTree
fexistsTests =
  let testFExists expect input =
        testCase (toString input) $
          (ѥ @IOError (fexists input)) ≫ assertRight (expect @=?)
   in testGroup "fexists"
                [ testFExists FExists   [absdir|/etc/|]
                , testFExists NoFExists [absdir|/nonsuch/|]
                , testFExists NoFExists [absdir|/etc/nonsuch/|]
                , testFExists FExists   [absfile|/etc/passwd|]
                , testFExists NoFExists [absdir|/etc/passwd/|]
                , testFExists NoFExists [absfile|/etc/passwd/nonsuch|]
                , testFExists NoFExists [absdir|/etc/passwd/nonsuch/|]
                ]

--------------------

{- | Does file exist.  Note that "does /etc/passwd/ exist?", where /etc/passwd
     exists but is a file, will return `FExists`.  See also `fexists`.  This is
     more symmetric, since "does /etc exist?" where /etc exists but is a
     directory will return `FExists`; but at a cost of being arguably less
     accurate.
 -}
fexists' ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath τ)⇒ τ → μ FExists
-- fileExist throws an InappropriateType IOException if you ask about a file
-- in a directory that is in reality a file.  I think that sucks, and should be
-- a simple False (NoFExists)
fexists' f = fromMaybe NoFExists ⩺ squashInappropriateTypeT ∘ asIOError $
               bool NoFExists FExists ⊳ fileExist (exterminate $ f ⫥ filepath)

----------

fexists'Tests ∷ TestTree
fexists'Tests =
  let testFExists' expect input =
        testCase (toString input) $
          (ѥ @IOError (fexists' input)) ≫ assertRight (expect @=?)
   in testGroup "fexists'"
                [ testFExists' FExists   [absdir|/etc/|]
                , testFExists' NoFExists [absdir|/nonsuch/|]
                , testFExists' NoFExists [absdir|/etc/nonsuch/|]
                , testFExists' FExists   [absfile|/etc/passwd|]
                , testFExists' FExists   [absdir|/etc/passwd/|]
                , testFExists' NoFExists [absfile|/etc/passwd/nonsuch|]
                , testFExists' NoFExists [absdir|/etc/passwd/nonsuch/|]
                ]

----------------------------------------

-- | file stat; returns Nothing if file does not exist
stat ∷ ∀ ε ρ μ . (MonadIO μ, AsFilePath ρ, AsIOError ε, MonadError ε μ) ⇒
       ρ → μ (𝕄 FileStatus)
stat f = do
  -- The fexists' introduces a race-condition - bah - but without it, the
  -- stat may fail with an `InappropriateType` IOException when trying to stat
  -- a file in a "directory" that is in reality a file.  I think that sucks, and
  -- want to try that like any other non-existent file.
  fexists' f ≫ \ case
    NoFExists → return Nothing
    FExists   → asIOErrorY ∘ getFileStatus ∘ exterminate $ (f ⫥ filepath)

----------

statTests ∷ TestTree
statTests =
  let testStat expect input f =
        testCase (toString input) $
          f (ѥ @IOError (stat input)) ≫ assertRight (expect @=?)
   in testGroup "stat"
                [ testStat (Just True)  [absdir|/etc/|]        (isDirectory ⊳⊳⊳)
                , testStat (Just False) [absfile|/etc/passwd|] (isDirectory ⊳⊳⊳)
                , testStat (Just False) [absdir|/etc/passwd/|] (isDirectory ⊳⊳⊳)
                , testStat Nothing      [absfile|/nonsuch|]    (isDirectory ⊳⊳⊳)
                , testStat Nothing      [absfile|/etc/passwd/nonsuch|]
                                                               (isDirectory ⊳⊳⊳)
                , testStat Nothing      [absdir|/nonsuch/|]    (isDirectory ⊳⊳⊳)
                ]

----------------------------------------

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
            TextEncoding → NewlineMode → IOMode → OpenFileFlags → (𝕄 FileMode)
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
                IOMode → OpenFileFlags → (𝕄 FileMode) → γ → μ Handle
openFileUTF8' = openFile' utf8 nativeNewlineMode

--------------------

openFileBinary' ∷ (MonadIO μ, FileAs γ) ⇒
                  IOMode → OpenFileFlags → (𝕄 FileMode) → γ → μ Handle
openFileBinary' = openFile' char8 noNewlineTranslation

----------------------------------------

openFile ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
           TextEncoding → NewlineMode → IOMode → OpenFileFlags → (𝕄 FileMode)
         → γ → μ Handle
openFile enc nlm mode flags perms fn =
   asIOError $ openFile' enc nlm mode flags perms fn

--------------------

openFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                  IOMode → OpenFileFlags → (𝕄 FileMode) → γ → μ Handle
openFileUTF8 mode flags perms = asIOError ∘ openFileUTF8' mode flags perms

--------------------

openFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                  IOMode → OpenFileFlags → (𝕄 FileMode) → γ → μ Handle
openFileBinary mode flags perms =
  asIOError ∘ openFileBinary' mode flags perms

----------------------------------------

openFileReadBinary' ∷ (MonadIO μ, FileAs γ) ⇒ γ → μ Handle
openFileReadBinary' = liftIO ∘ openFileBinary' ReadMode readFlags Nothing

openFileReadWriteBinary' ∷ (MonadIO μ, FileAs γ) ⇒ (𝕄 FileMode) → γ → μ Handle
openFileReadWriteBinary' perms =
  openFileBinary' ReadWriteMode readWriteFlags perms

openFileReadWriteNoTruncBinary' ∷ (MonadIO μ, FileAs γ) ⇒
                                  (𝕄 FileMode) → γ → μ Handle
openFileReadWriteNoTruncBinary' perms =
  openFileBinary' ReadWriteMode readWriteNoTruncFlags perms

openFileReadWriteExBinary' ∷ (MonadIO μ, FileAs γ) ⇒
                             (𝕄 FileMode) → γ → μ Handle
openFileReadWriteExBinary' perms =
  openFileBinary' ReadWriteMode readWriteExFlags perms

openFileWriteNoTruncBinary' ∷ (MonadIO μ, FileAs γ) ⇒
                              (𝕄 FileMode) → γ → μ Handle
openFileWriteNoTruncBinary' perms =
  openFileBinary' WriteMode writeNoTruncFlags perms

openFileWriteExBinary' ∷ (MonadIO μ, FileAs γ) ⇒ FileMode → γ → μ Handle
openFileWriteExBinary' perms =
  openFileBinary' WriteMode writeExFlags (Just perms)

openFileWriteBinary' ∷ (MonadIO μ, FileAs γ) ⇒ (𝕄 FileMode) → γ → μ Handle
openFileWriteBinary' perms =
  openFileBinary' WriteMode writeFlags perms

openFileAppendBinary' ∷ (MonadIO μ, FileAs γ) ⇒ (𝕄 FileMode) → γ → μ Handle
openFileAppendBinary' perms = openFileBinary' AppendMode appendFlags perms

----------------------------------------

openFileReadUTF8' ∷ (MonadIO μ, FileAs γ) ⇒ γ → μ Handle
openFileReadUTF8' = liftIO ∘ openFileUTF8' ReadMode readFlags Nothing

openFileReadWriteUTF8' ∷ (MonadIO μ, FileAs γ) ⇒ (𝕄 FileMode) → γ → μ Handle
openFileReadWriteUTF8' perms = openFileUTF8' ReadWriteMode readWriteFlags perms

openFileReadWriteNoTruncUTF8' ∷ (MonadIO μ, FileAs γ) ⇒
                                (𝕄 FileMode) → γ → μ Handle
openFileReadWriteNoTruncUTF8' perms =
  openFileUTF8' ReadWriteMode readWriteNoTruncFlags perms

openFileReadWriteExUTF8' ∷ (MonadIO μ, FileAs γ) ⇒ (𝕄 FileMode) → γ → μ Handle
openFileReadWriteExUTF8' perms =
  openFileUTF8' ReadWriteMode readWriteExFlags perms

openFileWriteUTF8' ∷ (MonadIO μ, FileAs γ) ⇒ (𝕄 FileMode) → γ → μ Handle
openFileWriteUTF8' perms = openFileUTF8' WriteMode writeFlags perms

openFileWriteNoTruncUTF8' ∷ (MonadIO μ, FileAs γ) ⇒ (𝕄 FileMode) → γ → μ Handle
openFileWriteNoTruncUTF8' perms =
  openFileUTF8' WriteMode writeNoTruncFlags perms

openFileWriteExUTF8' ∷ (MonadIO μ, FileAs γ) ⇒ FileMode → γ → μ Handle
openFileWriteExUTF8' perms = openFileUTF8' WriteMode writeExFlags (Just perms)

openFileAppendUTF8' ∷ (MonadIO μ, FileAs γ) ⇒ (𝕄 FileMode) → γ → μ Handle
openFileAppendUTF8' perms = openFileUTF8' AppendMode appendFlags perms

----------------------------------------

openFileReadBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                       γ → μ Handle
openFileReadBinary = asIOError ∘ openFileReadBinary'

openFileReadWriteBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                          (𝕄 FileMode) → γ → μ Handle
openFileReadWriteBinary perms = asIOError ∘ openFileReadWriteBinary' perms

openFileReadWriteNoTruncBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                                  FileAs γ) ⇒
                                 (𝕄 FileMode) → γ → μ Handle
openFileReadWriteNoTruncBinary perms =
  asIOError ∘ openFileReadWriteNoTruncBinary' perms

openFileReadWriteExBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                          (𝕄 FileMode) → γ → μ Handle
openFileReadWriteExBinary perms = asIOError ∘ openFileReadWriteExBinary' perms

openFileWriteBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                      (𝕄 FileMode) → γ → μ Handle
openFileWriteBinary perms = asIOError ∘ openFileWriteBinary' perms

openFileWriteNoTruncBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                      (𝕄 FileMode) → γ → μ Handle
openFileWriteNoTruncBinary perms = asIOError ∘ openFileWriteNoTruncBinary' perms

openFileWriteExBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                        FileMode → γ → μ Handle
openFileWriteExBinary perms = asIOError ∘ openFileWriteExBinary' perms

openFileAppendBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                       (𝕄 FileMode) → γ → μ Handle
openFileAppendBinary perms = asIOError ∘ openFileAppendBinary' perms

----------------------------------------

openFileReadUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                   γ → μ Handle
openFileReadUTF8 = asIOError ∘ openFileReadUTF8'

openFileReadWriteUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                        (𝕄 FileMode) → γ → μ Handle
openFileReadWriteUTF8 perms = asIOError ∘ openFileReadWriteUTF8' perms

openFileReadWriteNoTruncUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ,
                                FileAs γ) ⇒
                               (𝕄 FileMode) → γ → μ Handle
openFileReadWriteNoTruncUTF8 perms =
  asIOError ∘ openFileReadWriteNoTruncUTF8' perms

openFileReadWriteExUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                          (𝕄 FileMode) → γ → μ Handle
openFileReadWriteExUTF8 perms = asIOError ∘ openFileReadWriteExUTF8' perms

openFileWriteUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                    (𝕄 FileMode) → γ → μ Handle
openFileWriteUTF8 perms = asIOError ∘ openFileWriteUTF8' perms

openFileWriteNoTruncUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                           (𝕄 FileMode) → γ → μ Handle
openFileWriteNoTruncUTF8 perms = asIOError ∘ openFileWriteNoTruncUTF8' perms

openFileWriteExUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                      FileMode → γ → μ Handle
openFileWriteExUTF8 perms = asIOError ∘ openFileWriteExUTF8' perms

openFileAppendUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                     (𝕄 FileMode) → γ → μ Handle
openFileAppendUTF8 perms = asIOError ∘ openFileAppendUTF8' perms

----------------------------------------

withFile ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
           TextEncoding → NewlineMode → IOMode → OpenFileFlags → (𝕄 FileMode)
         → γ → (Handle → IO ω) → μ ω
withFile enc nlm mode flags perms (review _File_ → fn) io = asIOError $
   bracket (openFile' enc nlm mode flags perms fn) System.IO.hClose io

--------------------

withFileME ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
             TextEncoding → NewlineMode → IOMode → OpenFileFlags
           → (𝕄 FileMode) → γ → (Handle → ExceptT ε IO ω) → μ ω
withFileME enc nlm mode flags perms fn io =
  join $ withFile enc nlm mode flags perms fn (ѥ ∘ io)

----------------------------------------

withFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                 IOMode → OpenFileFlags → (𝕄 FileMode) → γ → (Handle → IO ω)
               → μ ω
withFileUTF8 = withFile utf8 nativeNewlineMode

--------------------

withFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                   IOMode → OpenFileFlags → (𝕄 FileMode) → γ → (Handle → IO ω)
                 → μ ω
withFileBinary = withFile char8 noNewlineTranslation

----------------------------------------

withReadFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                   γ → (Handle → IO ω) → μ ω
withReadFileBinary = withFileBinary ReadMode readFlags Nothing

withReadWriteFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                          (𝕄 FileMode) → γ → (Handle → IO ω) → μ ω
withReadWriteFileBinary perms =
  withFileBinary ReadWriteMode readWriteFlags perms

withReadWriteNoTruncFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε,
                                  MonadError ε μ) ⇒
                                 (𝕄 FileMode) → γ → (Handle → IO ω) → μ ω
withReadWriteNoTruncFileBinary perms =
  withFileBinary ReadWriteMode readWriteNoTruncFlags perms

withReadWriteExFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                            FileMode → γ → (Handle → IO ω) → μ ω
withReadWriteExFileBinary perms =
  withFileBinary ReadWriteMode readWriteExFlags (Just perms)

withWriteFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                    (𝕄 FileMode) → γ → (Handle → IO ω) → μ ω
withWriteFileBinary perms = withFileBinary WriteMode writeFlags perms

withWriteNoTruncFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε,
                              MonadError ε μ) ⇒
                             (𝕄 FileMode) → γ → (Handle → IO ω) → μ ω
withWriteNoTruncFileBinary perms =
  withFileBinary WriteMode writeNoTruncFlags perms

withWriteExFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                        FileMode → γ → (Handle → IO ω) → μ ω
withWriteExFileBinary perms = withFileBinary WriteMode writeExFlags (Just perms)

withAppendFileBinary ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                       (𝕄 FileMode) → γ → (Handle → IO ω) → μ ω
withAppendFileBinary perms = withFileBinary AppendMode appendFlags perms

----------------------------------------

withReadFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                   γ → (Handle → IO ω) → μ ω
withReadFileUTF8 = withFileUTF8 ReadMode readFlags Nothing

withReadWriteFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                        (𝕄 FileMode) → γ → (Handle → IO ω) → μ ω
withReadWriteFileUTF8 perms = withFileUTF8 ReadWriteMode readWriteFlags perms

withReadWriteNoTruncFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε,
                                  MonadError ε μ) ⇒
                                 (𝕄 FileMode) → γ → (Handle → IO ω) → μ ω
withReadWriteNoTruncFileUTF8 perms =
  withFileUTF8 ReadWriteMode readWriteNoTruncFlags perms

withReadWriteExFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                          FileMode → γ → (Handle → IO ω) → μ ω
withReadWriteExFileUTF8 perms =
  withFileUTF8 ReadWriteMode readWriteExFlags (Just perms)

withWriteFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                    (𝕄 FileMode) → γ → (Handle → IO ω) → μ ω
withWriteFileUTF8 perms = withFileUTF8 WriteMode writeFlags perms

withWriteNoTruncFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε,
                            MonadError ε μ) ⇒
                           (𝕄 FileMode) → γ → (Handle → IO ω) → μ ω
withWriteNoTruncFileUTF8 perms = withFileUTF8 WriteMode writeNoTruncFlags perms

withWriteExFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                      FileMode → γ → (Handle → IO ω) → μ ω
withWriteExFileUTF8 perms = withFileUTF8 WriteMode writeExFlags (Just perms)

withAppendFileUTF8 ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ) ⇒
                       (𝕄 FileMode) → γ → (Handle → IO ω) → μ ω
withAppendFileUTF8 perms = withFileUTF8 AppendMode appendFlags perms

----------------------------------------

{- | Read a file in UTF8 encoding using OS-specific line-ending handling.
     Throw an exception on invalid character.
 -}
readFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒ γ → μ 𝕋
readFileUTF8 fn = withReadFileUTF8 fn TextIO.hGetContents

{- | Read a file as bytes. -}
readFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                 γ → μ ByteString
readFileBinary fn = withReadFileBinary fn BS.hGetContents

writeFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                  𝕄 FileMode → γ → 𝕋 → μ ()
writeFileBinary perms fn t =
  withWriteFileBinary perms fn (flip TextIO.hPutStr t)

{- | Write a file in UTF8 encoding using OS-specific line-ending handling.
     `perms`, if not Nothing, will be used to create the file if it doesn't
     exist.  If it does exist, `perms` has no impact (use `chmod` to really
     force it).  If `perms is Nothing, and the file does not exist, then an
     exception shall be thrown.
 -}
writeFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                𝕄 FileMode → γ → 𝕋 → μ ()
writeFileUTF8 perms fn t = withWriteFileUTF8 perms fn (flip TextIO.hPutStr t)

writeNoTruncFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                         𝕄 FileMode → γ → 𝕋 → μ ()
writeNoTruncFileBinary perms fn t =
  withWriteNoTruncFileBinary perms fn (flip TextIO.hPutStr t)

writeNoTruncFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                        𝕄 FileMode → γ → 𝕋 → μ ()
writeNoTruncFileUTF8 perms fn t =
  withWriteNoTruncFileUTF8 perms fn (flip TextIO.hPutStr t)

writeExFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                  FileMode → γ → 𝕋 → μ ()
writeExFileBinary perms fn t =
  withWriteExFileBinary perms fn (flip TextIO.hPutStr t)

writeExFileUTF8 ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
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

appendFileBinary ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒
                𝕄 FileMode → γ → 𝕋 → μ ()
appendFileBinary perms fn t =
  withFileBinary AppendMode appendFlags perms fn (flip TextIO.hPutStr t)

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

unlink ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒ γ → μ ()
unlink (review _File_ → fn) = asIOError $ removeLink (fn ⫥ filepath)

chmod ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs γ) ⇒ FileMode → γ → μ ()
chmod perms (review _File_ → fn) = asIOError $ setFileMode (fn ⫥ filepath) perms

----------------------------------------

{- | Read a file in UTF8 encoding using OS-specific line-ending handling.
     Replace any invalid input bytes with the Unicode replacement character
     U+FFFD.
-}
-- plagiarized from https://www.snoyman.com/blog/2016/12/beware-of-readfile
readFileUTF8Lenient ∷ (AsIOError ε, MonadError ε μ, MonadIO μ, FileAs γ) ⇒
                      γ → μ 𝕋
readFileUTF8Lenient fn = decodeUtf8With lenientDecode ⊳ readFileBinary fn

----------------------------------------

hClose ∷ ∀ ε μ . (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒ Handle → μ ()
hClose = asIOError ∘ System.IO.hClose

-- fileAccess ----------------------------------------------

data AccessMode = ACCESS_R | ACCESS_WX | ACCESS_RWX
                | ACCESS_W | ACCESS_RX
                | ACCESS_X | ACCESS_RW

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
                  γ → 𝕄 FileStatus -> μ (𝕄 𝕋)

_isWritableFile (review _File_ → f) st =
  let rJust = return ∘ Just
   in case st of
        Nothing  → rJust $ [fmt|%T does not exist|] f
        Just stp → if isDirectory stp
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
isWritableDir ∷ ∀ α ε μ .
                (MonadIO μ, IsDir α, AsFilePath α, MonadError ε μ, AsIOError ε)⇒
                α -> μ (𝕄 𝕋)

isWritableDir d =
  let rJust = return ∘ Just
   in stat d ≫ \ case
        Nothing  → rJust $ [fmt|%T does not exist|] d
        Just stp → if isDirectory stp
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

tests ∷ TestTree
tests = testGroup "MonadIO.File" [ fexistsTests, fexists'Tests, statTests
                                 , isWritableDirTests, fileWritableTests
                                 , withFileTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------

