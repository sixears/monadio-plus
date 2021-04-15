{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module MonadIO.File
  ( AccessMode(..), FExists(..), System.IO.IOMode(..)
  , hClose

  , devnull

  , access, lstat, stat, writable, fexists, fexists', lfexists, lfexists'

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
  )
where

import Debug.Trace  ( trace, traceShow )
import Prelude  ( error, show, undefined )

-- base --------------------------------

import qualified  System.IO

import Control.Monad           ( Monad, (>=>), filterM, forM_, join, return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bool               ( Bool( False, True ), bool )
import Data.Either             ( Either( Left, Right ) )
import Data.Eq                 ( Eq )
import Data.Function           ( ($), (&), const, flip, id )
import Data.Functor            ( fmap )
import Data.IORef              ( IORef, newIORef, readIORef )
import Data.List               ( dropWhileEnd, head, init, intercalate, isSuffixOf, last, or, reverse, scanl, scanr, tail, zip )
import Data.Maybe              ( Maybe( Just, Nothing ), isJust )
import Data.String             ( String )
import Data.Tuple              ( fst )
import GHC.Exts                ( toList )
import System.Environment      ( getProgName )
import System.Exit             ( ExitCode )
import System.IO               ( FilePath, Handle, IO, NewlineMode, TextEncoding
                               , IOMode( AppendMode, ReadMode, ReadWriteMode
                                       , WriteMode )
                               , char8, hIsEOF, hSetEncoding, hSetNewlineMode
                               , nativeNewlineMode, noNewlineTranslation, utf8
                               )
import System.IO.Error         ( illegalOperationErrorType, mkIOError )
import System.Posix.Types      ( FileMode )
import Text.Show               ( Show )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode      ( (∧), (∨) )
import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.List.Unicode      ( (∈) )
import Data.Monoid.Unicode    ( (⊕) )

-- bytestring --------------------------

import qualified Data.ByteString  as  BS

import Data.ByteString  ( ByteString )

-- containers --------------------------

import Data.Sequence  ( Seq( Empty ), breakr, fromList )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString, toText )

-- directory ---------------------------

import System.Directory  ( createDirectory, removePathForcibly
                         , setCurrentDirectory, withCurrentDirectory )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadMask, bracket, onException )

-- filepath ----------------------------

import System.FilePath  ( FilePath, (</>), splitPath )

-- fpath -------------------------------

import FPath.Abs               ( Abs( AbsD, AbsF ) )
import FPath.AbsDir            ( AbsDir, NonRootAbsDir, absdir, root )
import FPath.AbsFile           ( AbsFile, absfile, absfileT )
import FPath.AppendableFPath   ( (⫻) )
import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.AsFilePath'       ( exterminate )
import FPath.Basename          ( basename )
import FPath.Dir               ( Dir, DirAs( _Dir_ ) )
import FPath.Dirname           ( dirname )
import FPath.Error.FPathError  ( AsFPathError, FPathIOError
                               , __FPathEmptyE__, __FPathNotAFileE__ )
import FPath.File              ( AsFile( _File ), File( FileR ), FileAs( _File_ ) )
import FPath.FPath             ( FPath( FAbsD, FAbsF, FRelD, FRelF ) )
import FPath.IO                ( getCwd, pResolve, pResolveDir )
import FPath.Parent            ( parent )
import FPath.Parseable         ( Parseable, parse )
import FPath.Rel               ( AsRel( _Rel ), Rel )
import FPath.RelDir            ( AsRelDir( _RelDir ), RelDir, reldir )
import FPath.RelFile           ( AsRelFile( _RelFile ), RelFile, relfile )

-- fstat -------------------------------

import FStat  ( FStat, FileType( Directory, SymbolicLink ), ftype, mkfstat )

-- lens --------------------------------

import Control.Lens.Prism   ( Prism', prism )
import Control.Lens.Review  ( (#), review )

import qualified System.FilePath.Lens

-- monadio-error -----------------------

import MonadError           ( ѥ, splitMError )
import MonadError.IO        ( ӝ, asIOError, asIOErrorY )
import MonadError.IO.Error  ( AsIOError, IOError, (~~)
                            , _IOErr, squashInappropriateTypeT )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool     ( 𝔹 )
import Data.MoreUnicode.Functor  ( (⊳), (⊳⊳), (⊳⊳⊳), (⩺) )
import Data.MoreUnicode.Lens     ( (⊣), (⫥), (⊢) )
import Data.MoreUnicode.Maybe    ( 𝕄 )
import Data.MoreUnicode.Monad    ( (≫), (⪼) )
import Data.MoreUnicode.Natural  ( ℕ )
import Data.MoreUnicode.String   ( 𝕊 )
import Data.MoreUnicode.Text     ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError, throwError )

-- safe --------------------------------

import Safe  ( headMay, lastDef, lastMay )

-- split -------------------------------

import Data.List.Split  ( wordsBy )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup, withResource )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), assertIsLeft, assertRight, runTestsP, runTestsReplay
                  , runTestTree )

-- temporary ---------------------------

import qualified System.IO.Temp

import System.IO.Temp  ( createTempDirectory, getCanonicalTemporaryDirectory
                       , withSystemTempDirectory, withTempDirectory
                       )

-- text --------------------------------

import qualified  Data.Text.IO  as  TextIO

import Data.Text                 ( Text, drop, length, pack )
import Data.Text.Encoding        ( decodeUtf8With )
import Data.Text.Encoding.Error  ( lenientDecode )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- time --------------------------------

import Data.Time.Clock  ( getCurrentTime )

-- unix --------------------------------

import qualified  System.Posix.Files  as  Files
import System.Posix.Files  ( FileStatus, fileExist, getFileStatus, getSymbolicLinkStatus
                           , readSymbolicLink, removeLink, setFileMode )
import System.Posix.IO     ( OpenFileFlags( OpenFileFlags, append, exclusive
                                          , noctty, nonBlock, trunc ),
                             OpenMode( ReadOnly, ReadWrite, WriteOnly )
                           , fdToHandle, noctty, nonBlock, openFd
                           )

--------------------------------------------------------------------------------

data FExists = FExists | NoFExists
  deriving (Eq,Show)

{- | Does this 𝕄 FStat refer to a directory? -}
mIsDir ∷ 𝕄 FStat → 𝔹
mIsDir (fmap ftype → Just Directory) = True
mIsDir _                             = False

fexists_ ∷ (Monad η, AsFilePath ρ) ⇒ 𝔹 → (ρ → η (𝕄 FStat)) → ρ → η FExists
fexists_ checkDir g f = bool NoFExists FExists ⊳ do
  s ← g f
  if checkDir ∧ '/' ≡ lastDef '\0' (f ⫥ filepath)
  then return (mIsDir s)
  else return (isJust s)

{- | Does file exist.  Note that "does /etc/passwd/ exist?", where /etc/passwd
     exists but is a file, will return `NoFExists`; but "does /etc exist?" where
     /etc exists but is a directory will return `FExists`.  See also `fexists'`.

     Symlinks are dereferenced; so dangling symlinks are considered to not
     exist.
 -}
fexists ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath τ) ⇒ τ → μ FExists
fexists = fexists_ True stat

{- | Like `fexists`; but for symlinks, checks the symlink rather than
     dereferencing; so dangling symlinks are considered to exist. -}
lfexists ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath τ) ⇒
           τ → μ FExists
lfexists = fexists_ True lstat

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
fexists' = fexists_ False stat

lfexists' ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath τ) ⇒
            τ → μ FExists
lfexists' = fexists_ False lstat

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

{- | File stat; returns Nothing if file does not exist.  Note that `stat`-ing
     a "directory" that is really a file (e.g., `/etc/passwd/`) will just stat
     the file (`/etc/passwd` in our example).
  -}
_stat ∷ ∀ ε ρ μ . (MonadIO μ, AsFilePath ρ, AsIOError ε, MonadError ε μ) ⇒
        (FilePath → IO FileStatus) → ρ → μ (𝕄 FStat)
_stat s fn = do
  let fp = exterminate $ fn ⫥ filepath
   in join ⊳⊳ squashInappropriateTypeT ∘ asIOErrorY ∘ fmap mkfstat ∘ s $ fp

-- | file stat; returns Nothing if file does not exist
stat ∷ ∀ ε ρ μ . (MonadIO μ, AsFilePath ρ, AsIOError ε, MonadError ε μ) ⇒
       ρ → μ (𝕄 FStat)
stat = _stat getFileStatus

----------------------------------------

{- | File stat; returns Nothing if file does not exist.  If the file is a
     symlink, return the stat of the symlink; cf. `stat`, which looks "through"
     the symlink to the file itself.
 -}
lstat ∷ ∀ ε ρ μ . (MonadIO μ, AsFilePath ρ, AsIOError ε, MonadError ε μ) ⇒
        ρ → μ (𝕄 FStat)
lstat = _stat getSymbolicLinkStatus

----------

statTests ∷ TestTree
statTests =
  let testStat expect input f =
        testCase (toString input) $
          f (ѥ @IOError (stat input)) ≫ assertRight (expect @=?)
      isDirectory = ((Directory ≡) ∘ ftype)
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

chmod ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath ρ) ⇒
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

----------------------------------------

hClose ∷ ∀ ε μ . (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒ Handle → μ ()
hClose = asIOError ∘ System.IO.hClose

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

class RelAs α where
  _Rel_ ∷ Prism' Rel α
instance RelAs Rel where
  _Rel_ = id
instance RelAs RelFile where
  _Rel_ = _RelFile
instance RelAs RelDir where
  _Rel_ = _RelDir

{- | Take some IO action, in a context which has other exceptions; catch any IO
     exceptions, and `join` them with the context exceptions. -}
asIOErrorT ∷ (MonadIO μ, AsIOError ε, MonadError ε μ) ⇒ ExceptT ε IO α → μ α
asIOErrorT = join ∘ asIOError ∘ splitMError

_parseD ∷ (Parseable χ, AsFPathError ε, MonadError ε η) ⇒ FilePath → η χ
_parseD = parse ∘ (⊕ "/") ∘ dropWhileEnd (≡ '/')

parseDir ∷ (AsFPathError ε, MonadError ε η) ⇒ FilePath → η Dir
-- parseDir = parse ∘ (⊕ "/") ∘ dropWhileEnd (≡ '/')
parseDir = _parseD @Dir

parseAbsDir ∷ (AsFPathError ε, MonadError ε η) ⇒ FilePath → η AbsDir
parseAbsDir = parse ∘ (⊕ "/") ∘ dropWhileEnd (≡ '/')

-- In practice, withSystemTempDirectory always gives us an AbsDir (even if
-- $TMPDIR is relative); if $TMPDIR doesn't exist or is not a dir, it will fail.

{- | Perform some IO with a given temporary directory, created within some given
     dir; the temporary dir is removed once IO is complete.  The directory
     created is passed into the IO as an `AbsDir`.  The directory name is
     prefixed by some relative name.
 -}
withTempDir'' ∷ (MonadIO μ, MonadMask μ, AsFPathError ε, AsIOError ε,
                 MonadError ε μ, DirAs δ, Parseable δ, RelAs ρ) ⇒
                δ → ρ → (δ → ExceptT ε IO α) → μ α
withTempDir'' d (review $ filepath ∘ _Rel_ → r) io =
  asIOErrorT $ withTempDirectory (d ⫥ filepath ∘ _Dir_) r (_parseD >=> io)

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

{- | Perform some IO with a given temporary file, created within some given dir;
     the temporary file is removed once IO is complete.  The file created is
     passed into the IO as an `AbsFile`.  The directory name is prefixed by some
     relative name.
 -}
withTempFile'' ∷ (MonadIO μ, MonadMask μ,
                  AsFPathError ε, AsIOError ε, MonadError ε μ,
                  DirAs δ, Parseable (MonadIO.File.FileType δ), RelAs ρ) ⇒
                 δ → ρ → (MonadIO.File.FileType δ → Handle → ExceptT ε IO α)
               → μ α
withTempFile'' d (review $ filepath ∘ _Rel_ → r) io =
  let doFile f h = parse f ≫ \ f' → io f' h
   in asIOErrorT $ System.IO.Temp.withTempFile (d ⫥ filepath ∘ _Dir_) r doFile


{- | Like `withTempFile''`, but uses the system temp dir (see `tempdir`). -}
withTempFile' ∷ (MonadIO μ, MonadMask μ,
                 AsFPathError ε, AsIOError ε, MonadError ε μ, RelAs ρ) ⇒
                ρ → (AbsFile → Handle → ExceptT ε IO α) → μ α
withTempFile' r io = tempdir ≫ \ d → withTempFile'' d r io

{- | Like `withTempFile''`, but uses the system temp dir (see `tempdir`). -}
withTempFile ∷ (MonadIO μ, MonadMask μ,
                AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒
               (AbsFile → Handle → ExceptT ε IO α) → μ α
withTempFile io = progNamePrefix ≫ \ p → withTempFile' p io

{- | Perform IO with the dir *temporarily* changed to a given directory. -}
inDir ∷ (MonadIO μ, DirAs δ, AsIOError ε, MonadError ε μ) ⇒
         δ → ExceptT ε IO α → μ α
inDir (review $ filepath ∘ _Dir_ → d) io =
  join ∘ asIOError $ withCurrentDirectory d (ѥ io)

----------

{- | Perform some IO with a temporary directory (created in the system temp
     directory, see `tempdir`), which is removed once IO is complete.  The
     directory created is passed into the IO as an `AbsDir`.  The directory name
     is prefixed by some relative name.
 -}
withTempDir' ∷ (MonadIO μ, MonadMask μ, AsFPathError ε, AsIOError ε,
                MonadError ε μ, RelAs ρ) ⇒
               ρ → (AbsDir → ExceptT ε IO α) → μ α
withTempDir' r io = tempdir ≫ \ d → withTempDir'' d r io

{- | A prefix (suitable for, e.g., temp files or dirs) in the form of a
     `RelFile` (which is the programme name, plus a '-' character. -}
progNamePrefix ∷ (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒
                 μ RelFile
progNamePrefix = asIOError getProgName ≫ parse ∘ (⊕ "-")

{- | Like `withTempDir'`, with the prefix being the program name plus `"-"`. -}
withTempDir ∷ (MonadIO μ, MonadMask μ, AsFPathError ε, AsIOError ε,
               MonadError ε μ) ⇒
              (AbsDir → ExceptT ε IO α) → μ α
withTempDir io = progNamePrefix ≫ \ p → withTempDir' p io

{- | Like `withTempDir`, but temporarily changes dir into the temporary
     directory, rather than passing the dir name to the IO. -}
withTempDirCD ∷ (MonadIO μ, MonadMask μ, AsFPathError ε, AsIOError ε,
                 MonadError ε μ) ⇒
                ExceptT ε IO α → μ α
withTempDirCD io = withTempDir (flip inDir io)

{- | Like `withTempDir`, but temporarily changes dir into the temporary
     directory, as well as passing the dir name to the IO. -}
withTempDirCD' ∷ (MonadIO μ, MonadMask μ, AsFPathError ε, AsIOError ε,
                  MonadError ε μ) ⇒
                 (AbsDir → ExceptT ε IO α) → μ α
withTempDirCD' io = withTempDir (\ d → inDir d $ io d)

----------------------------------------

{- | Write a temporary file, with contents, using a given encoding, newline-mode
     and writer function. -}
writeTempFile ∷ (MonadIO μ, MonadMask μ,
                 AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒
                TextEncoding → NewlineMode → (Handle → τ → IO ()) → τ
              → μ AbsFile
writeTempFile enc nlm wrt t = withTempFile $ \ tempfn h → do
  liftIO $ do
    hSetEncoding h enc
    hSetNewlineMode h nlm
    wrt h t
  hClose h
  return tempfn

----------

{- | Write a temporary file with UTF8 contents. -}
writeTempFileUTF8 ∷ (MonadIO μ, MonadMask μ,
                     AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒
                    Text → μ AbsFile
writeTempFileUTF8   = writeTempFile utf8 nativeNewlineMode TextIO.hPutStr

----------

{- | Write a temporary file with binary contents. -}
writeTempFileBinary ∷ (MonadIO μ, MonadMask μ,
                       AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒
                      ByteString → μ AbsFile
writeTempFileBinary = writeTempFile char8 noNewlineTranslation BS.hPutStr

----------------------------------------

mkdir ∷ ∀ ε δ μ . (MonadIO μ, AsIOError ε, MonadError ε μ, DirAs δ) ⇒ δ → μ ()
mkdir = asIOError ∘ createDirectory ∘ (review $ filepath ∘ _Dir_)

{- | Get the system temporary directory (TMPDIR, etc.) -}
tempdir ∷ (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒ μ AbsDir
tempdir = asIOError getCanonicalTemporaryDirectory ≫ parseAbsDir

{- | Create a temporary directory as a subdir of a given dir; return its name.
     It is the responsibility of the caller to arrange appropriate cleanup. -}
mkTempDir'' ∷ ∀ ε δ ρ μ .
              (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ, RelAs ρ,
               DirAs δ) ⇒
              δ → ρ → μ AbsDir
mkTempDir'' t (review filepath ∘ review _Rel_ → r) = do
  d ← liftIO $ createTempDirectory (t ⫥ (filepath ∘ _Dir_)) r
  parseAbsDir d

{- | `mkTempDir''`, but create a dir in the system temp dir. -}
mkTempDir' ∷ ∀ ε ρ μ .
             (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ, RelAs ρ) ⇒
             ρ → μ AbsDir
mkTempDir' r = tempdir ≫ \ d → mkTempDir'' d r

{- | `mkTempDir'`, with the prefix being the program name plus `"-"`. -}
mkTempDir ∷ ∀ ε μ .
            (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒ μ AbsDir
mkTempDir = progNamePrefix ≫ mkTempDir'

nuke ∷ ∀ ε ρ μ . (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath ρ) ⇒
       ρ → μ ()
nuke (review filepath → fp) = asIOError $ removePathForcibly fp

instance FileAs RelFile where
  _File_ = prism FileR (\ case (FileR r) → Right r; f → Left f)

{- | For complex tests that need an IO setup; where having acquired the
     resource, the setup may fail; regular `withResource` doesn't clean that up
     (since the acquire step throws an exception, the return value is never seen
     and so cannot be passed to the release); this version splits acquisition
     and setup.

     If acquisition fails, there should be nothing to release.
     But if setup fails, the release is called (though of course no tests are
     run).
     If the setup succeeds, tests are run, and cleanup is called.
 -}
withResourceCleanup ∷ IO α → (α → IO ()) → (α → IO ()) → (IO α → TestTree)
                    → TestTree
withResourceCleanup acquire setup release test =
  let -- safely acquire and run setup; if setup throws an IOException, release
      -- acquireAndSetup ∷ IO α
      acquireAndSetup = acquire ≫ \ resource → do
        onException (setup resource) (release resource)
        return resource
   in withResource acquireAndSetup release test

withResourceCleanup' ∷ IO α → (α → IO ()) → (IORef α → IO ())
                    → (IO (IORef α) → TestTree) → TestTree
withResourceCleanup' acquire setup release test =
  let -- safely acquire and run setup; if setup throws an IOException, release
      -- acquireAndSetup ∷ IO α
      acquireAndSetup = acquire ≫ \ resource → do
        resourceRef ← newIORef resource
        onException (setup resource) (release resourceRef)
        return resourceRef
   in withResource acquireAndSetup release test

readlinkTests ∷ TestTree
readlinkTests =
  let absD = const ∘ AbsD
      absF = const ∘ AbsF
      relD ∷ RelDir → AbsDir → Abs
      relD f = \ t → AbsD $ t ⫻ f
      relDp ∷ RelDir → AbsDir → Abs
      relDp d = \ t → AbsD $ (t ⊣ dirname) ⫻ d
      relF ∷ RelFile → AbsDir → Abs
      relF f = \ t → AbsF $ t ⫻ f
      relFp ∷ RelFile → AbsDir → Abs
      relFp f = \ t → AbsF $ (t ⊣ dirname) ⫻ f
      testlinks ∷ [(FilePath,FilePath,AbsDir → Abs)]
      testlinks =
        [ -- ("dangle-relfile" , "nonesuch"     , FRelF [relfile|nonesuch|])
--        , ("dangle-reldir"  , "nonesuch/"    , FRelD [reldir|nonesuch/|])
--        ,
          -- name of symlink ⫽ symlink target ⫽ singly-resolved target
          ("dangle-absfile" , "/nonesuch"    , absF [absfile|/nonesuch|])
        , ("dangle-absdir"  , "/nonesuch/"   , absD [absdir|/nonesuch/|])
        , ("slash"          , "/"            , absD root)
        , ("slashes"        , "///"          , absD root)
        , ("etc"            , "/etc/"        , absD [absdir|/etc/|])
        , ("etcf"           , "/etc"         , absF [absfile|/etc|])
        , ("passwd"         , "/etc/passwd"  , absF [absfile|/etc/passwd|])
        , ("passwd-dir"     , "/etc/passwd/" , absD [absdir|/etc/passwd/|])
        , ("parent-dangle"  , "../nonesuch"  , relFp [relfile|nonesuch|])
        -- this results in a dir, because '.' is always a dir
        , ("this"           , "."            , relD [reldir|./|])
        , ("this-dir"       , "./"           , relD [reldir|./|])
        , ("this-dirs"      , ".///"         , relD [reldir|./|])
        , ("this-this-this" , "././."        , relD [reldir|./|])
        -- this results in a dir, because '..' is always a dir
        , ("parent"         , ".."           , relDp [reldir|./|])
        , ("parent-dir"     , "../"          , relDp [reldir|./|])
        , ("this-parent"    , "./.."         , relDp [reldir|./|])
        , ("parent-this"    , "../."         , relDp [reldir|./|])
        , ("plainfile"      , "plain"        , relF  [relfile|plain|])
        , ("dir"            , "directory"    , relF  [relfile|directory|])
        , ("dir-dir"        , "directory/"   , relD  [reldir|directory/|])

        , ("dr-prnt-dr" , "dir/../dir/"             , relD [reldir|directory/|])
        , ("dr-prnt-dy" , "dir/../directory/"       , relD [reldir|directory/|])
        , ("dy-prnt-dr" , "directory/../dir/"       , relD [reldir|directory/|])
        , ("dy-prnt-dy" , "directory/../directory/" , relD [reldir|directory/|])
        
        , ("dr-prnt-dr-p"   , "dir/../dir/p" , relF  [relfile|directory/p|])
        , ("dy-prnt-dr-p"   , "directory/../dir/p" ,
           relF  [relfile|directory/p|])
        , ("dr-prnt-dy-p"   , "dir/../directory/p" ,
           relF  [relfile|directory/p|])
        , ("dy-prnt-dy-p"   , "directory/../directory/p" ,
           relF  [relfile|directory/p|])
        ]

      mkTempDir' ∷ MonadIO μ ⇒ μ AbsDir
      mkTempDir' = ӝ $ mkTempDir @FPathIOError

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
          withCurrentDirectory (d ⫥ filepath) $ do
            -- create a plain file 'plain', with current time as contents
            writeTime [relfile|plain|]
            -- create a directory, 'directory'
            ӝ $ mkdir @IOError @AbsDir (d ⫻ [reldir|directory/|])
            -- create all the symlinks cited in `testlinks`
            forM_ testlinks (\ (from,to,_) → Files.createSymbolicLink to from)

      -- We factor this out so it can be run on exception during creation;
      -- `withResource` does not run the resource-close step if there was an
      -- IOException during the resource-acquisition step
      delTemp ∷ AbsDir → IO ()
      delTemp = ӝ ∘ nuke @FPathIOError
    in withResourceCleanup mkTempDir' (ӝ ∘ populateTemp) delTemp $
      \ tmpdir →
      let readlink' ∷ 𝕊 → IO (Either FPathIOError Abs)
          readlink' = ѥ ∘ readlink
          check ∷ 𝕊 → (AbsDir → Abs) → TestTree
          check fn exp = let path t = toString t ⊕ "/" ⊕ fn
                          in testCase fn $ tmpdir ≫ \ t →
                               readlink' (path t) ≫ assertRight (exp t ≟)
       in testGroup "readlinkTests"
                    [ check name fpatht | (name,_,fpatht) ← testlinks ]

-- note that withTempDirectory will give us a relative dir if passed a relative
-- dir (that exists and is usable)

----------------------------------------

{- | Perform IO within a directory, with declared errors. -}
_inDir ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, Printable τ) ⇒ τ → IO α → μ α
_inDir (toString → d) io =
  -- ensure that the path is attached to the error
  (ѥ ∘ asIOError $ withCurrentDirectory d io) ≫ \ case
    Left e' → join $ throwError (e' ~~ d)-- (ioEWithPath d e')
    Right r → return r

{- | like `inDirT`, but takes IO that already throws some error(s). -}
_inDirT ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, Printable τ) ⇒
          τ → ExceptT ε IO α → μ α
_inDirT d io = join $ _inDir d (ѥ io)

scan ∷ (α → α → α) → α → [α] → [(α,α)]
scan f b xs = zip (scanl f b xs) (scanr f b xs)

{- | List of splits of a file path in two, at each directory along the path;
     e.g.,

     @
       λ> splitPoints "foo/bar/"
       [("","foo/bar"),("foo/","bar"),("foo/bar","")]
     @

     Note that any trailing '/' is dropped.
 -}
-- FIX IS HERE FIX IS HERE FIX IS HERE
-- ADD TESTS TO PRESOLVABLE ABSDIR & ABS for /// and .///, etc.
splitPoints ∷ FilePath → [(FilePath,FilePath)]
splitPoints f =
  let noSlash ∷ FilePath → FilePath
      noSlash p = let noSlash' ∷ FilePath → FilePath
                      noSlash' "/" = "/"
                      noSlash' t = dropWhileEnd (≡ '/') t
                      -- protect against //…
                   in case noSlash' p of
                        "" → "/"
                        _  → p
   in scan (</>) "" (splitPath $ noSlash f)

{- | Given an absdir and a subsequent filepath (which might be absolute),
     return a pair of the initial filepath that exists, *resolved* (symlinks,
     ., .., all resolved); and a part that does not exist.  Note that the
     extant part must have read-and-execute permission for the user; else an
     `AsIOError` will be raised.
 -}
resolve ∷ (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒
          AbsDir → FilePath → μ (FilePath, FilePath)
resolve d fp = traceShow ("resolve",d,fp) $
  let -- prepend `d`, note this is a no-op for input abs functions
      prepend ∷ FilePath → FilePath
      prepend = (filepath # d </>)

-- FIX IS HERE FIX IS HERE FIX IS HERE
-- Move stat, fexists to FPath.IO?  → Move FPath.IO to MonadIO? *DEPRECATE NOT REMOVE* ←
      _stat ∷ ∀ ε ρ μ . (MonadIO μ, AsFilePath ρ, AsIOError ε, MonadError ε μ) ⇒
              (FilePath → IO FileStatus) → ρ → μ (𝕄 FStat)
      _stat s fn = do
        let p = exterminate $ fn ⫥ filepath
         in join ⊳⊳ squashInappropriateTypeT ∘ asIOErrorY ∘ fmap mkfstat ∘ s $ p

      stat ∷ ∀ ε ρ μ . (MonadIO μ, AsFilePath ρ, AsIOError ε, MonadError ε μ) ⇒
             ρ → μ (𝕄 FStat)
      stat = _stat getFileStatus
      
      fexist_ ∷ (Monad η, AsFilePath ρ) ⇒ (ρ → η (𝕄 FStat)) → ρ → η 𝔹
      fexist_ g f = do
        s ← g f
        if '/' ≡ lastDef '\0' (f ⫥ filepath)
        then return (mIsDir s)
        else return (isJust s)
      fexist ∷ (MonadIO μ, AsIOError ε, MonadError ε μ) ⇒ FilePath → μ Bool
--      fexist  = asIOError ∘ fileExist
      fexist = fexist_ stat
      -- Given an AbsDir, `resolve` must resolve to *something* valid, since the
      -- top of an AbsDir is the root dir, and that always exists.
   in (head ⩺ filterM (fexist ∘ fst) ∘ reverse ∘ splitPoints ∘ prepend) fp

_pResolveDir ∷ (Printable τ, AsIOError ε, AsFPathError ε, MonadError ε μ,
                MonadIO μ) ⇒
               AbsDir → τ → μ AbsDir
_pResolveDir d (toString → p) = traceShow ("_pResolveDir", d, p) $ do
  (extant,non_extant) ← trace "resolved" $ resolve d p
  d' ← _inDirT extant getCwd
  let -- add a trailing / so reldir parses it
      toDir "" = "./"
-- FIX IS HERE FIX IS HERE FIX IS HERE
-- ADD TESTS TO pResolveDir FOR FilePath with trailing '/' (or two) 
      toDir t  = case last t of -- last is safe, t is non-empty
                   '/' → t
                   _   → t ⊕ "/"
  p' ← traceShow ("-> parse", non_extant, toDir non_extant) $ parse @RelDir (toDir non_extant)
  traceShow ("-> return", d', p') $ return $ d' ⫻ p'

_pResolveDirF ∷ (Printable τ, AsIOError ε, AsFPathError ε, MonadError ε μ,
                 MonadIO μ)⇒
                AbsDir → τ → μ AbsFile
_pResolveDirF d (toString → f) = traceShow ("_pResolveDirF", toText d, f) $ 
    -- we can't simply use parseRelFile, etc., here, as we want to accept
    -- paths with '..' and '.' in them (and resolve them)
    case breakr (≡ '/') $ fromList f of
      -- first element of tuple is suffix of seq (a little counterintuitively)
      (Empty, Empty) → traceShow("EE") $ -- f was empty
                       __FPathEmptyE__    absfileT
      (Empty, _    ) → traceShow("E_") $ -- f had a trailing /
                       __FPathNotAFileE__ absfileT (toText f)


      (_, Empty    ) → traceShow("_E",d,f) $ -- just a file, no dir part
                       do c {- ∷ AbsDir -} ← pResolveDir d ("."∷Text)
                          (c ⫻) ⊳ parse f

      (x    , y    ) → traceShow("xy") $ -- dir + file
                       do c ← pResolveDir d (toList y)
                          (c ⫻) ⊳ parse (toList x)


-- This has to return an absolute path, as the relative path might include
-- many '..' that can't be represented by FPath.  So we resolve it.
{- | Read a symlink, return the absolute path to the referent. -}
readlink ∷ ∀ ε γ μ .
            (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ,
             AsFilePath γ) ⇒
            γ → μ Abs
readlink (review filepath → fp) = traceShow ("readlink", fp) $ do
  r ← asIOError $ readSymbolicLink fp
  case headMay r of
    Nothing  → error $ [fmt|empty symlink found at '%s'|] fp
    Just '/' → -- last is safe, as fp is non-empty, given that headMay fp
               -- is not Nothing
               traceShow ("abs",fp,r) $ case last r of
                 '/' → AbsD ⊳ _pResolveDir root r
                 _   → AbsF ⊳ _pResolveDirF root r
    Just _   → do d ← pResolve (fp ⊣ System.FilePath.Lens.directory)
                   -- last is safe, as fp is non-empty, given that headMay fp
                   -- is not Nothing
                  traceShow ("rel",fp,d,r) $ case last r of
                    '/' → AbsD ⊳ _pResolveDir d r
                    _   → if or [ r ∈ [ ".", ".." ]
                                , "/." `isSuffixOf` r
                                , "/.." `isSuffixOf` r
                                ]
                          then AbsD ⊳ _pResolveDir d r
                          else AbsF ⊳ _pResolveDirF d r


{- | Read a single symbolic link; will error if the given input is not a
     symlink. -}
readlink' ∷ (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ,
             AsFilePath γ) ⇒
           γ → μ FPath
-- readlink (review filepath → f) = join ∘ asIOError $ parse ⊳ readSymbolicLink f
readlink' (review filepath → f) = do
  p ← asIOError $ readSymbolicLink f
  let msg        = "readlink returned empty path"
      emptyPathE = mkIOError illegalOperationErrorType msg Nothing (Just f)
  case headMay p of
    Nothing  → throwError $ _IOErr # emptyPathE
    Just '/' → let parts = wordsBy (≡'/') p -- drops delims and ignores //, etc.
                in case ".." ∈ parts of
                     False → -- p is absolute, and without ..; return as is
                             -- (having dumped any /.(/|$)) .
                             case lastMay p of
                               -- if parts is empty, it must be / (or //…);
                               -- so is covered by Just /
                               Nothing  → throwError $ _IOErr # emptyPathE
                               Just '/' → parse @FPath $ intercalate "/" ("":parts⊕[""])
                               Just _   → parse @FPath $ intercalate "/" ("":parts)
{-
                     True  → -- p is absolute, but with ..; split the dir &
                             -- basename; cd to the dir; and tack the basename
                             -- on.
                             -- Note that basename, etc., (from
                             -- System.Filepath) don't work well here because
                             -- with (say) /foo/bar/, the basename is
                             -- considered to be "", which is not what we want.
                             let dir      = intercalate "/" $ init parts
                                 -- tail must succeed: parts cannot be empty as
                                 -- it contains at least '..'
                                 basename = tail parts
                              in withCurrentDirectory getCurrentDirectory
                             -- consider .. at the end
                             -- consider '/' at end of basename (p)
-}
    Just x → let parts = wordsBy (≡'/') p -- drops delims and ignores //, etc.
                in case ".." ∈ parts of
                     False → -- p is relative, and without ..; return as is
                             -- (having dumped any /.(/|$)) .
                             -- since head of p is not /, parts cannot be empty
                             case last p of
                               '/' → parse @FPath $ intercalate "/" (parts⊕[""])
                               _   → parse @FPath $ intercalate "/" parts
  -- If p is relative, and neither p nor f have any ..; then append p to the
  --   dirname of f, and return (having dumped any (^|/).(/|$))
  -- Else p is relative, and (dirname f + p) have ..; cd to (dirname p + dirname
  --   f), and tack the basename on

--------------------

{- | Recursively read a symbolic link, until it is a symbolic link no more.
     Anything other than a symbolic link is immediately returned intact
     (including non-existent files).  A symbolic link is deferenced, and the
     result re-examined.
 -}
-- return nonexistent FilePath if any part does not exist: this is needed
-- because any part along the path might not exist; and if so, it may be
-- pointed at by a path, e.g., "./../foo"; which cannot be represented by FPath
{-
resolvelink ∷ (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ,
               AsFilePath γ, Printable γ) ⇒
              γ → μ FPath
resolvelink f =
  lstat f ≫ \ case
    Just (ftype → SymbolicLink) → do -- readlink f ≫ resolvelink
      r ← readlink f
      case r of
        FRelF rf → parse f ≫ \ case
--                     FAbsF af → error $ show (toString $ af ⊣ dirname,toString rf)
                     FAbsF af → error $ show (toText $ FAbsF (af ⊣ dirname ⫻ rf))
                     _        → error $ "wrong type"
        _        → resolvelink r
-}
{-
    _                           → do
      r ← readlink f
      case r of
        FRelF rf → error $ show rf
        _        → resolvelink r
-}

----------------------------------------

tests ∷ TestTree
tests = testGroup "MonadIO.File" [ fexistsTests, fexists'Tests, statTests
                                 , isWritableDirTests, fileWritableTests
                                 , withFileTests, readlinkTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------

