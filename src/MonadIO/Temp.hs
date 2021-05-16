{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE UnicodeSyntax    #-}
{-# LANGUAGE ViewPatterns     #-}

module MonadIO.Temp
  ( mkTempDir, withTempDir'', withTempDirCD, withTempDirCD'
  , withTempFH, withBinaryTempFH, withUTF8TempFH
  , writeTempFileBinary, writeTempFileUTF8 )
where

-- base --------------------------------

import Control.Monad           ( (>=>), return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Function           ( ($), flip )
import Data.List               ( dropWhileEnd )
import GHC.Stack               ( HasCallStack )
import System.Environment      ( getProgName )
import System.IO               ( FilePath, Handle, IO, NewlineMode
                               , SeekMode( AbsoluteSeek ), TextEncoding
                               , char8, hSeek, hSetEncoding, hSetNewlineMode
                               , nativeNewlineMode, noNewlineTranslation, utf8
                               )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- bytestring --------------------------

import qualified Data.ByteString  as  BS

import Data.ByteString  ( ByteString )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadMask )

-- fpath -------------------------------

import FPath.AbsDir            ( AbsDir )
import FPath.AbsFile           ( AbsFile )
import FPath.AsFilePath        ( filepath )
import FPath.Dir               ( DirAs( _Dir_ ) )
import FPath.Error.FPathError  ( AsFPathError )
import FPath.FileTypeC         ( FileTypeC( FileType ) )
import FPath.Rel               ( RelAs( _Rel_ ) )
import FPath.RelFile           ( RelFile )
import FPath.Parseable         ( Parseable( parse ) )

-- lens --------------------------------

import Control.Lens.Review  ( review )

-- monaderror-io -----------------------

import MonadError.IO        ( asIOError, asIOErrorT )
import MonadError.IO.Error  ( AsIOError )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens   ( (⫥) )
import Data.MoreUnicode.Monad  ( (≫) )
import Data.MoreUnicode.Text   ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )

-- temporary ---------------------------

import qualified System.IO.Temp

import System.IO.Temp  ( createTempDirectory, getCanonicalTemporaryDirectory
                       , withTempDirectory )

-- text --------------------------------

import qualified  Data.Text.IO  as  TextIO

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.Base       ( hClose, unlink )
import MonadIO.Directory  ( inDir )

--------------------------------------------------------------------------------

type ℍ  = Handle
type 𝔹𝕊 = ByteString

----------------------------------------

parseAbsDir ∷ (AsFPathError ε, MonadError ε η) ⇒ FilePath → η AbsDir
parseAbsDir = parse ∘ (⊕ "/") ∘ dropWhileEnd (≡ '/')

----------------------------------------

{- | Get the system temporary directory (TMPDIR, etc.) -}
tempdir ∷ ∀ ε μ .
          (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ,
           HasCallStack) ⇒
          μ AbsDir
tempdir = asIOError getCanonicalTemporaryDirectory ≫ parseAbsDir

----------------------------------------

{- | Create a temporary directory as a subdir of a given dir; return its name.
     It is the responsibility of the caller to arrange appropriate cleanup. -}
mkTempDir'' ∷ ∀ ε ρ δ μ .
              (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ,
               HasCallStack, RelAs ρ, DirAs δ) ⇒
              δ → ρ → μ AbsDir
mkTempDir'' t (review filepath ∘ review _Rel_ → r) = do
  d ← liftIO $ createTempDirectory (t ⫥ (filepath ∘ _Dir_)) r
  parseAbsDir d

--------------------

{- | `mkTempDir''`, but create a dir in the system temp dir. -}
mkTempDir' ∷ ∀ ε ρ μ .
             (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ,
              HasCallStack, RelAs ρ) ⇒
             ρ → μ AbsDir
mkTempDir' r = tempdir ≫ \ d → mkTempDir'' d r

--------------------

{- | `mkTempDir'`, with the prefix being the program name plus `"-"`. -}
mkTempDir ∷ ∀ ε μ .
            (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ,
             HasCallStack) ⇒
            μ AbsDir
mkTempDir = progNamePrefix ≫ mkTempDir'

----------------------------------------

{- | Perform some IO with a given temporary file, created within some given dir;
     the temporary file is removed once IO is complete.  The file created is
     passed into the IO as an `AbsFile`.  The directory name is prefixed by some
     relative name.
 -}
withTempFile'' ∷ ∀ ε α ρ δ μ .
                 (MonadIO μ, MonadMask μ,
                  AsFPathError ε, AsIOError ε, MonadError ε μ, HasCallStack,
                  DirAs δ, Parseable (FileType δ), RelAs ρ) ⇒
                 δ → ρ → (FileType δ → ℍ → ExceptT ε IO α) → μ α
withTempFile'' d (review $ filepath ∘ _Rel_ → r) io =
  let doFile f h = parse f ≫ \ f' → io f' h
   in asIOErrorT $ System.IO.Temp.withTempFile (d ⫥ filepath ∘ _Dir_) r doFile

--------------------

{- | Like `withTempFile''`, but uses the system temp dir (see `tempdir`). -}
withTempFile' ∷ ∀ ε α ρ μ .
                (MonadIO μ, MonadMask μ,
                 AsFPathError ε, AsIOError ε, MonadError ε μ, HasCallStack,
                 RelAs ρ) ⇒
                ρ → (AbsFile → ℍ → ExceptT ε IO α) → μ α
withTempFile' r io = tempdir ≫ \ d → withTempFile'' d r io

----------------------------------------

{- | Like `withTempFile'`, but the program name for the temp file template. -}
withTempFile ∷ ∀ ε α μ .
               (MonadIO μ, MonadMask μ,
                AsFPathError ε, AsIOError ε, MonadError ε μ, HasCallStack) ⇒
               (AbsFile → ℍ → ExceptT ε IO α) → μ α
withTempFile io = progNamePrefix ≫ \ p → withTempFile' p io

----------------------------------------

withTempFH ∷ ∀ ε β α μ .
             (MonadIO μ, MonadMask μ,
              AsIOError ε, AsFPathError ε, MonadError ε μ, HasCallStack) ⇒
             TextEncoding → NewlineMode → (ℍ → β → IO ()) → β
           → (ℍ → ExceptT ε IO α) → μ α

withTempFH enc nlm writer content io =
  withTempFile ( \ fn fh → do
                   unlink fn
                   liftIO $ do
                     hSetEncoding     fh enc
                     hSetNewlineMode  fh nlm
                     writer           fh content
                     hSeek            fh AbsoluteSeek 0
                   io fh
               )

--------------------

withUTF8TempFH ∷ ∀ ε α μ .
                 (MonadIO μ, MonadMask μ,
                  AsIOError ε, AsFPathError ε, MonadError ε μ, HasCallStack) ⇒
                 𝕋 → (ℍ → ExceptT ε IO α) → μ α

{- | Provide a filehandle to an already-deleted file, that has had data written
     to it (in UTF8-Text), but has been seek()ed (sought?) back to the start. -}
withUTF8TempFH = withTempFH utf8 nativeNewlineMode TextIO.hPutStrLn

--------------------

withBinaryTempFH ∷ ∀ ε α μ .
                 (MonadIO μ, MonadMask μ,
                  AsIOError ε, AsFPathError ε, MonadError ε μ, HasCallStack) ⇒
                 𝕋 → (ℍ → ExceptT ε IO α) → μ α

{- | Provide a filehandle to an already-deleted file, that has had data written
     to it (as binary), but has been seek()ed (sought?) back to the start. -}
withBinaryTempFH = withTempFH char8 noNewlineTranslation TextIO.hPutStrLn

----------------------------------------

_parseD ∷ (Parseable χ, AsFPathError ε, MonadError ε η) ⇒ FilePath → η χ
_parseD = parse ∘ (⊕ "/") ∘ dropWhileEnd (≡ '/')

{- | Perform some IO with a given temporary directory, created within some given
     dir; the temporary dir is removed once IO is complete.  The directory
     created is passed into the IO as an `AbsDir`.  The directory name is
     prefixed by some relative name.
 -}
-- note that withTempDirectory will give us a relative dir if passed a relative
-- dir (that exists and is usable)
withTempDir'' ∷ ∀ ε α ρ δ μ .
                (MonadIO μ, MonadMask μ, AsFPathError ε, AsIOError ε,
                 MonadError ε μ, HasCallStack, DirAs δ, Parseable δ, RelAs ρ) ⇒
                δ → ρ → (δ → ExceptT ε IO α) → μ α
withTempDir'' d (review $ filepath ∘ _Rel_ → r) io =
  asIOErrorT $ withTempDirectory (d ⫥ filepath ∘ _Dir_) r (_parseD >=> io)

----------

{- | Perform some IO with a temporary directory (created in the system temp
     directory, see `tempdir`), which is removed once IO is complete.  The
     directory created is passed into the IO as an `AbsDir`.  The directory name
     is prefixed by some relative name.
 -}
withTempDir' ∷ ∀ ε α ρ μ .
               (MonadIO μ, MonadMask μ, AsFPathError ε, AsIOError ε,
                MonadError ε μ, HasCallStack, RelAs ρ) ⇒
               ρ → (AbsDir → ExceptT ε IO α) → μ α
withTempDir' r io = tempdir ≫ \ d → withTempDir'' d r io

{- | A prefix (suitable for, e.g., temp files or dirs) in the form of a
     `RelFile` (which is the programme name, plus a '-' character. -}
progNamePrefix ∷ ∀ ε μ .
                 (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ,
                  HasCallStack) ⇒
                 μ RelFile
progNamePrefix = asIOError getProgName ≫ parse ∘ (⊕ "-")

{- | Like `withTempDir'`, with the prefix being the program name plus `"-"`. -}
withTempDir ∷ ∀ ε α μ .
              (MonadIO μ, MonadMask μ, AsFPathError ε, AsIOError ε,
               MonadError ε μ, HasCallStack) ⇒
              (AbsDir → ExceptT ε IO α) → μ α
withTempDir io = progNamePrefix ≫ \ p → withTempDir' p io

{- | Like `withTempDir`, but temporarily changes dir into the temporary
     directory, rather than passing the dir name to the IO. -}
withTempDirCD ∷ ∀ ε α μ .
                (MonadIO μ, MonadMask μ, AsFPathError ε, AsIOError ε,
                 MonadError ε μ, HasCallStack) ⇒
                ExceptT ε IO α → μ α
withTempDirCD io = withTempDir (flip inDir io)

{- | Like `withTempDir`, but temporarily changes dir into the temporary
     directory, as well as passing the dir name to the IO. -}
withTempDirCD' ∷ ∀ ε α μ .
                 (MonadIO μ, MonadMask μ, AsFPathError ε, AsIOError ε,
                  MonadError ε μ, HasCallStack) ⇒
                 (AbsDir → ExceptT ε IO α) → μ α
withTempDirCD' io = withTempDir (\ d → inDir d $ io d)

----------------------------------------

{- | Write a temporary file, with contents, using a given encoding, newline-mode
     and writer function. -}
writeTempFile ∷ ∀ ε τ μ .
                (MonadIO μ, MonadMask μ,
                 AsFPathError ε, AsIOError ε, MonadError ε μ, HasCallStack) ⇒
                TextEncoding → NewlineMode → (ℍ → τ → IO ()) → τ
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
writeTempFileUTF8 ∷ ∀ ε μ .
                    (MonadIO μ, MonadMask μ,
                     AsFPathError ε, AsIOError ε, MonadError ε μ, HasCallStack) ⇒
                    Text → μ AbsFile
writeTempFileUTF8   = writeTempFile utf8 nativeNewlineMode TextIO.hPutStr

----------

{- | Write a temporary file with binary contents. -}
writeTempFileBinary ∷ ∀ ε μ .
                      (MonadIO μ, MonadMask μ,
                       AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒
                      𝔹𝕊 → μ AbsFile
writeTempFileBinary = writeTempFile char8 noNewlineTranslation BS.hPutStr

-- that's all, folks! ----------------------------------------------------------
