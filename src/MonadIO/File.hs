{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module MonadIO.File
  ( module FStat
  , module OpenFile

  , AccessMode(..), System.IO.IOMode(..)
  , hClose

  , devnull

  , access, writable

  , chmod, unlink

  , readlink, resolvelink

  , fileWritable, isWritableFile, isWritableDir

  , fileFoldLinesUTF8, fileFoldLinesH

  , readFlags, readWriteFlags, readWriteExFlags, readWriteNoTruncFlags
  , writeFlags, writeExFlags, writeNoTruncFlags, appendFlags

  , tests
  )
where

import Prelude  ( error )

-- base --------------------------------

import Control.Monad           ( return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bool               ( Bool( False, True ) )
import Data.Either             ( Either )
import Data.Eq                 ( Eq )
import Data.Function           ( ($), const )
import Data.List               ( isSuffixOf, last, or )
import Data.Maybe              ( Maybe( Just, Nothing ) )
import Data.String             ( String )
import GHC.Stack               ( HasCallStack )
import System.Exit             ( ExitCode )
import System.IO               ( FilePath, Handle, IO
                               , IOMode( AppendMode, ReadMode, ReadWriteMode
                                       , WriteMode )
                               , hIsEOF
                               )
import Text.Show               ( Show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.List.Unicode      ( (∈) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( toString )

-- fpath -------------------------------

import FPath.Abs               ( Abs( AbsD, AbsF ) )
import FPath.AbsDir            ( AbsDir, absdir, root )
import FPath.AbsFile           ( absfile )
import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.AsFilePath'       ( exterminate )
import FPath.Dir               ( DirAs )
import FPath.Error.FPathError  ( AsFPathError, FPathIOError )
import FPath.File              ( FileAs( _File_ ) )
import FPath.Parent            ( parent )
import FPath.RelDir            ( reldir )
import FPath.RelFile           ( RelFile, relfile )

-- fstat -------------------------------

import FStat  ( FStat, FileType( Directory, SymbolicLink ), ftype )

-- lens --------------------------------

import Control.Lens.Review  ( review )

import qualified System.FilePath.Lens

-- monadio-error -----------------------

import MonadError           ( ѥ )
import MonadError.IO        ( asIOError, asIOErrorY )
import MonadError.IO.Error  ( AsIOError, IOError )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool     ( 𝔹 )
import Data.MoreUnicode.Functor  ( (⊳), (⊳⊳) )
import Data.MoreUnicode.Lens     ( (⊣), (⫥) )
import Data.MoreUnicode.Maybe    ( 𝕄 )
import Data.MoreUnicode.Monad    ( (≫) )
import Data.MoreUnicode.Monoid   ( ю )
import Data.MoreUnicode.Natural  ( ℕ )
import Data.MoreUnicode.String   ( 𝕊 )
import Data.MoreUnicode.Text     ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- safe --------------------------------

import Safe  ( headMay )

-- tasty -------------------------------

import Test.Tasty  ( TestName, TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), assertRight, runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import qualified  Data.Text.IO  as  TextIO

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- unix --------------------------------

import qualified  System.Posix.Files  as  Files
import System.Posix.Files  ( readSymbolicLink )
import System.Posix.IO     ( OpenFileFlags( OpenFileFlags, append, exclusive
                                          , noctty, nonBlock, trunc )
                           , noctty, nonBlock
                           )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.FStat     as  FStat     hiding ( tests )
import MonadIO.OpenFile  as  OpenFile  hiding ( tests )

import MonadIO.Base      ( chmod, hClose, unlink )
import MonadIO.FPath     ( pResolve, pResolveDir )
import MonadIO.Tasty     ( TestFileSpec( TFSDir, TFSFile, TFSSymL )
                         , testInTempDirFS )

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

-- fileAccess ----------------------------------------------

data AccessMode = ACCESS_R | ACCESS_WX | ACCESS_RWX
                | ACCESS_W | ACCESS_RX
                | ACCESS_X | ACCESS_RW
  deriving (Eq,Show)

access ∷ ∀ ε ρ μ .
         (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack, AsFilePath ρ) ⇒
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
writable ∷ ∀ ε ρ μ .
           (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack, AsFilePath ρ)⇒
            ρ → μ (𝕄 𝔹)
writable = access ACCESS_W

----------------------------------------

{- | Is `f` an extant writable file? -}
_isWritableFile ∷ (MonadIO μ, FileAs γ, MonadError ε μ, HasCallStack,
                   AsIOError ε) ⇒
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
isWritableFile ∷ ∀ ε γ μ .
                (MonadIO μ, FileAs γ, MonadError ε μ, HasCallStack,
                 AsIOError ε) ⇒
                 γ -> μ (𝕄 𝕋)

isWritableFile (review _File_ → f) = stat f ≫ _isWritableFile f

----------

isWritableFileTests ∷ TestTree
isWritableFileTests =
  let check f exp =
        testCase (toString f) $
                ѥ (isWritableFile @IOError f) ≫ assertRight (Just exp @=?)
   in testGroup "_isWritableFile"
                [ check [absfile|/etc|] "/etc is a directory" ]

----------------------------------------

{- | Is `d` an extant writable directory? -}
isWritableDir ∷ ∀ ε γ μ .
                (MonadIO μ, DirAs γ, MonadError ε μ, HasCallStack, AsIOError ε)⇒
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
                    ѥ (isWritableDir @IOError f) ≫ assertRight (Just e @=?)
      testN f   = testCase (toString f) $
                    ѥ (isWritableDir @IOError f) ≫ assertRight (Nothing @=?)
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
               (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ, HasCallStack)⇒
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
            , testE [absfile|/etc|]
                    "/etc is a directory"

            , testE' [absfile|/dev/null|] Nothing
            ]

----------------------------------------

{- | Work over a file, accumulating results, line-by-line. -}
fileFoldLinesUTF8 ∷ ∀ ε γ α μ .
                    (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                     HasCallStack) ⇒
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
devnull ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack) ⇒ μ Handle
devnull = openFileReadWriteNoTruncBinary Nothing [absfile|/dev/null|]

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

_readlinkTests ∷ TestName → (𝕊 → IO (Either FPathIOError Abs)) → (α → RelFile)
               → (α → FilePath) → (α → AbsDir → Abs) → [α] → TestTree
_readlinkTests name f getName getTarget getExp ts =
  let file_setup = ю [ [ TFSDir  [reldir|directory/|] 0o700
                       , TFSFile [relfile|plain|] 0o644 "some text" ]
                     , [ TFSSymL (getName t) (getTarget t) | t ← ts ]
                     ]
      check ∷ IO AbsDir → 𝕊 → (AbsDir → Abs) → TestTree
      check d fn exp = let path t = toString t ⊕ "/" ⊕ fn
                        in testCase fn $ d ≫ \ t →
                             f (path t) ≫ assertRight (exp t ≟)
      -- check' ∷ IO AbsDir → α → TestTree
      check' d t = check d (getName t ⫥ filepath) (getExp t)
      do_test tmpdir = testGroup name [ check' tmpdir t | t ← ts ]

  in testInTempDirFS file_setup (const $ return ()) do_test

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
tests = testGroup "MonadIO.File" [ isWritableDirTests, isWritableFileTests
                                 , fileWritableTests, readlinkTests
                                 , resolvelinkTests
                                 ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree MonadIO.File.tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP MonadIO.File.tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay MonadIO.File.tests

-- that's all, folks! ----------------------------------------------------------

