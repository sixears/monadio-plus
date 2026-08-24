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

{-| IO Operations on files -}

module MonadIO.File
  ( AccessMode(..)
  , module FStat
  , module OpenFile
  , System.IO.IOMode(..)
  , access
  , chmod
  , fileFoldLinesH
  , fileFoldLinesUTF8
  , fileWritable
  , hClose
  , isWritableDir
  , isWritableFile
  , readlink
  , rename
  , resolvelink
  , resolvelink'
  , tests
  , unlink
  , writable
  ) where

import Base1T
import Prelude ( error )

-- base --------------------------------

import Data.List.NonEmpty qualified as NonEmpty

import Data.List ( isSuffixOf, or )
import System.IO ( FilePath, Handle,
                   IOMode(AppendMode, ReadMode, ReadWriteMode, WriteMode),
                   hIsEOF )

-- fpath -------------------------------

import FPath.Abs              ( Abs(AbsD, AbsF) )
import FPath.AbsDir           ( AbsDir, absdir, root )
import FPath.AbsFile          ( absfile )
import FPath.AppendableFPath  ( (⫻) )
import FPath.AsFilePath       ( AsFilePath(filepath) )
import FPath.AsFilePath'      ( exterminate )
import FPath.Dir              ( DirAs )
import FPath.Error.FPathError ( AsFPathError, FPathIOError )
import FPath.File             ( FileAs(_File_) )
import FPath.Parent           ( parent )
import FPath.RelDir           ( reldir )
import FPath.RelFile          ( RelFile, relfile )
import FPath.ToDir            ( toDir )
import FPath.ToFile           ( toFileY )

-- fstat -------------------------------

import FStat ( FStat, FileType(Directory, SymbolicLink), ftype )

-- lens --------------------------------

import Control.Lens.Getter ( view )

import System.FilePath.Lens qualified

-- monadio-error -----------------------

import MonadError.IO       ( asIOErrorY, ioThrow )
import MonadError.IO.Error ( IOError )

-- mtl ---------------------------------

import Control.Monad.Trans ( lift )

-- tasty -------------------------------

import Test.Tasty  ( DependencyType( AllSucceed ), dependentTestGroup )

-- text --------------------------------

import Data.Text.IO qualified as TextIO

import Data.Text ( intercalate )

-- unix --------------------------------

import System.Posix.Files ( readSymbolicLink )
import System.Posix.Files qualified as Files

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.FStat    as FStat hiding ( tests )
import MonadIO.OpenFile as OpenFile hiding ( tests )

import MonadIO.Base        ( chmod, hClose, unlink )
import MonadIO.FPath       ( pResolve, pResolveDir )
import MonadIO.NamedHandle ( handle )
import MonadIO.Tasty       ( TestFileSpec(TFSDir, TFSFile, TFSSymL),
                             testInTempDirFS )

import MonadIO.T.ReadlinkTestCases ( readExp, readlinkTestCases, resolveExp,
                                     slName, slTarget )

--------------------------------------------------------------------------------

-- fileAccess ----------------------------------------------

{- | file access combinations -}
data AccessMode = ACCESS_R | ACCESS_WX | ACCESS_RWX | ACCESS_W | ACCESS_RX | ACCESS_X | ACCESS_RW deriving
  ( Eq
  , Show
  )

{-| see `Files.fileAccess` -}
access ∷ ∀ ε ρ μ .
         (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack, AsFilePath ρ) ⇒
         AccessMode → ρ → μ (𝕄 𝔹)
access mode ((⫥ filepath) → fp) = asIOErrorY $ go mode fp
  where go ∷ AccessMode → FilePath → IO 𝔹
        go ACCESS_R   p = Files.fileAccess (p ⫥ filepath) 𝓣 𝓕 𝓕
        go ACCESS_W   p = Files.fileAccess (p ⫥ filepath) 𝓕 𝓣 𝓕
        go ACCESS_X   p = Files.fileAccess (p ⫥ filepath) 𝓕 𝓕 𝓣
        go ACCESS_RW  p = Files.fileAccess (p ⫥ filepath) 𝓣 𝓣 𝓕
        go ACCESS_RX  p = Files.fileAccess (p ⫥ filepath) 𝓣 𝓕 𝓣
        go ACCESS_WX  p = Files.fileAccess (p ⫥ filepath) 𝓕 𝓣 𝓣
        go ACCESS_RWX p = Files.fileAccess (p ⫥ filepath) 𝓣 𝓣 𝓣

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
                  γ → 𝕄 FStat → μ (𝕄 𝕋)

_isWritableFile (review _File_ → f) st =
  let rJust = return ∘ 𝓙
   in case st of
        𝓝  → rJust $ [fmt|%T does not exist|] f
        𝓙 stp → if Directory ≡ ftype stp
                   then rJust $ [fmt|%T is a directory|] f
                   else writable f ≫ \ case
                          𝓝   → rJust $ [fmt|no such file %T|] f
                          𝓙 𝓣 → return 𝓝
                          𝓙 𝓕 → rJust $ [fmt|cannot write to %T|] f

----------------------------------------

{- | Is `f` an extant writable file? -}
isWritableFile ∷ ∀ ε γ μ .
                (MonadIO μ, FileAs γ, MonadError ε μ, HasCallStack,
                 AsIOError ε) ⇒
                 γ → μ (𝕄 𝕋)

isWritableFile (review _File_ → f) = stat f ≫ _isWritableFile f

----------

isWritableFileTests ∷ TestTree
isWritableFileTests =
  let check f exp =
        testCase (toString f) $
                ѥ (isWritableFile @IOError f) ≫ assertRight (𝓙 exp @=?)
   in testGroup "_isWritableFile"
                [ check [absfile|/etc|] "/etc is a directory" ]

----------------------------------------

{- | Is `d` an extant writable directory? -}
isWritableDir ∷ ∀ ε γ μ .
                (MonadIO μ, DirAs γ, MonadError ε μ, HasCallStack, AsIOError ε)⇒
                γ → μ (𝕄 𝕋)

isWritableDir d =
  let rJust = return ∘ 𝓙
   in stat d ≫ \ case
        𝓝  → rJust $ [fmt|%T does not exist|] d
        𝓙 stp → if Directory ≡ ftype stp
                   then writable d ≫ \ case
                          𝓝   → rJust $ [fmt|no such directory %T|] d
                          𝓙 𝓣 → return 𝓝
                          𝓙 𝓕 → rJust $ [fmt|cannot write to %T|] d
                   else -- remove trailing '/', since the point is that d is
                        -- not a directory
                        rJust $ [fmt|%s is not a directory|]
                                (exterminate (d ⫥ filepath))

----------

isWritableDirTests ∷ TestTree
isWritableDirTests =
  let testE f e = testCase (toString f) $
                    ѥ (isWritableDir @IOError f) ≫ assertRight (𝓙 e @=?)
      testN f   = testCase (toString f) $
                    ѥ (isWritableDir @IOError f) ≫ assertRight (𝓝 @=?)
   in testGroup "isWritableDir"
            [ testN [absdir|/tmp/|]
            , testE [absdir|/nonsuch/|]
                    "/nonsuch/ does not exist"
            , testE [absdir|/nonsuch/passwd/|]
                    "/nonsuch/passwd/ does not exist"
-- this doesn't fail when, e.g., running in a chroot with a user
-- namespace as root
--            , testE [absdir|/etc/|]
--                    "cannot write to /etc/"
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
    𝓙 st → _isWritableFile fn (𝓙 st)
    𝓝 → -- fn does not exist; does it have a writeable dir parent?
              isWritableDir (fn ⊣ parent) ≫ \ case
                   𝓝   → return 𝓝
                   𝓙 e → return ∘ 𝓙 $ [fmt|%t (%T)|] e fn

----------

fileWritableTests ∷ TestTree
fileWritableTests =
  let testE f e = testCase (toString f) $
                    ѥ (fileWritable @_ @IOError f) ≫ assertRight (𝓙 e @=?)
      testE' f e = testCase (toString f) $
                     ѥ (fileWritable @_ @IOError f) ≫ assertRight (e @=?)

   in testGroup "fileWritable"
            [ testE [absfile|/etc/passwd|]
                    "cannot write to /etc/passwd"
            , testE [absfile|/nonsuch/passwd|]
                    "/nonsuch/ does not exist (/nonsuch/passwd)"
-- this doesn't fail when, e.g., running in a chroot with a user
-- namespace as root
--            , testE [absfile|/etc/nonsuch|]
--                    "cannot write to /etc/ (/etc/nonsuch)"
            , testE [absfile|/etc/passwd/nonsuch|]
                    "/etc/passwd is not a directory (/etc/passwd/nonsuch)"
            , testE [absfile|/etc|]
                    "/etc is a directory"

            , testE' [absfile|/dev/null|] 𝓝
            ]

----------------------------------------

{-| Fold a function over the lines of a filehandle.
    `a` is the initial value of the fold; `io' is the folding function; `h` is
    the filehandle to read.
 -}
fileFoldLinesH ∷ ∀ α μ . (MonadIO μ) ⇒ α → (α → 𝕋 → μ α) → Handle → μ α
fileFoldLinesH a io h = do
  eof ← liftIO $ hIsEOF h
  case eof of
    𝓣 → return a
    𝓕 → do l ← liftIO $ TextIO.hGetLine h
           a' ← io a l
           fileFoldLinesH a' io h

{- | fold over a file, accumulating results, line-by-line -}
fileFoldLinesUTF8 ∷ ∀ ε γ α μ .
                    (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ,
                     HasCallStack) ⇒
                    α → (α → 𝕋 → IO α) → γ → μ α
fileFoldLinesUTF8 a io fn =
  withFile UTF8 FileR fn $ lift ∘ fileFoldLinesH a io ∘ view handle

----------------------------------------

-- This has to return an absolute path, as the relative path might include
-- many '..' that can't be represented by FPath.  So we resolve it.
{-| Read a symlink, return the absolute path to the referent.

    Any path that is not actually a symlink will cause an IO error to be thrown.

    The referent is returned as-is; that is, it is not checked for existence;
    a referent with a trailing slash is returned as a dir (whether or not the
    thing it points to is really a directory, or even exists); likewise, a thing
    without a trailing slash is returned as a file.
 -}
readlink ∷ ∀ ε ρ μ . (MonadIO μ, AsFilePath ρ,
                      AsIOError ε, AsFPathError ε, MonadError ε μ,HasCallStack)⇒
           ρ → μ Abs
-- we need the exterminate to ensure that a "dir symlink", e.g., /tmp/foo/s/
-- that is a symlink passed in with a trailing slash, is still treated as the
-- file that it is (i.e., /tmp/foo/s, which is a symlink).  Without the
-- exterminate, the System.FilePath.Lens.Directory gives the dir as /tmp/foo/s
-- rather than /tmp/foo.
readlink (review filepath → fp) = do
  -- readSymbolicLink doesn't like paths that end with a trailing slash.
  -- Dropping such characters should be safe; the only path for which that does
  -- not work is '/' (or "//", "///", etc.)
  r ← asIOError $ readSymbolicLink (exterminate fp)
  case head r of
    𝓝     → -- this should never happen, as `readSymbolicLink` returns a
            -- Filepath which in principle can never be an empty string
            error $ [fmt|empty symlink found at '%s'|] fp
    𝓙 '/' → -- last is safe, as fp is non-empty, given that head fp
            -- is not 𝓝
            case last r of
              𝓙 '/' → AbsD ⊳ pResolveDir root r
              _     → AbsF ⊳ pResolveDir root r
    𝓙 _   → do d ← pResolve (fp ⊣ System.FilePath.Lens.directory)
                   -- last is safe, as fp is non-empty, given that headMay fp
                   -- is not 𝓝
               case last r of
                 𝓙 '/' → AbsD ⊳ pResolveDir d r
                 _     → if or [ r ∈ [ ".", ".." ]
                               , "/." `isSuffixOf` r
                               , "/.." `isSuffixOf` r
                               ]
                         then AbsD ⊳ pResolveDir d r
                         else AbsF ⊳ pResolveDir d r

----------

{-| Run some tests for `readlink`. -}
_readlinkTests ∷ ∀ α β . (Eq β, Show β) ⇒
                 TestName
                 {--| function to be tested -}
               → (Abs → IO (Either FPathIOError β))
                 {--| find name of the symlink relative to the tmp dir -}
               → (α → RelFile)
                 {--| find the target of the symlink -}
               → (α → FilePath)
                 {--| expected result of calling the function under test
                      (as an abs, possibly at the given tmpdir; the fn is
                      given a 𝕊 absolute filepath being the tmpdir+"/"+relfile)
                  -}
               → (α → AbsDir → β)
                 {--| test cases; as an opaque type whose attributes are
                    are queried by prior functions -}
               → [α]
               → TestTree
_readlinkTests name f getName getTarget getExp ts =
  let file_setup = ю [ [ TFSDir  [reldir|directory/|] 0o700
                       , TFSFile [relfile|plain|] 0o644 "some text" ]
                     , [ TFSSymL (getName t) (getTarget t) | t ← ts ]
                     ]
      --- check ∷ IO AbsDir → 𝕊 → (AbsDir → Abs) → TestTree
      check d fn exp = let -- path t = toString t ⊕ "/" ⊕ fn
                        in testCase (toString fn) $ d ≫ \ t →
                             f (AbsF $ t ⫻ fn) ≫ assertRight (exp t @=?)
      -- check' ∷ IO AbsDir → α → TestTree
      check' d t = check d (getName t {- ⫥ filepath -}) (getExp t)
      do_test tmpdir = dependentTestGroup name AllSucceed
                                          [ check' tmpdir t | t ← ts ]

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
     the result re-examined.  Note, however, an error is thrown if the input
     file does not exist.

     The return value is the list of files (symlinks) that are resolved; with
     the final resolved file at the head of the list, and the input file at the
     last of the list.
-}
resolvelink' ∷ ∀ ε μ . (MonadIO μ, HasCallStack,
                        AsIOError ε, AsFPathError ε, MonadError ε μ) ⇒
              NonEmpty Abs → μ (NonEmpty Abs)
resolvelink' (fp :| fps) = do
  when (fp ∈ fps) $
    ioThrow $ [fmtT|resolvelink: cycle detected: %t|]
            $ intercalate " → " (toText ⊳ (fp:fps))
  ftype ⊳⊳ lstat fp ≫ \ case
    𝓙 SymbolicLink → readlink fp ≫ resolvelink' ∘ (:| (fp : fps))
    𝓙 Directory    → return $ AbsD (toDir fp) :| fps
    𝓙 _            → case toFileY fp of
                       𝓙 r → return $ AbsF r :| fps
                       -- this should never happen; toFileY only fails on
                       --   /  ) which will be caught by the `𝓙 Directory`
                       --        clause above
                       --   ./ ) which is clearly not an Abs
                       𝓝 → ioThrow $ [fmtT|resolvelink: '%T' failed toFileY|] fp
    𝓝              → return (fp :| fps)

{- | Recursively read a symbolic link, until it is a symbolic link no more.
     Anything other than a (readable) symbolic link is immediately returned
     intact (including non-existent files).  A symbolic link is deferenced, and
     the result re-examined.  Note, however, an error is thrown if the input
     file does not exist.
-}
resolvelink ∷ ∀ ε μ . (MonadIO μ, HasCallStack,
                       AsIOError ε, AsFPathError ε, MonadError ε μ) ⇒
              Abs → μ (𝕄 Abs)
resolvelink = 𝓙 ∘ NonEmpty.head ⩺ resolvelink' ∘ pure

----------------------------------------

resolvelinkTests ∷ TestTree
resolvelinkTests = _readlinkTests "resolvelink" (ѥ ∘ resolvelink) slName
                   slTarget resolveExp readlinkTestCases


----------------------------------------

{- | See `Files.rename` -}
rename ∷ ∀ ε γ δ μ . (MonadIO μ, HasCallStack, FileAs γ, FileAs δ,
                      AsIOError ε, MonadError ε μ, HasCallStack) ⇒
         γ → δ → μ ()
rename (review _File_ → from) (review _File_ → to) =
  liftIO $ Files.rename (from ⫥ filepath) (to ⫥ filepath)

------------------------------------------------------------

{-| unit tests -}
tests ∷ TestTree
tests = dependentTestGroup "MonadIO.File" AllSucceed [ isWritableDirTests
                                                     , isWritableFileTests
                                                     , fileWritableTests
                                                     , readlinkTests
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

