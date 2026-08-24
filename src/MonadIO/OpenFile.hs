{-# LANGUAGE DataKinds         #-}
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
  ( FileOpenMode(..)
  , HEncoding(Binary, NoEncoding, UTF8)
  , appendFile
  , appendFlags
  , devnull
  , fileOpenMode
  , openFile
  , pattern ℍ
  , readFile
  , readFileUTF8Lenient
  , readFileUTF8LenientY
  , readFileY
  , readFlags
  , readWriteExFlags
  , readWriteFlags
  , readWriteNoTruncFlags
  , tests
  , withFile
  , writeExFile
  , writeExFlags
  , writeFile
  , writeFlags
  , writeNoTruncFile
  , writeNoTruncFlags
  ) where

import Base1T

-- base --------------------------------

import System.IO          ( IOMode( AppendMode, ReadMode, ReadWriteMode,
                                    WriteMode ) )
import System.Posix.Types ( FileMode )

-- exceptions --------------------------

import Control.Monad.Catch ( bracket )

-- fpath -------------------------------

import FPath.AbsFile    ( absfile )
import FPath.AsFilePath ( AsFilePath(filepath) )
import FPath.File       ( FileAs(_File_) )

-- monadio-error -----------------------

import MonadError.IO.Error ( IOError, squashNoSuchThingT )

-- tasty -------------------------------

import Test.Tasty  ( DependencyType( AllSucceed ), dependentTestGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit ( Assertion )

-- tasty-plus --------------------------

import TastyPlus  ( (≟) )

-- text --------------------------------

import Data.Text                ( drop, length )
import Data.Text.Encoding       ( decodeUtf8With )
import Data.Text.Encoding.Error ( lenientDecode )

-- unix --------------------------------

import System.Posix.IO ( OpenFileFlags(OpenFileFlags, append, exclusive, noctty, nonBlock, trunc),
                         OpenMode(ReadOnly, ReadWrite, WriteOnly), cloexec,
                         creat, directory, fdToHandle, noctty, nofollow,
                         nonBlock, openFd, sync )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.Base        ( chmod, unlink )
import MonadIO.FStat       ( FExists( NoFExists ), lfexists' )
import MonadIO.NamedHandle ( HEncoding(Binary, NoEncoding, UTF8),
                             HGetContents(hGetContents),
                             HWriteContents(hWriteContents),
                             ImpliedEncoding(impliedEncoding), ℍ, hClose,
                             hSetEncoding, impliedEncodingM, pattern ℍ )

--------------------------------------------------------------------------------

data FileOpenMode = FileR
                  | FileRW (𝕄 FileMode)
                  | FileRWEx FileMode
                  | FileRWNoTrunc (𝕄 FileMode)
                  | FileA (𝕄 FileMode)
                  | FileW (𝕄 FileMode)
                  | FileWEx FileMode
                  | FileWNoTrunc (𝕄 FileMode)
                  | FileOpenMode (IOMode, OpenFileFlags, 𝕄 FileMode)

fileOpenMode ∷ FileOpenMode → (IOMode, OpenFileFlags)
fileOpenMode FileR                 = (ReadMode     , readFlags)
fileOpenMode (FileRW        perms) = (ReadWriteMode, readWriteFlags { creat = perms})
fileOpenMode (FileRWEx      perms) = (ReadWriteMode, readWriteExFlags  { creat = 𝓙 perms })
fileOpenMode (FileRWNoTrunc perms) = (ReadWriteMode, writeNoTruncFlags { creat = perms })
fileOpenMode (FileA         perms) = (AppendMode   , appendFlags       { creat = perms })
fileOpenMode (FileW         perms) = (WriteMode    , writeFlags        { creat = perms })
fileOpenMode (FileWEx       perms) = (WriteMode    , writeExFlags      { creat = 𝓙 perms })
fileOpenMode (FileWNoTrunc  perms) = (WriteMode    , writeNoTruncFlags { creat = perms })
fileOpenMode (FileOpenMode(m,f,p)) = (m            , f                 { creat = p })

------------------------------------------------------------

{- | OpenFileFlags suitable for reading. -}
readFlags ∷ OpenFileFlags
readFlags = OpenFileFlags { append = 𝓕, exclusive = 𝓕, noctty = 𝓕
                          , nonBlock = 𝓕, trunc = 𝓕, nofollow = 𝓕
                          , creat = 𝓝, cloexec = 𝓕, directory = 𝓕, sync = 𝓕 }
--------------------

{- | OpenFileFlags suitable for read-write opens /with pre-truncation/
     (analogous to writeFlags) . -}
readWriteFlags ∷ OpenFileFlags
readWriteFlags = OpenFileFlags { append = 𝓕, exclusive = 𝓕, noctty = 𝓕
                               , nonBlock = 𝓕, trunc = 𝓣, nofollow = 𝓕
                               , creat = 𝓝, cloexec = 𝓕, directory = 𝓕
                               , sync = 𝓕
                               }

--------------------

{- | OpenFileFlags suitable for read-write opens /with pre-truncation/
     (analogous to writeFlags) . -}
readWriteNoTruncFlags ∷ OpenFileFlags
readWriteNoTruncFlags = OpenFileFlags { append = 𝓕, exclusive = 𝓕, noctty = 𝓕
                                      , nonBlock = 𝓕, trunc = 𝓕, nofollow = 𝓕
                                      , creat = 𝓝, cloexec = 𝓕, directory = 𝓕
                                      , sync = 𝓕
                                      }

--------------------

{- | OpenFileFlags suitable for read-write opens, with exclusive (file must
     not pre-exist (man file(2):O_EXCL). -}
readWriteExFlags ∷ OpenFileFlags
readWriteExFlags = OpenFileFlags { append = 𝓕, exclusive = 𝓣, noctty = 𝓕
                                 , nonBlock = 𝓕, trunc = 𝓕, nofollow = 𝓕
                                 , creat = 𝓝, cloexec = 𝓕, directory = 𝓕
                                 , sync = 𝓕
                                 }

--------------------

{- | OpenFileFlags suitable for writing /with pre-truncation/; this is just the
      `trunc` (man file(2):O_TRUNC) flag. -}
writeFlags ∷ OpenFileFlags
writeFlags = OpenFileFlags { append = 𝓕, exclusive = 𝓕, noctty = 𝓕
                           , nonBlock = 𝓕, trunc = 𝓣, nofollow = 𝓕
                           , creat = 𝓝, cloexec = 𝓕, directory = 𝓕, sync = 𝓕 }

--------------------

{- | OpenFileFlags suitable for writing /without pre-truncating/. -}
writeNoTruncFlags ∷ OpenFileFlags
writeNoTruncFlags = OpenFileFlags { append = 𝓕, exclusive = 𝓕, noctty = 𝓕
                                  , nonBlock = 𝓕 , trunc = 𝓕, nofollow = 𝓕
                                  , creat = 𝓝, cloexec = 𝓕, directory = 𝓕
                                  , sync = 𝓕 }

--------------------

{- | OpenFileFlags suitable for writing a new file; this is just the `exclusive`
     (man file(2):O_EXCL) flag.
     This seems redundant in practice, but I've added it here as a belt'n'braces
     thing.
-}
writeExFlags ∷ OpenFileFlags
writeExFlags = OpenFileFlags { append = 𝓕, exclusive = 𝓣, noctty = 𝓕
                             , nonBlock = 𝓕, trunc = 𝓕, nofollow = 𝓕
                             , creat = 𝓝, cloexec = 𝓕, directory = 𝓕
                             , sync = 𝓕 }

--------------------

{- | OpenFileFlags suitable for appending; this is just the `append`
     (man file(2):O_APPEND) flag. -}
appendFlags ∷ OpenFileFlags
appendFlags = OpenFileFlags { append = 𝓣, exclusive = 𝓕, noctty = 𝓕
                            , nonBlock = 𝓕, trunc = 𝓕, nofollow = 𝓕
                            , creat = 𝓝, cloexec = 𝓕, directory = 𝓕
                            , sync = 𝓕 }

----------------------------------------

openFile_ ∷ ∀ γ μ . (MonadIO μ, FileAs γ) ⇒
            HEncoding → FileOpenMode → γ → μ ℍ
openFile_ enc fomode (review _File_ → fn) = do
  let (mode,flags) = fileOpenMode fomode
      openMode ReadMode      = ReadOnly
      openMode WriteMode     = WriteOnly
      openMode ReadWriteMode = ReadWrite
      openMode AppendMode    = WriteOnly
      flags'   = case mode of
                   AppendMode → flags { append = 𝓣 }
                   _          → flags
  h ← liftIO $ openFd (fn ⫥ filepath) (openMode mode) flags' ≫ fdToHandle
  hSetEncoding h enc
  return $ ℍ h fn mode

----------------------------------------

openFile ∷ ∀ ε γ μ .
           (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ, HasCallStack) =>
           HEncoding → FileOpenMode → γ → μ ℍ
openFile enc fomode fn = asIOError $ openFile_ enc fomode fn

----------------------------------------

{- | An open RW handle to /dev/null. -}
devnull ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack) => μ ℍ
devnull = openFile Binary (FileRWNoTrunc 𝓝) [absfile|/dev/null|]

----------------------------------------

{- | Perform an IO action (that may throw a `MonadError`) in the context of an
     open filehandle . -}
withFile ∷ ∀ ε α γ μ .
           (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ, HasCallStack) ⇒
           HEncoding → FileOpenMode → γ → (ℍ → ExceptT ε IO α) → μ α
withFile enc fomode fn io =
  join $ asIOError $ bracket (openFile_ enc fomode fn) hClose (ѥ ∘ io)

--------------------

readFile ∷ ∀ ε τ γ μ .
           (MonadIO μ, FileAs γ,
            AsIOError ε, MonadError ε μ, HasCallStack, HGetContents τ) ⇒
           γ → μ τ
readFile fn = let result = withFile enc FileR fn hGetContents
                  enc    = impliedEncodingM result
               in result

readFileY ∷ ∀ ε τ γ μ .
            (MonadIO μ, FileAs γ,
             AsIOError ε, MonadError ε μ, HasCallStack, HGetContents τ) ⇒
            γ → μ (𝕄 τ)
readFileY = squashNoSuchThingT ∘ readFile

----------------------------------------

{- | Write a file in an implied encoding (see `impliedEncoding`).
     `perms`, if not 𝓝, will be used to create the file if it doesn't
     exist.  If it does exist, `perms` has no impact (use `chmod` to really
     force it).  If `perms is 𝓝, and the file does not exist, then an
     exception shall be thrown.
 -}
writeFile ∷ ∀ ε τ γ μ .
            (MonadIO μ, HWriteContents τ, FileAs γ,
             AsIOError ε, MonadError ε μ, HasCallStack) ⇒
            𝕄 FileMode → γ → τ → μ ()
writeFile perms fn t =
  withFile (impliedEncoding t) (FileW perms) fn (flip hWriteContents t)

--------------------

writeNoTruncFile ∷ ∀ ε τ γ μ .
                   (MonadIO μ, HWriteContents τ, FileAs γ,
                    AsIOError ε, MonadError ε μ, HasCallStack) ⇒
                   𝕄 FileMode → γ → τ → μ ()
writeNoTruncFile perms fn t =
  withFile (impliedEncoding t) (FileWNoTrunc perms) fn (flip hWriteContents t)

--------------------

writeExFile ∷ ∀ ε τ γ μ .
              (MonadIO μ, HWriteContents τ, FileAs γ,
               AsIOError ε, MonadError ε μ, HasCallStack) ⇒
              FileMode → γ → τ → μ ()
writeExFile perms fn t =
  withFile (impliedEncoding t) (FileWEx perms) fn (flip hWriteContents t)

----------------------------------------

{- | Append to a file.  `perms`, if not 𝓝, will be used to create the file
     if it doesn't exist.  If it does exist, `perms` has no impact (use `chmod`
     to really force it).  If `perms is 𝓝, and the file does not exist,
     then an exception shall be thrown.
 -}
appendFile ∷ ∀ ε τ γ μ .
             (MonadIO μ, HWriteContents τ, FileAs γ,
              AsIOError ε, MonadError ε μ, HasCallStack) ⇒
             𝕄 FileMode → γ → τ → μ ()
appendFile perms fn t =
  withFile (impliedEncoding t) (FileA perms) fn (flip hWriteContents t)

------------------------------------------------------------

withFileTests ∷ TestTree
withFileTests =
  let f = [absfile|/tmp/monadio-file-test.txt|]
      txt = "Swap twenty bottles for an aqua-walkman"
      t2  = "Medicine Show: "
      assertIsRight ∷ Either IOError () → Assertion
      assertIsRight = assertRight (\ _ → () @=? ())
      read ∷ FileAs γ ⇒ γ → IO (Either IOError 𝕋)
      read fn = ѥ $ readFile fn
      write ∷ FileAs γ ⇒ 𝕄 FileMode → γ → 𝕋 → IO (Either IOError ())
      write perms fn t = ѥ $ writeFile perms fn t
      writeNoTrunc ∷ FileAs γ ⇒ 𝕄 FileMode → γ → 𝕋 → IO (Either IOError ())
      writeNoTrunc perms fn t = ѥ $ writeNoTruncFile perms fn t
      -- `append` is imported from System.Posix.IO, so don't shadow that
      appnd ∷ FileAs γ ⇒ 𝕄 FileMode → γ → 𝕋 → IO (Either IOError ())
      appnd perms fn t = ѥ $ appendFile perms fn t
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
      testNotExists fn =
        let name = [fmt|'%T' does not exist|] fn
        in  testCase name $ ѥ @IOError (lfexists' fn) ≫assertRight (≟ NoFExists)
   in dependentTestGroup "withFile" AllSucceed
                         [ testNotExists f
                           -- WRITE NEW FILE NO PERMS, CHECK FOR FAILURE
                         , testWriteFail 𝓝 f txt
                         , testWrite (𝓙 0o600) f txt
                         , testRead f txt
                         -- re-write, to check for lack of auto-truncation
                         , testWriteNoTrunc (𝓙 0o600) f t2
                         , testRead f (t2 ⊕ drop (length t2) txt)
                         , testAppend (𝓙 0o600) f txt
                         , testRead f (t2 ⊕ drop (length t2) txt ⊕ txt)
                         -- DELETE
                         , testCase "delete" $ ѥ (unlink f) ≫ assertIsRight
                         -- TEST READ FAIL
                         , testReadFail f
                         -- APPEND NEW FAIL
                         , testAppendFail 𝓝 f txt
                         , testAppend (𝓙 0o000) f txt
                         -- TEST READ FAIL
                         , testReadFail f
                         , testCase "chmod" $ ѥ (chmod 0400 f) ≫ assertIsRight
                         -- DELETE
                         , testCase "delete" $ ѥ (unlink f) ≫ assertIsRight
                         -- make sure it's gone
                         , testNotExists f
                         ]

----------------------------------------

{- | Read a file in UTF8 encoding using OS-specific line-ending handling.
     Replace any invalid input bytes with the Unicode replacement character
     U+FFFD.
-}
-- plagiarized from https://www.snoyman.com/blog/2016/12/beware-of-readfile
readFileUTF8Lenient ∷ ∀ ε γ μ .
                      (AsIOError ε, MonadError ε μ, HasCallStack, MonadIO μ,
                       FileAs γ) =>
                      γ → μ 𝕋
readFileUTF8Lenient fn = decodeUtf8With lenientDecode ⊳ readFile fn

readFileUTF8LenientY ∷ ∀ ε γ μ .
                       (AsIOError ε, MonadError ε μ, HasCallStack, MonadIO μ,
                        FileAs γ) =>
                       γ → μ (𝕄 𝕋)
readFileUTF8LenientY = squashNoSuchThingT ∘ readFileUTF8Lenient

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
