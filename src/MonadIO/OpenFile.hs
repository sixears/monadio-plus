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
  ( FileOpenMode(..), HEncoding( Binary, NoEncoding, UTF8 )
  , appendFile, openFile, readFile, readFileUTF8Lenient, withFile, writeExFile
  , writeFile, writeNoTruncFile

  , appendFlags, readFlags, readWriteExFlags, readWriteFlags
  , readWriteNoTruncFlags, writeExFlags, writeFlags, writeNoTruncFlags
  , tests

  )
where

-- base --------------------------------

import qualified  System.IO

import Control.Monad           ( join, return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Either             ( Either )
import Data.Function           ( ($), flip )
import Data.String             ( String )
import GHC.Stack               ( HasCallStack )
import System.Exit             ( ExitCode )
import System.IO               ( Handle, IO
                               , IOMode( AppendMode, ReadMode, ReadWriteMode
                                       , WriteMode )
                               )
import System.Posix.Types      ( FileMode )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

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

import Data.MoreUnicode.Bool     ( pattern 𝕿, pattern 𝕱 )
import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⫥) )
import Data.MoreUnicode.Maybe    ( 𝕄, pattern 𝕵, pattern 𝕹 )
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

import MonadIO.Base    ( chmod, unlink )
import MonadIO.Handle  ( HEncoding( Binary, NoEncoding, UTF8 )
                       , HGetContents( hGetContents )
                       , HWriteContents( hWriteContents )
                       , ImpliedEncoding( impliedEncoding )
                       , hSetEncoding, impliedEncodingM
                       )

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

fileOpenMode ∷ FileOpenMode → (IOMode, OpenFileFlags, 𝕄 FileMode)
fileOpenMode FileR                 = (ReadMode     , readFlags        , 𝕹)
fileOpenMode (FileRW        perms) = (ReadWriteMode, readWriteFlags   , perms)
fileOpenMode (FileRWEx      perms) = (ReadWriteMode, readWriteExFlags , 𝕵 perms)
fileOpenMode (FileRWNoTrunc perms) = (ReadWriteMode, writeNoTruncFlags, perms)
fileOpenMode (FileA         perms) = (AppendMode   , appendFlags      , perms)
fileOpenMode (FileW         perms) = (WriteMode    , writeFlags       , perms)
fileOpenMode (FileWEx       perms) = (WriteMode    , writeExFlags     , 𝕵 perms)
fileOpenMode (FileWNoTrunc  perms) = (WriteMode    , writeNoTruncFlags, perms)
fileOpenMode (FileOpenMode(m,f,p)) = (m            , f                , p)

------------------------------------------------------------

{- | OpenFileFlags suitable for reading. -}
readFlags ∷ OpenFileFlags
readFlags = OpenFileFlags { append = 𝕱, exclusive = 𝕱, noctty = 𝕱,
                            nonBlock = 𝕱, trunc = 𝕱 }
--------------------

{- | OpenFileFlags suitable for read-write opens /with pre-truncation/
     (analogous to writeFlags) . -}
readWriteFlags ∷ OpenFileFlags
readWriteFlags = OpenFileFlags { append = 𝕱, exclusive = 𝕱
                               , noctty = 𝕱, nonBlock = 𝕱
                               , trunc = 𝕿
                               }

--------------------

{- | OpenFileFlags suitable for read-write opens /with pre-truncation/
     (analogous to writeFlags) . -}
readWriteNoTruncFlags ∷ OpenFileFlags
readWriteNoTruncFlags = OpenFileFlags { append = 𝕱, exclusive = 𝕱
                                      , noctty = 𝕱, nonBlock = 𝕱
                                      , trunc = 𝕱
                                      }

--------------------

{- | OpenFileFlags suitable for read-write opens, with exclusive (file must
     not pre-exist (man file(2):O_EXCL). -}
readWriteExFlags ∷ OpenFileFlags
readWriteExFlags = OpenFileFlags { append = 𝕱, exclusive = 𝕿
                                 , noctty = 𝕱, nonBlock = 𝕱
                                 , trunc = 𝕱
                                 }

--------------------

{- | OpenFileFlags suitable for writing /with pre-truncation/; this is just the
      `trunc` (man file(2):O_TRUNC) flag. -}
writeFlags ∷ OpenFileFlags
writeFlags = OpenFileFlags { append = 𝕱, exclusive = 𝕱, noctty = 𝕱
                           , nonBlock = 𝕱, trunc = 𝕿 }

--------------------

{- | OpenFileFlags suitable for writing /without pre-truncating/. -}
writeNoTruncFlags ∷ OpenFileFlags
writeNoTruncFlags = OpenFileFlags { append = 𝕱, exclusive = 𝕱
                                  , noctty = 𝕱, nonBlock = 𝕱
                                  , trunc = 𝕱 }

--------------------

{- | OpenFileFlags suitable for writing a new file; this is just the `exclusive`
     (man file(2):O_EXCL) flag.
     This seems redundant in practice, but I've added it here as a belt'n'braces
     thing.
-}
writeExFlags ∷ OpenFileFlags
writeExFlags = OpenFileFlags { append = 𝕱, exclusive = 𝕿, noctty = 𝕱,
                               nonBlock = 𝕱, trunc = 𝕱 }

--------------------

{- | OpenFileFlags suitable for appending; this is just the `append`
     (man file(2):O_APPEND) flag. -}
appendFlags ∷ OpenFileFlags
appendFlags = OpenFileFlags { append = 𝕿, exclusive = 𝕱, noctty = 𝕱,
                              nonBlock = 𝕱, trunc = 𝕱 }

----------------------------------------

openFile'' ∷ (MonadIO μ, FileAs γ) ⇒
             HEncoding → IOMode → OpenFileFlags → 𝕄 FileMode
           → γ → μ Handle
openFile'' enc mode flags perms (review _File_ → fn) = liftIO $ do
  let openMode ReadMode      = ReadOnly
      openMode WriteMode     = WriteOnly
      openMode ReadWriteMode = ReadWrite
      openMode AppendMode    = WriteOnly
      flags'   = case mode of
                   AppendMode → flags { append = 𝕿 }
                   _          → flags
  h ← openFd (fn ⫥ filepath) (openMode mode) perms flags' ≫ fdToHandle
  hSetEncoding h enc
  return h

openFile_ ∷ (MonadIO μ, FileAs γ) ⇒
            HEncoding → FileOpenMode → γ → μ Handle
openFile_ enc fomode = let (mode,flags,perms) = fileOpenMode fomode
                        in openFile'' enc mode flags perms

----------------------------------------

openFile ∷ (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ, HasCallStack) ⇒
           HEncoding → FileOpenMode → γ → μ Handle
openFile enc fomode fn = asIOError $ openFile_ enc fomode fn

--------------------

readFile ∷ ∀ ε ω γ μ .
           (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ, HGetContents ω) =>
           γ -> μ ω
readFile fn = let result = withFile enc FileR fn hGetContents
                  enc = impliedEncodingM result
               in result

----------------------------------------

{- | Perform an IO action (that may throw a `MonadError`) in the context of an
     open filehandle . -}
withFile ∷ ∀ ε ω γ μ .
           (MonadIO μ, FileAs γ, AsIOError ε, MonadError ε μ, HasCallStack) ⇒
           HEncoding → FileOpenMode → γ → (Handle → ExceptT ε IO ω) → μ ω
withFile enc fomode fn io =
  join $ asIOError $ bracket (openFile_ enc fomode fn) System.IO.hClose (ѥ ∘ io)

{- | Write a file in an implied encoding (see `impliedEncoding`).
     `perms`, if not 𝕹, will be used to create the file if it doesn't
     exist.  If it does exist, `perms` has no impact (use `chmod` to really
     force it).  If `perms is 𝕹, and the file does not exist, then an
     exception shall be thrown.
 -}
writeFile ∷ ∀ ε τ γ μ .
            (MonadIO μ, HWriteContents τ, FileAs γ,
             AsIOError ε, MonadError ε μ, HasCallStack) ⇒
            𝕄 FileMode → γ → τ → μ ()
writeFile perms fn t =
  withFile (impliedEncoding t) (FileW perms) fn (flip hWriteContents t)

writeNoTruncFile ∷ ∀ ε τ γ μ .
                   (MonadIO μ, HWriteContents τ, FileAs γ,
                    AsIOError ε, MonadError ε μ, HasCallStack) ⇒
                   𝕄 FileMode → γ → τ → μ ()
writeNoTruncFile perms fn t =
  withFile (impliedEncoding t) (FileWNoTrunc perms) fn (flip hWriteContents t)

writeExFile ∷ ∀ ε τ γ μ .
              (MonadIO μ, HWriteContents τ, FileAs γ,
               AsIOError ε, MonadError ε μ, HasCallStack) ⇒
              FileMode → γ → τ → μ ()
writeExFile perms fn t =
  withFile (impliedEncoding t) (FileWEx perms) fn (flip hWriteContents t)

{- | Append to a file.  `perms`, if not 𝕹, will be used to create the file
     if it doesn't exist.  If it does exist, `perms` has no impact (use `chmod`
     to really force it).  If `perms is 𝕹, and the file does not exist,
     then an exception shall be thrown.
 -}
appendFile ∷ ∀ ε τ γ μ .
             (MonadIO μ, HWriteContents τ, FileAs γ,
              AsIOError ε, MonadError ε μ, HasCallStack) ⇒
             𝕄 FileMode → γ → τ → μ ()
appendFile perms fn t =
  withFile (impliedEncoding t) (FileA perms) fn (flip hWriteContents t)

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
   in testGroup "withFile"
                [ -- WRITE NEW FILE NO PERMS, CHECK FOR FAILURE
                  testWriteFail 𝕹 f txt
                , testWrite (𝕵 0o600) f txt
                , testRead f txt
                -- re-write, to check for lack of auto-truncation
                , testWriteNoTrunc (𝕵 0o600) f t2
                , testRead f (t2 ⊕ drop (length t2) txt)
                , testAppend (𝕵 0o600) f txt
                , testRead f (t2 ⊕ drop (length t2) txt ⊕ txt)
                -- DELETE
                , testCase "delete" $ ѥ (unlink f) ≫ assertIsRight
                -- TEST READ FAIL
                , testReadFail f
                -- APPEND NEW FAIL
                , testAppendFail 𝕹 f txt
                , testAppend (𝕵 0o000) f txt
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
readFileUTF8Lenient fn = decodeUtf8With lenientDecode ⊳ readFile fn

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

