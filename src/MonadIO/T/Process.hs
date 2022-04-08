module MonadIO.T.Process
  ( tests )
where

import Base1T

-- base --------------------------------

import Data.Maybe  ( isJust )

-- containers-plus ---------------------

import ContainersPlus.Insert  ( (⨭) )

-- exceptions --------------------------

import Control.Monad.Catch ( MonadMask )

-- fpath -------------------------------

import FPath.AbsFile  ( AbsFile, absfile )

-- lens --------------------------------

import Control.Lens.Fold  ( (^?) )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, assertBool, assertEqual )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), assertIOError )

-- text --------------------------------

import Data.Text  ( unlines )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

-- monadio-plus ------------------------

import qualified  MonadIO.Paths  as  Paths

import MonadIO.Error.CreateProcError  ( ProcError )
import MonadIO.Error.ProcExitError    ( ProcExitError, _ProcExitError
                                      , stdErr, stdOut )
import MonadIO.File                   ( devnull )
import MonadIO.Process                ( system )
import MonadIO.Process.CmdSpec        ( CmdArgs( CmdArgs ), CmdExe( CmdExe )
                                      , cmdArgs, cmdExe, expExitVal, mkCmd )
import MonadIO.Process.ExitInfo       ( ExitInfo )
import MonadIO.Process.ExitStatus     ( ExitStatus( ExitVal ), exitVal )
import MonadIO.Process.MkInputStream  ( MkInputStream )

--------------------------------------------------------------------------------

foo ∷ 𝕋
foo = unlines [ "jimmy 7"
              , "martyn 12"
              , "marbyns 3"
              ]

grep_ ∷ (MonadIO μ, MkInputStream σ, MonadError ProcError μ) ⇒
        [𝕋] → σ → μ (ExitInfo, (𝕋,𝕋))
grep_ args input =
  let cmd = mkCmd Paths.grep args & expExitVal ⨭ 1
   in system input cmd

grep ∷ (MonadIO μ, MkInputStream σ, MonadError ProcError μ) ⇒
       𝕋 → σ → μ (ExitInfo, (𝕋,𝕋))
grep pat input = grep_ [pat] input

{- | Like `grep`, but passes in an `AbsFile` rather than piping in the data. -}
grepaf ∷ (MonadIO μ, MonadError ProcError μ) ⇒
         𝕋 → AbsFile → μ (ExitInfo, (𝕋,𝕋))
grepaf pat fn = devnull ≫ grep_ [pat, toText fn] -- [absfile|/dev/null|]

-- for repl use

{- | grep a pattern from some `Text`; capture the logs (for debugging). -}
_grep_ ∷ (MonadIO μ, MkInputStream σ) ⇒
        𝕋 → σ → μ (𝔼 ProcError (ExitInfo, (𝕋,𝕋)))
_grep_ pat input = ѥ $ grep pat input

{- | grep a pattern from some `Text`; write the logs to stderr (for
     debugging). -}
_grep ∷ (MonadIO μ, MonadMask μ, MkInputStream σ) ⇒
        𝕋 → σ → μ (𝔼 ProcError (ExitInfo, (𝕋,𝕋)))
_grep pat input = ѥ $ grep_ [pat] input

{- | Perform a list of tests independently against the result of an IO.
     Note that the IO is performed once for each test. -}
ioTests ∷ ∀ ρ ε . Show ε ⇒
          TestName → IO (𝔼 ε ρ) → [(TestName, ρ → Assertion)] → TestTree
ioTests nm s xs =
  testGroup nm $
    ( \ (n,f ∷ ρ → Assertion) → testCase n $ s ≫ \ x → assertRight f x) ⊳ xs

----------------------------------------

data ProcResult = ProcResult { exit ∷ ExitInfo
                             , out  ∷ 𝕋
                             , err  ∷ 𝕋
                             }

mkProcResult ∷ ((ExitInfo, (𝕋,𝕋))) → ProcResult
mkProcResult (ex,(ot,er)) = ProcResult ex ot er

{- | Test the results of an external process.  Note that the proc is run
     multiple times (once for each test). -}
testProc ∷ TestName
         → IO (𝔼 ProcError ProcResult)
         → Word8                                -- ^ expected exit
         → 𝕋                                    -- ^ expected stdout
         → 𝕋                                    -- ^ expected stderr
         → TestTree
testProc nm s expExit expOut expErr =
  ioTests nm s $ [ ("exit",   (\ r → ExitVal expExit ≟ exit r ⊣ exitVal))
                 , ("stdout", (\ r → expOut  ≟ out r))
                 , ("stderr", (\ r → expErr  ≟ err r))
                 ]

----------------------------------------

tests ∷ TestTree
tests =
  let
    p     ∷ 𝕋 → IO (𝔼 ProcError ProcResult)
    p t   = mkProcResult ⊳⊳ (ѥ @ProcError $ grep t foo)
    grepf = grepaf "x" [absfile|/nonesuch|]
    testErrs ∷ (Eq α, Show α) ⇒
               𝕊 → (Lens' ProcExitError α) → α → Assertion
    testErrs n f x =
      assertIOError (\ e → assertEqual n (𝕵 x) (e ^? _ProcExitError ∘ f)) grepf
    testE ∷ (Eq α, Printable α, Show α) ⇒
              (Lens' ProcExitError α) → α → Assertion
    testE f x =
      testErrs (toString x) f x
    testErr' ∷ (Eq α, Printable α, Show α) ⇒
               (Lens' ProcExitError α) → α → TestTree
    testErr' f x =
      testCase (toString x) $ testE f x
  in
    testGroup
      "Process"
      [ -- grep matches 'martyn', exits 0
        testProc "grep martyn" (p "martyn") 0 "martyn 12\n" ""
      , -- grep matches nothing, exits 1
        testProc "grep john"   (p "john")   1 "" ""
      , -- grep exits 2 due to missing file
        testGroup "missing file"
          [
            let
              isProcExitError e =
                assertBool (toString e) $ isJust (e ^? _ProcExitError)
             in
              testCase "ProcExitError" $ assertIOError isProcExitError grepf
          , testErr' exitVal (ExitVal 2)
          , testErr' cmdExe (CmdExe Paths.grep)
          , testErr' cmdArgs (CmdArgs ["x","/nonesuch"])
          , testCase "stdout" $ testErrs "stdout" stdOut (𝕵 "")
          , let
              msg = [fmt|%T: /nonesuch: No such file or directory\n|] Paths.grep
             in
              testCase "stderr" $ testErrs "stderr" stdErr (𝕵 msg)
          ]
      ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
