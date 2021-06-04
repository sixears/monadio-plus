module MonadIO.Process.CmdSpec
  ( CmdArgs( CmdArgs, unCmdArgs ), CmdExe(..), CmdSpec(..), CreateGroup(..)
  , HasCmdArgs(..), HasCmdExe(..), HasCmdSpec(..), mkCmd, mkCmd'
  )
where

-- base --------------------------------

import Data.Eq         ( Eq )
import Data.Function   ( ($), (&), id )
import Data.Maybe      ( catMaybes, maybe )
import Text.Show       ( Show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toText )

-- env-plus ----------------------------

import Env.Types  ( Env, fromList )

-- fpath -------------------------------

import FPath.AbsDir      ( AbsDir, root )
import FPath.AbsFile     ( AbsFile )
import FPath.AsFilePath  ( AsFilePath( filepath ) )

-- lens --------------------------------

import Control.Lens.Lens    ( Lens', lens )
import Control.Lens.Prism   ( prism )

-- more-unicode ------------------------

import Data.MoreUnicode.Either   ( pattern 𝕷, pattern 𝕽 )
import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⊣), (⫥), (⩼), (⊢) )
import Data.MoreUnicode.Maybe    ( 𝕄, pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.String   ( 𝕊 )
import Data.MoreUnicode.Text     ( 𝕋 )

-- process -----------------------------

import System.Process  ( showCommandForUser )

-- text --------------------------------

import Data.Text  ( intercalate, pack, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

--------------------------------------------------------------------------------

-- CmdExe --------------------------------------------------

{- | The path to the executable. -}
newtype CmdExe = CmdExe AbsFile
  deriving (Eq,Show)

class HasCmdExe α where
  cmdExe ∷ Lens' α CmdExe

instance HasCmdExe CmdExe where
  cmdExe = id

instance AsFilePath CmdExe where
  filepath = prism (\ (CmdExe f) → f ⫥ filepath)
                   (\ f → maybe (𝕷 f) (𝕽 ∘ CmdExe) (f ⩼ filepath))

-- CmdArgs -------------------------------------------------

newtype CmdArgs = CmdArgs { unCmdArgs ∷ [𝕋] }
  deriving (Eq,Show)

class HasCmdArgs α where
  cmdArgs  ∷ Lens' α CmdArgs
  cmdArgsT ∷ Lens' α [𝕋]
  cmdArgsT = lens (\ a → unCmdArgs (a ⊣ cmdArgs))
                  (\ a ts → a & cmdArgs ⊢ (CmdArgs ts))
  cmdArgsS ∷ Lens' α [𝕊]
  cmdArgsS = lens (\ a → unpack ⊳ a ⊣ cmdArgsT)
                  (\ a ss → a & cmdArgsT ⊢ (pack ⊳ ss))

instance HasCmdArgs CmdArgs where
  cmdArgs = id

-- CreateGroup ---------------------------------------------

data CreateGroup = CreateGroup | NoCreateGroup
  deriving (Eq, Show)

-- CmdSpec -------------------------------------------------

data CmdSpec = CmdSpec { _cmdExe      ∷ CmdExe
                       , _cmdArgs     ∷ CmdArgs
                       , _cwd         ∷ 𝕄 AbsDir
                       , _env         ∷ 𝕄 Env
                       , _createGroup ∷ CreateGroup
                        -- Function name (for error messages), not necessarily
                        -- the executable name.
                        -- see `System.Process.createProcess_`
                       , _cmdName      ∷ 𝕄 𝕋
                       }
  deriving (Eq,Show)

instance HasCmdExe CmdSpec where
  cmdExe ∷ Lens' CmdSpec CmdExe
  cmdExe = lens _cmdExe (\ s e → s { _cmdExe = e })

instance HasCmdArgs CmdSpec where
  cmdArgs ∷ Lens' CmdSpec CmdArgs
  cmdArgs = lens _cmdArgs (\ s as → s { _cmdArgs = as })

class (HasCmdExe α, HasCmdArgs α) ⇒ HasCmdSpec α where
  cmdSpec     ∷ Lens' α CmdSpec
  cmdName     ∷ Lens' α (𝕄 𝕋)
  cmdName     = cmdSpec ∘ cmdName
  createGroup ∷ Lens' α CreateGroup
  createGroup = cmdSpec ∘ createGroup
  cwd         ∷ Lens' α (𝕄 AbsDir)
  cwd         = cmdSpec ∘ cwd
  env         ∷ Lens' α (𝕄 Env)
  env         = cmdSpec ∘ env

instance HasCmdSpec CmdSpec where
  cmdSpec     = id
  cmdName     = lens _cmdName     (\ cpo nm  → cpo { _cmdName     = nm  })
  createGroup = lens _createGroup (\ cpo cg  → cpo { _createGroup = cg  })
  cwd         = lens _cwd         (\ cpo wd  → cpo { _cwd         = wd  })
  env         = lens _env         (\ cpo nv  → cpo { _env         = nv  })

{- | The exe+args in a shell-safe form (using `showCommandForUser`). -}
cmdStr ∷ CmdSpec → 𝕊
cmdStr c = showCommandForUser ((c ⊣ cmdExe) ⫥ filepath) (c ⊣ cmdArgsS)

instance Printable CmdSpec where
  print c = let ds t = "«" ⊕ t ⊕ "»"
                as t = "<" ⊕ t ⊕ ">"
                cg = if CreateGroup ≡ c ⊣ createGroup
                     then 𝕵 "(CreateGroup)"
                     else 𝕹
             in P.text ∘ intercalate " " $ catMaybes [ 𝕵 ∘ pack $ cmdStr c
                                                     , toText ⊳ c ⊣ env
                                                     , ds ∘ toText ⊳ c ⊣ cwd
                                                     , as ⊳ c ⊣ cmdName
                                                     , cg
                                                     ]

{- | Create a `CmdSpec` from an executable (`AbsFile`) and a set of args
     (`[Text]`). -}
mkCmd ∷ AbsFile → [𝕋] → CmdSpec
mkCmd exe args = CmdSpec { _cmdExe      = CmdExe exe
                         , _cmdArgs     = CmdArgs args
                         , _cwd         = 𝕹
                         , _env         = 𝕹
                         , _createGroup = NoCreateGroup
                         , _cmdName     = 𝕹
                         }

--------------------

{- | Like `mkCmd`, but safer; uses `/` as cwd, an empty environment and creates
     a process group. -}

mkCmd' ∷ AbsFile → [𝕋] → CmdSpec
mkCmd' exe args = CmdSpec { _cmdExe      = CmdExe exe
                         , _cmdArgs     = CmdArgs args
                         , _cwd         = 𝕵 root
                         , _env         = 𝕵 $ fromList []
                         , _createGroup = CreateGroup
                         , _cmdName     = 𝕹
                         }

-- that's all, folks! ----------------------------------------------------------
