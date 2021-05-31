module MonadIO.Process.CmdSpec
  ( CmdArgs( CmdArgs, unCmdArgs ), CmdExe(..), CmdSpec(..), HasCmdArgs(..)
  , HasCmdExe(..), HasCmdSpec(..)
  )
where

-- base --------------------------------

import Data.Either     ( Either( Left, Right ) )
import Data.Eq         ( Eq )
import Data.Function   ( ($), (&), id )
import Data.Maybe      ( maybe )
import Data.String     ( String )
import Text.Show       ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- fpath -------------------------------

import FPath.AbsFile     ( AbsFile )
import FPath.AsFilePath  ( AsFilePath( filepath ) )

-- lens --------------------------------

import Control.Lens.Lens    ( Lens', lens )
import Control.Lens.Prism   ( prism )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⊣), (⫥), (⩼), (⊢) )

-- process -----------------------------

import System.Process  ( showCommandForUser )

-- text --------------------------------

import Data.Text  ( Text, pack, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

--------------------------------------------------------------------------------

{- | The path to the executable. -}
newtype CmdExe = CmdExe AbsFile
  deriving (Eq,Show)

class HasCmdExe α where
  cmdExe ∷ Lens' α CmdExe

instance HasCmdExe CmdExe where
  cmdExe = id

instance AsFilePath CmdExe where
  filepath = prism (\ (CmdExe f) → f ⫥ filepath)
                   (\ f → maybe (Left f) (Right ∘ CmdExe) (f ⩼ filepath))

------------------------------------------------------------

newtype CmdArgs = CmdArgs { unCmdArgs ∷ [Text] }
  deriving (Eq,Show)

class HasCmdArgs α where
  cmdArgs  ∷ Lens' α CmdArgs
  cmdArgsT ∷ Lens' α [Text]
  cmdArgsT = lens (\ a → unCmdArgs (a ⊣ cmdArgs))
                  (\ a ts → a & cmdArgs ⊢ (CmdArgs ts))
  cmdArgsS ∷ Lens' α [String]
  cmdArgsS = lens (\ a → unpack ⊳ a ⊣ cmdArgsT)
                  (\ a ss → a & cmdArgsT ⊢ (pack ⊳ ss))

instance HasCmdArgs CmdArgs where
  cmdArgs = id

------------------------------------------------------------

data CmdSpec = CmdSpec { _cmdExe ∷ CmdExe, _cmdArgs ∷ CmdArgs }
  deriving (Eq,Show)

class HasCmdSpec α where
  cmdSpec ∷ Lens' α CmdSpec

instance HasCmdSpec CmdSpec where
  cmdSpec = id

instance HasCmdExe CmdSpec where
  cmdExe ∷ Lens' CmdSpec CmdExe
  cmdExe = lens _cmdExe (\ s e → s { _cmdExe = e })

instance HasCmdArgs CmdSpec where
  cmdArgs ∷ Lens' CmdSpec CmdArgs
  cmdArgs = lens _cmdArgs (\ s as → s { _cmdArgs = as })

instance Printable CmdSpec where
  print c = P.string $ showCommandForUser ((c ⊣ cmdExe) ⫥ filepath) (c ⊣ cmdArgsS)

-- that's all, folks! ----------------------------------------------------------
