{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnicodeSyntax              #-}

{- | User lookup (from `/etc/passwd`, etc.) with MonadIO, FPath, etc. -}
module MonadIO.User
  ( UserName
  , UserPwEnt
  , getPwBy
  , getPwBy'
  , getUserName
  , getUserName'
  , getpwuid
  , getuid
  , homeDir
  , homeDirectory
  , homePath
  , pwUID
  , userDir
  , userName
  , userPwEntFromUserEntry
  ) where

import Base1T

-- base --------------------------------

import Data.Type.Equality  ( type(~) )

-- fpath -------------------------------

import FPath.AbsDir           ( AbsDir, parseAbsDirP )
import FPath.AppendableFPath  ( AppendableFPath, AppendableFPathD,
                                AppendableFPathF, (⫻) )
import FPath.Error.FPathError ( AsFPathError )

-- lens --------------------------------

import Control.Lens.Getter ( view )

-- monaderror-io -----------------------

import MonadError.IO.Error ( squashNoSuchThing, throwUserError )

-- unix --------------------------------

import System.Posix.User qualified as PosixUser

import System.Posix.Types           ( UserID )
import System.Posix.User            ( getRealUserID, getUserEntryForID )
import System.Posix.User.ByteString ( UserEntry )

------------------------------------------------------------

-- | a user name, as found in the pw table
newtype UserName = UserName 𝕋
  deriving newtype (Printable, Show)

------------------------------------------------------------

{- | An entry in the pw table (e.g., `/etc/passwd`). -}
data UserPwEnt = UserPwEnt { _userName :: UserName
                           , _userDir  :: AbsDir
                           }
  deriving (Show)

{-| lens for homedir of `UserPwEnt` -}
userDir ∷ Lens' UserPwEnt AbsDir
userDir = lens _userDir (\ upe ud → upe { _userDir = ud })

{-| lens for username of `UserPwEnt` -}
userName ∷ Lens' UserPwEnt UserName
userName = lens _userName (\ upe un → upe { _userName = un })

{- | Convert a `UserEntry` to a `UserPwEnt`; throws error if the home dir is not
     a valid abs dir. -}
userPwEntFromUserEntry ∷ ∀ ε η . (AsFPathError ε, MonadError ε η) ⇒
                         UserEntry → η UserPwEnt
userPwEntFromUserEntry ue = do
  hD ← parseAbsDirP $ PosixUser.homeDirectory ue
  let uN = UserName (toText $ PosixUser.userName ue)
  return $ UserPwEnt { _userName = uN, _userDir  = hD }

----------------------------------------

-- | the pw entry, for the given user selector fn & value; error if not exists
getPwBy' ∷ ∀ ε α μ . (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ) ⇒
           (α → IO UserEntry) → α → μ UserPwEnt
getPwBy' f a = join ∘ asIOError $ userPwEntFromUserEntry ⊳ f a

----------------------------------------

{- | The pw entry, if one exists, for the given user selector fn & value. -}
getPwBy ∷ ∀ ε α μ . (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ) ⇒
          (α → IO UserEntry) → α → μ (𝕄 UserPwEnt)
getPwBy f a = join $ squashNoSuchThing ⊳ ѥ (getPwBy' f a)

{- | Like `getpwuid', but (IO)Error if we're missing an entry for the given
     UID. -}
pwUID ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ) ⇒
         UserID → μ UserPwEnt
pwUID = getPwBy' $ getUserEntryForID

----------------------------------------

{- | The home directory (per getpw*, i.e., /etc/passwd or equivalent) of the
     current Real user (that return by getuid).
     Returns nothing if the pw db has no entry for the current user.
     Errors if reading the pw db errors, or if the value provided is not a valid
     absolute directory.
 -}
homeDirectory ∷ ∀ ε μ . (MonadIO μ, AsIOError ε,AsFPathError ε,MonadError ε μ) ⇒
                μ (𝕄 AbsDir)
homeDirectory = (view userDir ⊳⊳) $ getuid ≫ getpwuid

----------------------------------------

{- | Like `homeDirectory`, but throws if the getuid entry isn't found. -}
homeDir ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ) ⇒
          μ AbsDir
homeDir = fmap (view userDir) $ getuid ≫ pwUID

----------------------------------------

{- | Construct an absolute dir/file from a relative dir/file, prepended with
     the home dir. -}
homePath ∷ ∀ ε μ β . (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ,
                      AppendableFPath β, AppendableFPathD β ~ AbsDir) ⇒
           AppendableFPathF β → μ β
homePath p = homeDir ≫ return ∘ (⫻ p)

----------------------------------------

{- | Real user ID of the calling process. -}
getuid ∷ MonadIO μ ⇒ μ UserID
getuid = liftIO getRealUserID

----------------------------------------

{- | The pw entry, if one exists, for the given user ID. -}
getpwuid ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ) ⇒
           UserID → μ (𝕄 UserPwEnt)
getpwuid = getPwBy getUserEntryForID

----------------------------------------

{-| the current user name -}
getUserName ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, AsFPathError ε, MonadError ε μ) ⇒
              μ (𝕄 UserName)
getUserName = fmap _userName ⊳ (getuid ≫ getpwuid)

----------------------------------------

{-| the current user name; throws an error if getpwuid can't find the uid -}
getUserName' ∷ (MonadIO μ,
                AsIOError ε, AsFPathError ε, Printable ε, MonadError ε μ) ⇒
               μ UserName
getUserName' = do
  uid ← getuid
  getpwuid uid ≫ \ case
    𝓙 user_pw_ent → return (user_pw_ent ⊣ userName)
    𝓝             → throwUserError $ [fmtT|no passwd entry found for %d|] uid

-- that's all, folks! ----------------------------------------------------------
