{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module MonadIO.Directory
  ( chdir, inDir, mkdir, mkpath, nuke )
where

-- base --------------------------------

import Control.Monad           ( filterM, forM_, join, return, when )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Function           ( ($) )
import Data.Functor            ( fmap )
import Data.Maybe              ( Maybe( Just, Nothing ) )
import GHC.Stack               ( HasCallStack )
import System.IO               ( IO )
import System.Posix.Types      ( FileMode )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )

-- directory ---------------------------

import System.Directory  ( createDirectory, removePathForcibly
                         , setCurrentDirectory, withCurrentDirectory )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadCatch, onException )

-- fpath -------------------------------

import FPath.AsFilePath  ( AsFilePath, filepath )
import FPath.Dir         ( DirAs( _Dir_ ) )
import FPath.DirType     ( DirType )
import FPath.Parent      ( HasParentMay, parents' )

-- lens --------------------------------

import Control.Lens.Review  ( review )

-- monaderror-io -----------------------

import MonadError           ( ѥ )
import MonadError.IO        ( ӝ, asIOError )
import MonadError.IO.Error  ( AsIOError, IOError )

-- more-unicode ------------------------

import Data.MoreUnicode.Monad  ( (⪼) )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )

-- safe --------------------------------

import Safe  ( headMay )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.Base   ( chmod )
import MonadIO.FStat  ( FExists( FExists, NoFExists ), fexists, lfexists )

--------------------------------------------------------------------------------

{- | Change working directory. -}
chdir ∷ ∀ ε δ μ .
        (MonadIO μ, DirAs δ, AsIOError ε, MonadError ε μ, HasCallStack) ⇒
        δ    -- ^ directory to change to
      → μ ()
chdir (review filepath → d) = asIOError $ setCurrentDirectory d

----------------------------------------

{- | Perform IO with the dir *temporarily* changed to a given directory. -}
inDir ∷ (MonadIO μ, DirAs δ, AsIOError ε, MonadError ε μ, HasCallStack) ⇒
        δ              -- ^ directory to work in
      → ExceptT ε IO α -- ^ IO to perform in the given directory
      → μ α
inDir (review $ filepath ∘ _Dir_ → d) io =
  join ∘ asIOError $ withCurrentDirectory d (ѥ io)

----------------------------------------

{- | Forcibly remove a file or directory (including any descendents). -}
nuke ∷ ∀ ε ρ μ .
       (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack, AsFilePath ρ) ⇒
       ρ    -- ^ file/dir to remove
     → μ ()
nuke (review filepath → fp) = asIOError $ removePathForcibly fp

----------------------------------------

{- | Create a (single) directory.  Will error if the directory already exists
     (either as a directory or a file), or the parent directory does not exist
     or is not writable by the current user.
 -}
mkdir ∷ ∀ ε δ μ .
        (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack, DirAs δ) ⇒
        δ        -- ^ directory to create
      → FileMode -- ^ permissions for the directory
      → μ ()
mkdir d p = do
  let _mkdir = asIOError ∘ createDirectory ∘ (review $ filepath ∘ _Dir_)
  pre_exists ← lfexists d
  asIOError $ onException (ӝ $ _mkdir d ⪼ chmod @IOError p d)
                          (ӝ $ when (FExists ≡ pre_exists) $ nuke @IOError d)

----------------------------------------

{- | Create all missing elements of a path.
     The complex type signature in practice roughly equates `δ` to `Dir` or
     `AbsDir` or `RelDir`.
     Directories that are newly created are given the perms specified as `p`.
     Pre-existing directories are untouched.
     In case of error, newly-made directories are removed; pre-existing
     directories are left in place.
 -}
mkpath ∷ ∀ ε δ μ . (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack,
                    MonadCatch μ, DirAs δ,
                    HasParentMay δ, HasParentMay (DirType δ),
                    DirType δ ~ DirType (DirType δ), δ ~ DirType δ) ⇒
         δ        -- ^ directory to create
       → FileMode -- ^ permissions to apply to any *newly created* directories
       → μ ()
mkpath d p = do
  to_make ← filterM (fmap (≡ NoFExists) ∘ fexists) (parents' d)
  case headMay to_make of
    Nothing → return () -- nothing to do, all exist
    Just t  → -- make the intervening dirs, carefully; in case of any error,
              -- try to nuke those we freshly made
              onException (forM_ to_make (\ a → mkdir a p)) (nuke t)

-- that's all, folks! ----------------------------------------------------------
