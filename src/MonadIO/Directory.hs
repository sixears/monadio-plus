{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

{-| Directory operations, as MonadIO, with MonadError handling -}

module MonadIO.Directory
  ( chdir, __chdir__, directoryList, glob, inDir, listdir, listdirStdOut,
    listdirStdErr, lsdir, mkdir, mkpath, nuke, __nuke__, pwd, __pwd__ )
where

import Base1T

-- base --------------------------------

import Data.List           ( filter, sortBy )
import Data.Ord            ( compare )
import Data.Type.Equality  ( type (~) )
import System.IO           ( stderr )
import System.Posix.Types  ( FileMode )

-- directory ---------------------------

import System.Directory  ( createDirectory, listDirectory, getCurrentDirectory,
                           removePathForcibly, setCurrentDirectory,
                           withCurrentDirectory )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadCatch, onException )

-- fpath -------------------------------

import FPath.Abs               ( Abs( AbsD, AbsF ) )
import FPath.AbsDir            ( AbsDir )
import FPath.AbsFile           ( AbsFile )
import FPath.AppendableFPath   ( AppendableFPath, AppendableFPathF
                               , AppendableFPathD, (⫻) )
import FPath.AsFilePath        ( AsFilePath, filepath )
import FPath.Basename          ( Basename, basename )
import FPath.Dir               ( DirAs( _Dir_ ) )
import FPath.DirType           ( DirType )
import FPath.Error.FPathError  ( AsFPathError, FPathError, FPathIOError )
import FPath.Parent            ( HasParentMay, parents' )
import FPath.Parseable         ( parse, parseDir )
import FPath.RelFile           ( RelFile )
import FPath.RelType           ( RelType )
import FPath.ToDir             ( ToDir )

-- fstat -------------------------------

import FStat  ( FStat, ftypeLSTxt, permLSTxt, size )

-- monaderror-io -----------------------

import MonadError.IO        ( ӝ, asIOErrorT )
import MonadError.IO.Error  ( IOError )

-- regex-pcre --------------------------

import Text.Regex.PCRE  ( (=~) )

-- safe --------------------------------

import Safe  ( headMay )

-- text --------------------------------

import Data.Text.IO  ( hPutStrLn, putStrLn )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.Base   ( chmod )
import MonadIO.FStat  ( FExists( FExists, NoFExists )
                      , fexists, lfexists, lstats, lstat', pathTypes )

--------------------------------------------------------------------------------

{-| current working directory -}
pwd ∷ ∀ ε μ .
      (MonadIO μ, AsFPathError ε, MonadError ε μ, HasCallStack) => μ AbsDir
pwd = liftIO getCurrentDirectory ≫ parseDir

--------------------

{-| current working directory; like `pwd`, but errors thrown into IO -}
__pwd__ ∷ (MonadIO μ, HasCallStack) => μ AbsDir
__pwd__ = ӝ @FPathError pwd

----------------------------------------

{-| change working directory -}
chdir ∷ ∀ ε δ μ .
        (MonadIO μ, DirAs δ, AsIOError ε, MonadError ε μ, HasCallStack) =>
        δ {- ^ directory to change to -} → μ ()
chdir (review filepath → d) = asIOError $ setCurrentDirectory d

----------

{-| change working directory; like `chdir`, but errors thrown into IO -}
__chdir__ ∷ ∀ δ μ . (MonadIO μ, DirAs δ, HasCallStack) => δ → μ ()
__chdir__ = ӝ @IOError ∘ chdir

----------------------------------------

{-| perform IO with the dir *temporarily* changed to a given directory -}
inDir ∷ ∀ ε α δ μ .
        (MonadIO μ, DirAs δ, AsIOError ε, MonadError ε μ, HasCallStack) =>
        δ              -- ^ directory to work in
      → ExceptT ε IO α -- ^ IO to perform in the given directory
      → μ α
inDir (review $ filepath ∘ _Dir_ → d) io =
  join ∘ asIOError $ withCurrentDirectory d (ѥ io)

----------------------------------------

{-| forcibly remove a file or directory (including any descendents) -}
nuke ∷ ∀ ε ρ μ .
       (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack, AsFilePath ρ) =>
       ρ {- ^ file/dir to remove -} → μ ()
nuke (review filepath → fp) = asIOError $ removePathForcibly fp

----------

{-| like `nuke`, but errors thrown into IO -}
__nuke__ ∷ ∀ ρ μ . (MonadIO μ, DirAs ρ, HasCallStack) => ρ → μ ()
__nuke__ = ӝ @IOError ∘ nuke

----------------------------------------

{- | Create a (single) directory.  Will error if the directory already exists
     (either as a directory or a file), or the parent directory does not exist
     or is not writable by the current user.
 -}
mkdir ∷ ∀ ε δ μ .
        (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack, DirAs δ) =>
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
                    DirType δ ~ DirType (DirType δ), δ ~ DirType δ) =>
         δ        -- ^ directory to create
       → FileMode -- ^ permissions to apply to any *newly created* directories
       → μ ()
mkpath d p = do
  to_make ← filterM (fmap (≡ NoFExists) ∘ fexists) (parents' d)
  case headMay to_make of
    𝓝    → return () -- nothing to do, all exist
    𝓙 t  → -- make the intervening dirs, carefully; in case of any error,
           -- try to nuke those we freshly made
           onException (forM_ to_make (\ a → mkdir a p)) (nuke t)

----------------------------------------

{-| List a directory's files & subdirs, along with their stat results.
    The results are split into files & dirs, so that they get appropriate types
    (`AbsDir`/`AbsFile`, or `RelDir`/`RelFile`).

    Note that `ρ` should be a file type, e.g., `AbsFile`, `File` or `RelFile`;
    and that `d` should be its directory type (`AbsDir`, `Dir`, `RelDir`
    respectively).

    An IOError will be returned (in the `MonadError` of `μ`) if the directory is
    not readable.

    N.B.: the type of ρ may need to be specified if it's not implicit in context;
          the type of the first argument (e.g., an AbsDir) will not suffice, as
          the fact that (e.g.,) `AppendableFPath ρ ~ AbsDir` is insufficient to
          specify ρ precisely.

    Example usage:
    > lsdir @FPathIOError @IOError @AbsFile [absdir|/tmp/]

-}
lsdir ∷ ∀ ε ε' ρ μ .
        (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ, HasCallStack,
         AsFilePath (AppendableFPathD ρ), ToDir ρ, AsIOError ε',
         AppendableFPath ρ,
         -- AppendableFPathF ρ ~ RelFile means that we have to be able to append
         -- `RelFile` to ρ to make a new path - hence ρ must be a file type
         AppendableFPathF ρ ~ RelFile) =>
        AppendableFPathD ρ → μ ([(ρ, FStat)], [(DirType ρ, FStat)], [(ρ, ε')])
lsdir d = do
  fns ← asIOErrorT $ liftIO (listDirectory (d ⫥ filepath))
  xs ← sequence $ (fmap (d ⫻) ∘ parse @RelFile) ⊳ fns
  (foldr pathTypes ([],[],[]) ⩺ lstats) xs

------------------------------------------------------------

data Recursive         = NotRecursive | Recursive  deriving  (Eq, Show)

------------------------------------------------------------

data DirectoryListOpts = DirectoryListOpts { recursive ∷ Recursive }
  deriving (Eq, Show)

----------

instance Default DirectoryListOpts where
  def = DirectoryListOpts { recursive = NotRecursive }

------------------------------------------------------------

{-| List a directory's files & subdirs, along with their stat results.
    The results are split into files & dirs.

    The results always include the input directory `d`.  Directories that cannot
    be listed because, e.g., the permissions do not allow it; will be added to
    the directory errors list.
-}
directoryList ∷ ∀ ε ε' μ .
                (MonadIO μ,
                 AsFPathError ε, AsIOError ε, AsFPathError ε', AsIOError ε',
                 HasCallStack) =>
                DirectoryListOpts → AbsDir
              → μ([(AbsFile,FStat)],[(AbsDir,FStat)],[(AbsFile,ε)],[(AbsDir,ε')])
directoryList opts d = do
  let (⊛) (f1,d1,e1,g1) (f2,d2,e2,g2) = (f1◇f2,d1◇d2,e1◇e2,g1◇g2)
      go đ =
        do -- if an lsdir fails, add it to the failures, don't bail
           ѥ (lsdir @_ @_ @_ đ) ≫ \ case
             𝓛 e                 → return ([],[],[],[(đ,e)])
             𝓡 (files,dirs,errs) →
               if Recursive ≡ (recursive opts)
               then do results ← mapM go (fst ⊳ dirs)

                       return $ (foldl (⊛) (files,dirs,errs,[]) results)
               else return (files,dirs,errs,[])
  ѥ (lstat' d) ≫ \ case
    𝓛 e     → return ([],[],[],[(d,e)])
    𝓡 dstat → (([],[(d,dstat)],[],[]) ⊛) ⊳ go d

----------------------------------------

{-| list all files matching a PCRE pattern in a directory -}
glob ∷ ∀ ε ε' ρ μ .
          (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ, HasCallStack,
           AsFilePath (AppendableFPathD ρ), ToDir ρ, AsIOError ε',
           AppendableFPath ρ, Basename ρ, Basename (DirType ρ),
           Printable(RelType ρ), Printable(RelType(DirType ρ)),
           -- AppendableFPathF ρ ~ RelFile means that we have to be able to
           -- append `RelFile` to ρ to make a new path - hence ρ must be a file
           -- type
           AppendableFPathF ρ ~ RelFile) =>
          𝕊 → AppendableFPathD ρ → μ ([(ρ,FStat)], [(DirType ρ,FStat)], [(ρ,ε')])

glob patt d = do
  let pattFilt ∷ ∀ χ ξ . (Basename χ, Printable (RelType χ)) =>
                 [(χ,ξ)] → [(χ,ξ)]
      pattFilt = filter (\ (s,_) → toString (basename s) =~ patt)
  (fs,ds,es) ← lsdir d
  return (pattFilt fs, pattFilt ds, es)

----------------------------------------

{-| Provide a directory listing as `[Text]`.  Format subject to change; this is
    currently used only for diagnostics (but that may change - note here if so).
-}
listdir ∷ ∀ ε μ .(MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ) =>
          DirectoryListOpts → AbsDir → μ [𝕋]
listdir opts d = do
  (files, dirs, ferrs, derrs) ← directoryList opts d
  let to_txt ∷ Printable τ => (τ,FStat) → 𝕋
      to_txt (x,s) = [fmt|%t %t %7y %T|] (ftypeLSTxt s) (permLSTxt s) (size s) x
      files_dirs = sortBy go $ ю [ first AbsF ⊳ files, first AbsD ⊳ dirs ]
                   where go (a,_) (b,_) = compare (toText a) (toText b)
      files_dirs_txts = to_txt ⊳ files_dirs
      e_to_txt ∷ Printable τ => (τ,FPathIOError) → 𝕋
      e_to_txt (x,s) = [fmt|¡ERROR! %T ▶▶ %T|] x s
      e_txts = sortBy go $ ю [ first AbsF ⊳ ferrs, first AbsD ⊳ derrs ]
               where go (a,_) (b,_) = compare (toText a) (toText b)
  return $ files_dirs_txts ◇ (e_to_txt ⊳ e_txts)

----------

{-| dump a directory listing to stdout.  See `listdir` -}
listdirStdOut ∷ MonadIO μ => DirectoryListOpts → AbsDir → μ ()
listdirStdOut opts d = ӝ (listdir @FPathIOError opts d)≫ liftIO ∘ mapM_ putStrLn

----------

{-| dump a directory listing to stderr.  See `listdir` -}
listdirStdErr ∷ MonadIO μ => DirectoryListOpts → AbsDir → μ ()
listdirStdErr opts d =
  ӝ (listdir @FPathIOError opts d)≫ liftIO ∘ mapM_ (hPutStrLn stderr)

-- that's all, folks! ----------------------------------------------------------
