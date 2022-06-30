{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE ViewPatterns        #-}

module MonadIO.FPath
  ( PResolvable( pResolveDir, pResolve )

  -- re-exported for backwards compatibility
  , getCwd

  , tests
  )
where

import Base1T
import Debug.Trace  ( trace, traceShow )

-- base --------------------------------

import qualified  Data.List

import Control.Monad  ( filterM )
import Data.List      ( dropWhile, dropWhileEnd, filter, intercalate, isPrefixOf
                      , isSuffixOf, reverse, scanl, scanr,zip )

-- containers --------------------------

import Data.Sequence  ( Seq( Empty ), breakr {- , fromList -} )

-- directory ---------------------------

import System.Directory  ( withCurrentDirectory )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadMask, bracket )

-- filepath ----------------------------

import System.FilePath  ( FilePath, (</>), splitPath )

-- fpath -------------------------------

import FPath.Abs               ( Abs( AbsD, AbsF ), absT )
import FPath.AbsDir            ( AbsDir, absdir, __parseAbsDirP__, root )
import FPath.AbsFile           ( AbsFile, absfileT )
import FPath.AppendableFPath   ( (⫻) )
import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.AsFilePath'       ( exterminate )
import FPath.Basename          ( basename )
import FPath.Dirname           ( dirname )
import FPath.Error.FPathError  ( AsFPathError, FPathIOError, _FPathEmptyE
                               , __FPathEmptyE__, __FPathNotAFileE__ )
import FPath.Parseable         ( parse )
import FPath.RelDir            ( RelDir, reldir )
import FPath.RelFile           ( RelFile, relfile )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( (~~) )

-- safe --------------------------------

import Safe  ( headNote )

-- temporary ---------------------------

import System.IO.Temp ( getCanonicalTemporaryDirectory,withSystemTempDirectory )

-- unix --------------------------------

import System.Posix.Directory  ( changeWorkingDirectory, getWorkingDirectory )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.Cwd    ( getCwd )
import MonadIO.FStat  ( FExists( FExists ), fexists )

--------------------------------------------------------------------------------

{- | Perform IO within a directory, with declared errors. -}
_inDir ∷ ∀ ε α τ μ .
         (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack, Printable τ) ⇒
         τ → IO α → μ α
_inDir (toString → d) io = traceShow ("_inDir",d) $
  -- ensure that the path is attached to the error
  (ѥ ∘ asIOError $ withCurrentDirectory d io) ≫ \ case
    Left e' → join $ throwError (e' ~~ d)-- (ioEWithPath d e')
    Right r → return r

{- | like `inDir`, but takes IO that already throws some error(s). -}
_inDirT ∷ forall ε α τ μ .
          (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack, Printable τ) ⇒
          τ → ExceptT ε IO α → μ α
_inDirT d io = join $ _inDir d (ѥ io)

scan ∷ (α → α → α) → α → [α] → [(α,α)]
scan f b xs = zip (scanl f b xs) (scanr f b xs)

{- | List of splits of a file path in two, at each directory along the path;
     e.g.,

     @
       λ> splitPoints "foo/bar/"
       [("","foo/bar"),("foo/","bar"),("foo/bar","")]
     @

     Note that:
       -) any trailing '/' is dropped.
       -) all sequences of slashes are compressed to a single slash.  This is
          done to avoid questions of whether "//" should be split to ("/","/")
          (clearly, it should not), and the potential return of ("///", "foo")
          leading to a 'root' directory of "///", which fexists considers not
          to exist.
 -}
splitPoints ∷ FilePath → [(FilePath,FilePath)]
splitPoints f =
  let -- compress "//…" sequences to a single /.
      -- this is always fair game for slashes, and in particular means that the
      -- split points will work; e.g., you don't end with a split point of
      -- "///", which doesn't exist
      compressSlashes ∷ FilePath → FilePath
      compressSlashes ('/' : p) = '/' : compressSlashes (dropWhile (≡ '/') p)
      compressSlashes (c   : p) = c : compressSlashes p
      compressSlashes []        = []
   in scan (</>) "" (splitPath $ exterminate (compressSlashes f))

splitPointsTests ∷ TestTree
splitPointsTests =
  testGroup "splitPoints" $
    let check sp exp = testCase sp $ exp @=? splitPoints sp
     in [ check ""          [("","/"),("/","")]
        , check "/"         [("","/"),("/","")]
        , check "/foo"      [("","/foo"),("/","foo"),("/foo","")]
        , check "./"        [("","./"),("./","")]
        , check "./foo"     [("","./foo"),("./","foo"),("./foo","")]
        , check "bar/./foo" [("","bar/./foo"),("bar/","./foo"),("bar/./","foo")
                            ,("bar/./foo","")]
        , check "///"       [("","/"),("/","")]
        , check ".///"      [("","./"),("./","")]
        , check "//./"      [("","/./"),("/","./"),("/./","")]
        , check "///."      [("","/."),("/","."),("/.","")]
        , check "/foo/bar/" [("","/foo/bar/"),("/","foo/bar/")
                            ,("/foo/","bar/"),("/foo/bar/","")]
        ]

{- | Given an absdir and a subsequent filepath (which might be absolute),
     return a pair of the initial filepath that exists, which has been fully
     *resolved* (symlinks, ., .., all resolved); and a part that does not exist.
     Note that the extant part must have read-and-execute permission for the user
     for every dir along the way, and read for the final file (if any); else an
     `AsIOError` will be raised.
 -}
resolve ∷ forall ε μ . (AsIOError ε, MonadError ε μ, HasCallStack, MonadIO μ) ⇒
          AbsDir → FilePath → μ (FilePath, FilePath)
resolve d fp =
  let -- prepend `d`, note this is a no-op for input abs functions
      prepend ∷ FilePath → FilePath
      prepend = (filepath # d </>)
      fex = fmap (FExists ≡) ∘ fexists ∘ fst
      -- Given an AbsDir, `resolve` must resolve to *something* valid, since the
      -- top of an AbsDir is the root dir, and that always exists.
   in headNote ([fmt|MonadIO.FPath.resolve should never fail (%T '%T')|] d fp) ⊳
        (filterM fex ∘ reverse ∘ splitPoints $ prepend fp)

----------------------------------------

{- | Compress multiple @/@ to a single @/@; remove @.@; remove @x/..@ sequences.
     Note that logically, @""@ canonicalizes to @"."@, for consistency with (say)
     @"foo/.."@.

     This is not the same as `resolve`, which checks the filesystem.  It is
     somewhat similar to @realpath --logical@.  For example, if @/tmp/foo/pam@
     points to @/etc/pam@ (which is a real directory); then @realpath@ of
     @/tmp/foo/pam/..@ will be @/etc@; but @realpath --logical@ of
     @/tmp/foo/pam/..@ will be @/tmp/foo@.  `canonicalize` should give the same
     result.
 -}
canonicalize ∷ FilePath → FilePath
canonicalize p =
  let is_abs = "/" `isPrefixOf` p
      is_dir = "/" `isSuffixOf` p
      go' ∷ [FilePath] → [FilePath]
      go' xs = let r = go ([],xs) in traceShow ("go'",xs,r) (reverse (fst r) ⊕ snd r)
      go ∷ ([FilePath],[FilePath]) → ([FilePath],[FilePath])
      go zs@(ys,".":xs) = traceShow("go",zs) $ go (ys,xs)
-- this can probably be more efficient by only considering the last element of
-- ys
      go zs@([],"..":xs) | is_abs = traceShow("go",zs) $ go ([],xs)
      go zs@(y:ys,[".."]) | y ≢ ".." = traceShow("go",zs) $ go (ys,[])
      go zs@(ys,x:"..":xs) = traceShow("go",zs) $ go ([],reverse ys⊕xs)
      go zs@(ys,x:xs) = traceShow("go",zs) $ go(x:ys,xs)
      go zs@(ys,[]) = traceShow("go",zs) $ (ys,[])
--      go [] = []
      rex ∷ FilePath → FilePath
      rex "" = traceShow ("rex empty",is_abs,is_dir) $
               if is_abs
               then "/"
               else if is_dir then "./" else "."
      rex x = traceShow ("rex",x,is_abs,is_dir,p) $ if is_abs
              then if is_dir then "/" ⊕ x ⊕ "/" else "/" ⊕ x
              else  if is_dir then x ⊕ "/" else x
   in rex ∘ intercalate "/" $ go' $ fmap (dropWhileEnd (≡'/')) $ splitPath (dropWhile (≡'/') (dropWhileEnd (≡'/') p))

-- XXX test with /, ./, .
_canonicalize "" = ""
_canonicalize p =
  let abs = "/" `isPrefixOf` p
   in rex $ intercalate "/" ∘ go' ∘ go $ dropWhileEnd (≡'/') ⊳ splitPath p
   where rex "" = if "/" `isPrefixOf` p
                  then "/"
                  else if "/" `isSuffixOf` p then "./" else "."
         rex "/" = "/"
         rex xs = xs ⊕ if 𝕵 '/' ≡ last p then "/" else ""
         go []        = []
         go ["."] = ["."]
--         go ("..":xs) = ".." : go xs
         go (".":xs) = go xs
--         go (".":"..":xs) = "." : go ("..":xs)
         go (x:"..":xs) = go xs
         go (x:"..":xs) = go xs
         go (x:xs) = x : go xs
         go' ["."] = ["."]
         go' xs = filter (≢ ".") xs

canonicalizeTests ∷ TestTree
canonicalizeTests =
  testGroup "canonicalize" $
    let check p exp = testCase p $ exp @=? canonicalize p
     in [ check ""      "." -- "" is what you'd get for, say, foo/..; hence .
        , check "/"     "/"
        , check "//"    "/"
        , check "///"   "/"
        , check "///."  "/"
        , check "//./"  "/"
        , check "/.//"  "/"
        , check "./"    "./"
        , check ".//"   "./"
        , check ".///"  "./"

        , check "/etc" "/etc"
        , check "/etc/" "/etc/"
        , check "/etc/.." "/"
        , check "/etc/../" "/"
        , check "/etc/../var" "/var"
        , check "/etc/../var/" "/var/"
        , check "/etc/pam/../../var" "/var"
        , check "/etc/pam/../../var/" "/var/"
        , check "/etc/../var/log/.." "/var"
        , check "/../etc/../var/log/.." "/var"
        , check "../etc/../var/log" "../var/log"
        , check "../etc/../var/log/" "../var/log/"
        , check "../etc/../var/log/.." "../var"
        , check "../etc/../var/log/../" "../var/"

        , check "etc" "etc"
        , check "etc/" "etc/"
        , check "etc/.." "."
        , check "etc/../" "./"
        , check "etc/../var" "var"
        , check "etc/../var/" "var/"
        , check "etc/pam/../../var" "var"
        , check "etc/pam/../../var/" "var/"
        , check "etc/../var/log/.." "var"

        , check "."     "."
        , check ".."    ".."
        , check "../a"  "../a"
        , check "../a/" "../a/"
        , check "a/.."  "."
        , check "a/../" "./"
        , check "/a//b///c/.//.."  "/a/b"
        , check "/a//b///c/.//../" "/a/b/"
        ]

------------------------------------------------------------

{- | Things which are physically resolvable -}
class PResolvable α where
  {- | Given a path, which might well be relative and include '..' and/or '.',
       physically resolve that to an α by starting at a given `AbsDir`.
       This involves trying to @chdir@ to the target directory; so will only
       work for directories that you have permission to @chdir@ into (for files,
       you need to be able to @chdir@ into the parent directory).
   -}
  pResolveDir ∷ ∀ ε τ μ .
                (Printable τ, AsIOError ε, AsFPathError ε, MonadError ε μ,
                 HasCallStack, MonadIO μ) ⇒
                AbsDir → τ → μ α

  {- | `pResolveDir`, taking the current working directory as the starting point
   -}
  pResolve ∷ ∀ ε τ μ .
             (Printable τ, AsIOError ε, AsFPathError ε, MonadError ε μ,
              HasCallStack, MonadIO μ) ⇒
             τ → μ α
  pResolve f = getCwd ≫ \ d → pResolveDir d f

{- | Physically resolve the whole path, thus every directory (including the last
     must) exist.  Treats the lack of a trailing '/' on the input stringlike
     kindly; that is, even without a trailing '/', it is considered as a
     directory.
 -}
instance PResolvable AbsDir where
  pResolveDir ∷ ∀ ε τ μ .
                (Printable τ, AsIOError ε, AsFPathError ε, MonadError ε μ,
                 HasCallStack, MonadIO μ) ⇒
                AbsDir → τ → μ AbsDir
  pResolveDir d (toString → p) = traceShow ("pResolveDir AbsDir", d, p ) $ do
    (extant,non_extant) ← resolve d p
    d' ← traceShow("pResolveDir AD",extant,non_extant) $ _inDirT extant getCwd
    let -- add a trailing / so reldir parses it
        toDir ∷ FilePath → FilePath
        toDir "" = "./"
        toDir t  = case Data.List.last t of -- last is safe, t is non-empty
                     '/' → t
                     _   → t ⊕ "/"
    p' ← traceShow("pResolveDir AD+",d',non_extant) $ parse @RelDir (toDir non_extant)
    return $ d' ⫻ p'

----------

pResolveAbsDirTests ∷ TestTree
pResolveAbsDirTests =
  let tName   = "pResolveTests.AbsDir"
      inTmp   = inSystemTempDirectory tName

      pResolve_ ∷ 𝕋 → IO (Either FPathIOError AbsDir)
      pResolve_ = ѥ ∘ pResolve

      getTmpdir ∷ IO AbsDir
      getTmpdir = __parseAbsDirP__ ⊳ getCanonicalTemporaryDirectory

   in testGroup "AbsDir"
        [ testCase "inTmp ./" $ inTmp $ \ d → pResolve_ "./" ≫ (Right d @=?)
        , testCase "inTmp . (forgiveness of pResolve wrt trailing /)" $
            inTmp $ \ d → pResolve_ "."  ≫ (Right d @=?)
        , testCase "inTmp .." $
            inTmp ∘ const $
                    getTmpdir ≫ \ tmpdir → pResolve_ ".."  ≫ (Right tmpdir @=?)

        , testCase "inTmp cwd" $
            -- value is an abs dir, e.g., /tmp/<user>/d7b66efeebbcf249/
            inTmp $ \ d → pResolve_ (toText d) ≫ (Right d @=?)
        , testCase "root" $
            inTmp $ \ _ → pResolve_ "/" ≫ (Right [absdir|/|] @=?)
        , testCase "/nonsuch/" $
            inTmp $ \ _ → pResolve_ "/nonsuch/" ≫
                            (Right [absdir|/nonsuch/|] @=?)
        , testCase "inTmp nonsuch" $
            inTmp $ \ d → pResolve_ "nonsuch" ≫
                          (Right (d ⫻ [reldir|nonsuch/|]) @=?)
        , testCase "inTmp nonesuch" $
            inTmp $ \ _ → pResolve_ "/nonsuch" ≫
                          (Right [absdir|/nonsuch/|] @=?)
        , testCase "inTmp nonsuch/" $
            inTmp $ \ d → pResolve_ "nonsuch/" ≫
                          (Right (d ⫻ [reldir|nonsuch/|]) @=?)

        , testCase "inTmp ./" $ inTmp $ \ d → do
            d' <- pResolve_ "nonsuch/nonsuch"
            Right (d ⫻ [reldir|nonsuch/nonsuch/|]) @=? d'

        , testCase "inTmp ../ (dirname)" $
            inTmp $ \ d → pResolve_ "../" ≫ ((Right (d ⊣ dirname) @=?))
        , testCase "inTmp ../ (basename)" $
            inTmp $ \ d → pResolve_ "../" ≫
                            ((Right d @=?) ∘ fmap (⫻ basename d))
        ]

----------

pResolveDirAbsDirTests ∷ TestTree
pResolveDirAbsDirTests =
  let tName   = "pResolveDirTests.AbsDir"
      withTmp ∷ (MonadIO μ, MonadMask μ) ⇒ (AbsDir → μ α) → μ α
      withTmp = withSystemTempDirectory tName ∘ (∘ __parseAbsDirP__)

      pResolveDir_ ∷ AbsDir → 𝕋 → IO (Either FPathIOError AbsDir)
      pResolveDir_ d = ѥ ∘ pResolveDir d

      getTmpdir ∷ IO AbsDir
      getTmpdir = __parseAbsDirP__ ⊳ getCanonicalTemporaryDirectory

   in testGroup "AbsDir"
        [ testCase "withTmp ./" $
            withTmp $ \ d → pResolveDir_ d "./" ≫ (Right d @=?)
        , testCase "withTmp .//" $
            withTmp $ \ d → pResolveDir_ d ".//" ≫ (Right d @=?)
        , testCase "withTmp /" $
            withTmp $ \ d → pResolveDir_ d "/" ≫ (Right root @=?)
        , testCase "withTmp //" $
            withTmp $ \ d → pResolveDir_ d "//" ≫ (Right root @=?)
        , testCase "withTmp ." $
            withTmp $ \ d → pResolveDir_ d "." ≫ (Right d @=?)
        , testCase "withTmp .." $
            withTmp $ \ d → getTmpdir ≫ \ tmpdir →
                      pResolveDir_ d ".." ≫ (Right tmpdir @=?)
        ]

----------------------------------------

{- | Physically resolve every directory up to and including the dirname of the
     input stringlike; and then tacks the file basename onto the end.  Treats a
     trailing '/' as a dir, and thus fails.
 -}
instance PResolvable AbsFile where
  pResolveDir ∷ ∀ ε τ μ .
                (Printable τ, AsIOError ε, AsFPathError ε, MonadError ε μ,
                 HasCallStack, MonadIO μ) ⇒
                AbsDir → τ → μ AbsFile
  pResolveDir d (toString → f) = traceShow ("pResolveDir AbsFile", d, f ) $
    -- we can't simply use parseRelFile, etc., here, as we want to accept
    -- paths with '..' and '.' in them (and resolve them)
    case breakr (≡ '/') $ fromList f of
      -- first element of tuple is suffix of seq (a little counterintuitively)
      (Empty, Empty) → -- f was empty
                       __FPathEmptyE__    absfileT
      (Empty, _    ) → -- f had a trailing /
                       __FPathNotAFileE__ absfileT (toText f)

      (_, Empty    ) → -- just a file, no dir part
                       do c ← pResolveDir @AbsDir d (""∷𝕋)
                          (c ⫻) ⊳ parse @RelFile f

      (x    , y    ) → -- dir + file
                       do c ← pResolveDir @AbsDir d (toList y)
                          (c ⫻) ⊳ parse @RelFile (toList x)

pResolveAbsFileTests ∷ TestTree
pResolveAbsFileTests =
  let tName   = "MonadIO.FPath.pResolveTests.AbsFile"
      inTmp   = inSystemTempDirectory tName
      withTmp ∷ (MonadIO μ, MonadMask μ) ⇒ (AbsDir → μ α) → μ α
      withTmp = withSystemTempDirectory tName ∘ (∘ __parseAbsDirP__)

      pResolve_ ∷ 𝕋 → IO (Either FPathIOError AbsFile)
      pResolve_ = ѥ ∘ pResolve

      pResolveDir_ ∷ AbsDir → 𝕋 → IO (Either FPathIOError AbsFile)
      pResolveDir_ d = ѥ ∘ pResolveDir d

   in testGroup "AbsFile"
        [ testCase "inTmp '' x" $
            inTmp $ \ d → pResolve_ "x" ≫
                            (Right (d ⫻ [relfile|x|] ∷ AbsFile) @=?)
        , testCase "withTmp '' x" $
            withTmp $ \ d → pResolveDir_ d "x" ≫
                          (Right (d ⫻ [relfile|x|] ∷ AbsFile) @=?)
        , testCase "inTmp ./ x" $
            inTmp $ \ d → pResolve_ "./x" ≫
                          (Right (d ⫻ [relfile|x|] ∷ AbsFile) @=?)
        , testCase "withTmp ./ x" $
            withTmp $ \ d → pResolveDir_ d "./x" ≫
                          (Right (d ⫻ [relfile|x|] ∷ AbsFile) @=?)
        , testCase "inTmp ../ x" $
            inTmp $ \ d → pResolve_ "../x" ≫
                          (Right (d ⊣ dirname ⫻ [relfile|x|] ∷ AbsFile) @=?)
        , testCase "withTmp ../ x" $
            withTmp $ \ d → pResolveDir_ d "../x" ≫
                          (Right (d ⊣ dirname ⫻ [relfile|x|] ∷ AbsFile) @=?)
        , testCase "non-extant dir" $
            withTmp $ \ d → pResolveDir_ (d ⫻ [reldir|nonesuch/|]) "x" ≫
                          (Right (d ⫻ [relfile|nonesuch/x|] ∷ AbsFile) @=?)
        ]


{- | Given a path, which might well relative include '..' and/or '.', physically
     resolve that to an Abs.  Relative paths are contextual to the cwd.
     Input with a trailing '/', "/.", or "/.."; or the special cases "." and
     ".." are resolved to directories; without are resolved to files.  Empty
     input strings cause a failure.
 -}
instance PResolvable Abs where
  pResolveDir ∷ forall ε τ μ .
                (Printable τ, AsIOError ε, AsFPathError ε, MonadError ε μ,
                 HasCallStack, MonadIO μ)⇒
                AbsDir → τ → μ Abs
  pResolveDir _ (toString → [])                       = __FPathEmptyE__ absT
  pResolveDir d t@(toString → ".")                    = AbsD ⊳ pResolveDir d t
  pResolveDir d t@(toString → "..")                   = AbsD ⊳ pResolveDir d t
  pResolveDir d t@(reverse ∘ toString → '/' : _)      = AbsD ⊳ pResolveDir d t
  pResolveDir d t@(reverse ∘ toString → '.' : '/' : _)= AbsD ⊳ pResolveDir d t
  pResolveDir d t@(reverse ∘ toString → '.':'.':'/':_)= AbsD ⊳ pResolveDir d t
  pResolveDir d t                                     = AbsF ⊳ pResolveDir d t

pResolveAbsTests ∷ TestTree
pResolveAbsTests =
  let tName   = "MonadIO.FPath.pResolveTests.Abs"
      withTmp ∷ (MonadIO μ, MonadMask μ) ⇒ (AbsDir → μ α) → μ α
      withTmp = withSystemTempDirectory tName ∘ (∘ __parseAbsDirP__)

      pResolveDir_ ∷ AbsDir → 𝕋 → IO (Either FPathIOError Abs)
      pResolveDir_ d = ѥ ∘ pResolveDir d

   in testGroup "Abs"
        [ testCase "withTmp ''" $
            withTmp $ \ d → pResolveDir_ d "" ≫ (Left (_FPathEmptyE absT) @=?)
        , testCase "withTmp ./" $
            withTmp $ \ d → pResolveDir_ d "./" ≫ (Right (AbsD d) @=?)
        , testCase "withTmp ." $
            withTmp $ \ d → pResolveDir_ d "." ≫ (Right (AbsD d) @=?)
        , testCase "withTmp .." $
            withTmp $ \ d → pResolveDir_ d ".." ≫
                              (Right (AbsD (d ⊣ dirname)) @=?)
        , testCase "withTmp ../" $
            withTmp $ \ d → pResolveDir_ d "../" ≫
                              (Right (AbsD (d ⊣ dirname)) @=?)
        , testCase "withTmp ../." $
            withTmp $ \ d → pResolveDir_ d "../." ≫
                              (Right (AbsD (d ⊣ dirname)) @=?)
        , testCase "withTmp ./../." $
            withTmp $ \ d →
                      pResolveDir_ d "./../."≫(Right (AbsD (d ⊣ dirname)) @=?)
        , testCase "withTmp .././." $
            withTmp $ \ d →
                      pResolveDir_ d ".././."≫(Right (AbsD (d ⊣ dirname)) @=?)

        , testCase "withTmp ''" $
            withTmp $ \ d → pResolveDir_ d "" ≫ (Left (_FPathEmptyE absT) @=?)
        , testCase "withTmp '' x" $
            withTmp $ \ d → pResolveDir_ d "x" ≫
                          (Right (AbsF (d ⫻ [relfile|x|])) @=?)
        , testCase "withTmp ./ x" $
            withTmp $ \ d → pResolveDir_ d "./x" ≫
                          (Right (AbsF (d ⫻ [relfile|x|])) @=?)
        , testCase "withTmp ../ x" $
            withTmp $ \ d → pResolveDir_ d "../x" ≫
                          (Right (AbsF (d ⊣ dirname ⫻ [relfile|x|])) @=?)
        ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

inSystemTempDirectory ∷ String → (AbsDir → IO α) → IO α
inSystemTempDirectory t io =
  withSystemTempDirectory t $ \ d →
    bracket (getWorkingDirectory ≫  \ o → changeWorkingDirectory d ⪼ return o)
            changeWorkingDirectory
            (\ _ → io $ __parseAbsDirP__ d)

pResolveDirTests ∷ TestTree
pResolveDirTests = testGroup "pResolveDir" [ pResolveDirAbsDirTests ]

pResolveTests ∷ TestTree
pResolveTests = testGroup "pResolve" [ pResolveAbsDirTests
                                     , pResolveAbsFileTests, pResolveAbsTests ]

tests ∷ TestTree
tests = testGroup "MonadIO.FPath" [ splitPointsTests, canonicalizeTests
                                  , pResolveTests, pResolveDirTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
