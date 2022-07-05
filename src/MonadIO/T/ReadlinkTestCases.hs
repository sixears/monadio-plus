{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MonadIO.T.ReadlinkTestCases
  ( ReadlinkTestCase
  , slName, readExp, resolveExp, readlinkTestCases, slTarget )
where

import Base1T

-- base --------------------------------

import System.IO  ( FilePath )

-- fpath -------------------------------

import FPath.Abs      ( Abs( AbsD, AbsF ) )
import FPath.AbsDir   ( AbsDir, absdir, root )
import FPath.AbsFile  ( absfile )
import FPath.AppendableFPath   ( (⫻) )
import FPath.Dirname  ( dirname )
import FPath.RelDir   ( RelDir, reldir )
import FPath.RelFile  ( RelFile, relfile )

--------------------------------------------------------------------------------

data ReadlinkTestCase = ReadlinkTestCase { -- name of the symlink relative to
                                           -- some dir; the test prep should
                                           -- create this symlink
                                           slName     ∷ RelFile
                                         , -- target of the symlink; the test
                                           -- prep should create the symlink
                                           -- `name` pointing to this
                                           slTarget   ∷ FilePath
                                         , -- `readlink` should return this
                                           -- value
                                           readExp    ∷ AbsDir → Abs
                                         , -- `resolvelink` should return this
                                           -- value
                                           resolveExp ∷ AbsDir → 𝕄 Abs
                                         }

{- | Test cases for `readlink` and `resolvelink`; each being a tuple of local
     name, link target (as a `FilePath`), link target (as an Abs-type) which
     should be the result of calling `readlink` on the local name, and full link
     target (as an Abs-type) which should be the result of calling
     `resolvelink` on the local name .
 -}
readlinkTestCases ∷ [ReadlinkTestCase]
readlinkTestCases =
  let absD = const ∘ AbsD
      absD' = const ∘ 𝕵 ∘ AbsD
      absF = const ∘ AbsF
      absF' = const ∘ 𝕵 ∘ AbsF
      relD ∷ RelDir → AbsDir → Abs
      relD f = \ t → AbsD $ t ⫻ f
      relD' ∷ RelDir → AbsDir → 𝕄 Abs
      relD' f = \ t → 𝕵 ∘ AbsD $ t ⫻ f
      relDp ∷ RelDir → AbsDir → Abs
      relDp d = \ t → AbsD $ (t ⊣ dirname) ⫻ d
      relF ∷ RelFile → AbsDir → Abs
      relF f = \ t → AbsF $ t ⫻ f
      relFp ∷ RelFile → AbsDir → Abs
      relFp f = \ t → AbsF $ (t ⊣ dirname) ⫻ f
   in (((\ (a,b,c) → ReadlinkTestCase a b c (𝕵 ∘ c)) ⊳
        [ ([relfile|dangle-relfile|] , "nonesuch"     , relF [relfile|nonesuch|])
        , ([relfile|dangle-reldir|]  , "nonesuch/"    , relD [reldir|nonesuch/|])
        , ([relfile|dangle-absfile|] , "/nonesuch"    , absF [absfile|/nonesuch|])
        , ([relfile|dangle-absdir|]  , "/nonesuch/"   , absD [absdir|/nonesuch/|])
        , ([relfile|parent-dangle|]  , "../nonesuch"  , relFp [relfile|nonesuch|])
        , ([relfile|dr-prnt-dr-p|]   , "dir/../dir/p" , relF  [relfile|directory/p|])
        , ([relfile|dy-prnt-dr-p|]   , "directory/../dir/p" ,
           relF  [relfile|directory/p|])
        , ([relfile|dr-prnt-dy-p|]   , "dir/../directory/p" ,
           relF  [relfile|directory/p|])
        , ([relfile|dy-prnt-dy-p|]   , "directory/../directory/p" ,
           relF  [relfile|directory/p|])
       ])

        -- for each of the below, the
        -- fully-resolved target ≡ the single-resolved target

     ⊕ ((\ (a,b,c) → ReadlinkTestCase a b c (𝕵 ∘ c)) ⊳
       [ ([relfile|slash|]          , "/"            , absD root)
       , ([relfile|slashes|]        , "///"          , absD root)
       , ([relfile|etc|]            , "/etc/"        , absD [absdir|/etc/|])
       , ([relfile|passwd|]         , "/etc/passwd"  , absF [absfile|/etc/passwd|])
       -- this results in a dir, because '.' is always a dir
       , ([relfile|this|]           , "."            , relD [reldir|./|])
       , ([relfile|this-dir|]       , "./"           , relD [reldir|./|])
       , ([relfile|this-dirs|]      , ".///"         , relD [reldir|./|])
       , ([relfile|this-this-this|] , "././."        , relD [reldir|./|])
       -- this results in a dir, because '..' is always a dir
       , ([relfile|parent|]         , ".."           , relDp [reldir|./|])
       , ([relfile|parent-dir|]     , "../"          , relDp [reldir|./|])
       , ([relfile|this-parent|]    , "./.."         , relDp [reldir|./|])
       , ([relfile|parent-this|]    , "../."         , relDp [reldir|./|])
       , ([relfile|plainfile|]      , "plain"        , relF  [relfile|plain|])
       , ([relfile|dir-dir|]        , "directory/"   , relD  [reldir|directory/|])

       , ([relfile|dr-prnt-dr|] , "dir/../dir/"             , relD [reldir|directory/|])
       , ([relfile|dr-prnt-dy|] , "dir/../directory/"       , relD [reldir|directory/|])
       , ([relfile|dy-prnt-dr|] , "directory/../dir/"       , relD [reldir|directory/|])
       , ([relfile|dy-prnt-dy|] , "directory/../directory/" , relD [reldir|directory/|])

       ])

     ⊕ [
        ReadlinkTestCase [relfile|dangle-relfile2|] "dangle-relfile"
                         (relF [relfile|dangle-relfile|])
                         (𝕵 ∘ relF ([relfile|nonesuch|]))
        -- The target of a symlink can have a '/' at the end; but a symlink
        -- name cannot have a '/' at the end, because a symlink is a file not a
        -- directory.  So the filepath target cannot usefully have a trailing
        -- '/' here, as that would not be a resolvable thing by readlink /
        -- resolvelink.
      , ReadlinkTestCase [relfile|dangle-reldir2|]  "dangle-reldir/"
                         (relD [reldir|dangle-reldir/|])
                         (𝕵 ∘ relD [reldir|dangle-reldir/nonesuch/|])
      , ReadlinkTestCase [relfile|dangle-absfile2|] "dangle-absfile"
                         (relF [relfile|dangle-absfile|])
                         (const ∘ 𝕵 $ AbsF [absfile|/nonesuch|])
      , ReadlinkTestCase [relfile|dangle-absdir2|] "dangle-absdir"
                         (relF [relfile|dangle-absdir|])
                         (const ∘ 𝕵 $ AbsD [absdir|/nonesuch/|])
      -- resolvelink should identify these as directories
      , ReadlinkTestCase [relfile|etcf|] "/etc" (absF [absfile|/etc|])
                         (absD' [absdir|/etc/|])
      , ReadlinkTestCase [relfile|dir|] "directory" (relF [relfile|directory|])
                         (relD' [reldir|directory/|])
      , ReadlinkTestCase [relfile|passwd-dir|] "/etc/passwd/"
                         (absD [absdir|/etc/passwd/|])
                         (absF' [absfile|/etc/passwd|])
      ])

-- that's all, folks! ----------------------------------------------------------
