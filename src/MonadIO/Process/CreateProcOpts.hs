module MonadIO.Process.CreateProcOpts
  ( CreateProcOpts, CreateGroup(..), HasCreateProcOpts( createProcOpts )
  -- , HasMockLvl( mockLvl ) -- , MockLvl( MockLvl )
  , cmdName, createGroup, cwd, defCPOpts, defCreateProcOpts{- , defMockLvl -}, env
--  , mock
  )
where

-- base --------------------------------

import Data.Eq        ( Eq )
import Data.Function  ( id )
import Text.Show      ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-default ------------------------

import Data.Default  ( Default, def )

-- env-plus ----------------------------

import Env.Types  ( Env )

-- fpath -------------------------------

import FPath.AbsDir  ( AbsDir )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- more-unicode ------------------------

import Data.MoreUnicode.Maybe  ( 𝕄, pattern 𝕹 )
import Data.MoreUnicode.Text   ( 𝕋 )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.Process.OutputDefault  ( OutputDefault )

--------------------------------------------------------------------------------

-- CreateGroup ---------------------------------------------

data CreateGroup = CreateGroup | NoCreateGroup
  deriving (Eq, Show)

-- CreateProcOpts ------------------------------------------

data CreateProcOpts = CreateProcOpts { _cwd          ∷ 𝕄 AbsDir
                                       , _env          ∷ 𝕄 Env
                                       , _createGroup  ∷ CreateGroup
                                       -- function name (for error messages)
                                       -- NOT the executable name
                                       -- see System.Process.createProcess_
                                       , _cmdName      ∷ 𝕄 𝕋
                                       }

class HasCreateProcOpts c where
  createProcOpts ∷ Lens' c CreateProcOpts
  cmdName        ∷ Lens' c (𝕄 𝕋)
  cmdName        = createProcOpts ∘ cmdName
  createGroup    ∷ Lens' c CreateGroup
  createGroup    = createProcOpts ∘ createGroup
  cwd            ∷ Lens' c (𝕄 AbsDir)
  cwd            = createProcOpts ∘ cwd
  env            ∷ Lens' c (𝕄 Env)
  env            = createProcOpts ∘ env

instance HasCreateProcOpts CreateProcOpts where
  createProcOpts = id
  cmdName        = lens _cmdName     (\ cpo nm  → cpo { _cmdName     = nm  })
  createGroup    = lens _createGroup (\ cpo cg  → cpo { _createGroup = cg  })
  cwd            = lens _cwd         (\ cpo wd  → cpo { _cwd         = wd  })
  env            = lens _env         (\ cpo nv  → cpo { _env         = nv  })

instance Show CreateProcOpts where
  show o = [fmt|CreateProcOpts cwd: %w|] (_cwd o)

defCreateProcOpts ∷ CreateProcOpts
defCreateProcOpts = CreateProcOpts { _cwd         = 𝕹
                                   , _env         = 𝕹
                                   , _createGroup = NoCreateGroup
                                   , _cmdName     = 𝕹
                                   }

-- | default CreateProcOpts, shorter name for convenience
defCPOpts ∷ CreateProcOpts
defCPOpts = defCreateProcOpts

instance Default CreateProcOpts where
  def = defCreateProcOpts

-- that's all, folks! ----------------------------------------------------------
