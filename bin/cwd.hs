-- base --------------------------------

import Data.Function  ( ($) )
import System.Exit    ( ExitCode( ExitFailure ), exitWith )
import System.IO      ( IO, hPutStrLn, putStrLn, stderr )

-- data-textual ------------------------

import Data.Textual  ( toString )

-- fpath -------------------------------

import FPath.Error.FPathError  ( FPathIOError )

-- monaderror-io -----------------------

import MonadError  ( ѥ )

-- more-unicode ------------------------

import Data.MoreUnicode.Either  ( pattern 𝕷, pattern 𝕽 )
import Data.MoreUnicode.Monad   ( (≫), (⪼) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.Cwd  ( getCwd )

--------------------------------------------------------------------------------

main ∷ IO ()
main = do
  ѥ @FPathIOError getCwd ≫ \ case
    𝕽 cwd → putStrLn $ toString cwd
    𝕷 e   → hPutStrLn stderr (toString e) ⪼ exitWith (ExitFailure 255)

-- that's all, folks! ----------------------------------------------------------
