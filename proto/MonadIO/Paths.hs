module MonadIO.Paths where

import FPath.AbsFile  ( AbsFile, absfile )

grep :: AbsFile
grep = [absfile|__gnugrep__/bin/grep|]
