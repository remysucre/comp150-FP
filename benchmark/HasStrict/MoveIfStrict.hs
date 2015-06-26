-- Partition course code w/ and w/o strictness into two folders

import System.Directory
import System.Environment
import Data.Functor
import Control.Applicative
import System.FilePath.Find
import DirHasStrict

hasBang :: FilePath -> FindClause Bool
hasBang _ = always

moveTo :: FilePath -> FilePath -> IO()
moveTo dir dir' = renameDirectory dir $ dir' ++ "/" ++ dir

main = do 
    dir <- head <$> getArgs
    if dirHasStrict dir then dir `moveTo` "strictapps" else return ()
