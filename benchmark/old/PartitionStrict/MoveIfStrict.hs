-- Partition course code w/ and w/o strictness into two folders

import System.Directory
import System.Environment
import Data.Functor
import Control.Applicative
import Control.Cond
import System.FilePath.Find
import Control.Monad.IfElse
import HasStrict.HasStrict

moveTo :: FilePath -> FilePath -> IO()
moveTo dir dir' = renameDirectory dir $ dir' ++ "/" ++ dir

main = do {
    dir <- head <$> getArgs;
    ifM (hasStrict dir)
        (do {
            dir `moveTo` "strictapps";
            putStrLn $ "package " ++ dir ++ " has bang"
        })
        (do {
            dir `moveTo` "nonstrict";
            putStrLn $ "package " ++ dir ++ " has no bang"
        })
    }
