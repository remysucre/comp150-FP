-- Partition course code w/ and w/o strictness into two folders

import System.Directory
import System.Environment
import Data.Functor
import Control.Applicative
import System.FilePath.Find

-- if hasstrict file then move to strict app: concurrently
{-
moveIfStrict :: Filepath -> IO()
moveIfStrict = 

childrenHaveStrict :: Filepath -> Bool
-}

hasBang :: FilePath -> FindClause Bool
hasBang _ = always

main = do 
    createDirectoryIfMissing False "strictapps" 
    dirs <- getDirectoryContents "."
    --map moveIfStrict dirs
    files <- find always hasBang "."
    putStrLn $ concat files
    return ()
