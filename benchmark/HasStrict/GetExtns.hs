module GetExtns (getExtns) where

import System.Environment
import Data.List
import Control.Applicative
import Data.Functor

getExtns :: String -> IO [String]
getExtns fp = do
    fc <- readFile fp
    return $ lines fc
{--
main = do
    fp <- head <$> getArgs
    l <- getExtns fp
    putStrLn $ show $ l 
    --}
