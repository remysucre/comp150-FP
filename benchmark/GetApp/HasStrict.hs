module HasStrict (hasStrict) where

import System.FilePath.Find
import System.Environment 
import System.Process
import Control.Monad
import Data.Functor
import Control.Applicative
import Data.List
import FindHs
import HasBang

hasStrict :: FilePath -> IO Bool
hasStrict fp = do
    srcs <- findHs fp
    srcsWBang <- filterM hasBang srcs 
    return $ length srcsWBang > 0
{-
main = do
    fp <- head <$> getArgs
    result <- hasStrict fp
    putStrLn $ show $ result
    --}
