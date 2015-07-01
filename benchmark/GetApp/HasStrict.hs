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

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p [] = return False
anyM p (x : xs) = liftM2 (||) (p $! x) $ anyM p xs

hasStrict :: FilePath -> IO Bool
hasStrict fp = do
    srcs <- findHs fp
    anyM hasBang srcs

main = do
    fp <- head <$> getArgs
    result <- hasStrict fp
    putStrLn $ show $ result