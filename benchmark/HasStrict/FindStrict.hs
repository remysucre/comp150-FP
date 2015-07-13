--module HasStrict.HasStrict (hasStrict) where

import System.FilePath.Find
import System.Environment 
import System.Process
import Control.Monad
import Data.Functor
import Control.Applicative
import Data.List
import System.FilePath.Find
--import HasStrict.HasBang
import HasBang

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p [] = return False
anyM p (x : xs) = liftM2 (||) (p $! x) $ anyM p xs

hasStrict :: FilePath -> IO FilePath
hasStrict fp = do
    srcs <- findHs fp
    head <$> filterM hasBang srcs

findHs = System.FilePath.Find.find always (extension ==? ".hs") 

-- test: comment out module declaration to test

main = do
    fp <- head <$> getArgs
    result <- hasStrict fp
    putStrLn result
