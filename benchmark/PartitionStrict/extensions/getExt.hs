import System.Environment
import Data.List
import Data.Functor
import Control.Applicative

main = do
    fp <- head <$> getArgs
    fc <- readFile fp
    putStrLn $ unlines $ map (head.words) $ lines fc
