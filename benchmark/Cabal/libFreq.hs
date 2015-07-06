import System.Environment
import Control.Applicative
import Data.Functor
import Data.List

insertF :: String -> [(Int, String)] -> [(Int, String)]
insertF s [] = [(1, s)]
insertF x ((n, y):yns)
    | x == y    = (n + 1, y):yns
    | otherwise = (1, x):(n, y):yns

sortFreq :: [String] -> [(Int, String)]
sortFreq = sortBy cmpPair . foldr insertF []

cmpPair :: (Int, String) -> (Int, String)-> Ordering
cmpPair (n1, _) (n2, _) = compare n1 n2

main = do
    fp <- head <$> getArgs
    fc <- readFile fp
    putStr $ unlines $ map show $ sortFreq $ lines fc
