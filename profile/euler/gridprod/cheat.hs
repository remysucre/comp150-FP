import System.Environment
import Control.Applicative
import Data.Functor
import Data.List

gridToNums :: String -> [[Int]]
gridToNums g = map (map read . words) $ lines g

sieve :: [[Int]] -> String
sieve g = unlines $ map (unwords . map (\n -> if n > 80 then show n else "  ")) g

largeprod :: String -> String
largeprod = sieve . gridToNums

main = do
    fc <- (head <$> getArgs) >>= readFile 
    let ans = largeprod fc
    putStr ans
