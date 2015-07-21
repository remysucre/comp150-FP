import System.Environment
import Control.Applicative
import Data.Functor
import Data.List

lines2ns :: String -> [Integer]
lines2ns = map read . lines

largesum :: String -> String
largesum = show . sum . lines2ns

main = do
    fc <- (head <$> getArgs) >>= readFile 
    let ans = largesum fc
    putStr ans
