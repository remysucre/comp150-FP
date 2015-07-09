import System.Environment
import Control.Applicative
import Data.Functor
import Data.List

gridToNums g = map (map read . words) $ lines g

largeprod :: String -> Int
largeprod fc = product $ big4 $ gridToNums fc

big4 :: [[Int]] -> [Int]

fours :: 
-- like thirteens


fourDiag ::
-- intersperse 4 lists and call take 4s

main = do
    fc <- (head <$> getArgs) >>= readFile 
    let ans = largeprod fc
    putStr $ show ans
