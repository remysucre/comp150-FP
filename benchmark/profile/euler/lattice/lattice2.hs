import Control.DeepSeq
import Control.Exception.Base

nextsteps :: Int -> (Int, Int) -> [(Int, Int)]
nextsteps n (x, y)
    | x < n && y < n = [(x + 1, y), (x, y + 1)]
    | x == n && y == n = [(x, y)]
    | x == n && y < n = [(x, y + 1)]
    | x < n && y == n = [(x + 1, y)]

paths2 0 _ ps = ps
paths2 n e ps = paths2 (n - 1) e $!! (ps >>= nextsteps e)

ans2 = paths2 22 11 [(0, 0)]

main = do 
    putStr $ show ans2
