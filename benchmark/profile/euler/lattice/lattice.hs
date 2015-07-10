import Control.DeepSeq
import Control.Exception.Base

nextsteps :: Int -> (Int, Int) -> [(Int, Int)]
nextsteps n (x, y)
    | x < n && y < n = [(x + 1, y), (x, y + 1)]
    | x == n && y == n = [(x, y)]
    | x == n && y < n = [(x, y + 1)]
    | x < n && y == n = [(x + 1, y)]

paths 0 0 = 1
paths x 0 = paths (x - 1) 0
paths 0 y = paths 0 (y - 1)
paths x y = paths (x - 1) y + paths x (y - 1)

paths2 0 _ ps = ps
paths2 n e ps = paths2 (n - 1) e $!! (ps >>= nextsteps e)

paths1 0 _ ps = ps
paths1 n e ps = paths1 (n - 1) e $! (ps >>= nextsteps e)

paths0 0 _ ps = ps
paths0 n e ps = paths0 (n - 1) e $ (ps >>= nextsteps e)

ans0 = length $ paths0 22 11 [(0, 0)]
ans1 = length $ paths1 22 11 [(0, 0)]
ans2 = length $ paths2 22 11 [(0, 0)]

main = do 
    evaluate ans2
