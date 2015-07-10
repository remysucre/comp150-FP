import Data.List

fibs = unfoldr (\(f1,f2) -> Just (f1,(f2,f1+f2))) (0,1)

evenFibs = [x | x <- fibs, even x]
evenFibs' = [x | x <- fibs, x < 4000000, even x]

ans = sum [x | x <- fibs, x < 4000000, even x]
ans' = sum $ filter even $ takeWhile (< 4000000) fibs
