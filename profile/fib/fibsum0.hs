{-# LANGUAGE BangPatterns #-}
import Data.List

fib 0 (_, b)  = b
fib n (a, b) = fib (n - 1) (b, a + b)
{-
fibs = unfoldr (\(f1,f2) -> Just (f1,(f2,f1+f2))) (0,1)

evenFibs = [x | x <- fibs, even x]
evenFibs' = [x | x <- fibs, x < 4000000, even x]

ans = sum [x | x <- fibs, x < 4000000, even x]
ans' = sum $ filter even $ takeWhile (< 999999999) fibs
-}
fiba = fib 500000 (0, 1)

main = do
    putStrLn $ show fiba
