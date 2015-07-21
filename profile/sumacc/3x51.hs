{-# LANGUAGE BangPatterns #-}
import Control.Exception

sum' :: [Integer] -> Integer -> Integer
sum' [] n = n
sum' (!x:xs) !n = sum' xs (x + n)

ns = [1..9999999]

ns' = filter (\x -> x `mod` 5 == 0 || x `mod` 3 == 0) ns

ans = sum' ns' 0

main = do
    putStrLn $ show ans
