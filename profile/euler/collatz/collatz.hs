import Data.List

collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n =
    filter (< 1000000) prev >>= poss
    where prev = collatz (n - 1)

poss :: Integer -> [Integer]
poss prev
    | even prev && prev `mod` 3 == 1 && (prev - 1) `div` 3 > 1 = [2 * prev, (prev - 1) `div` 3]
    | otherwise = [2 * prev]

collatzof 1 = 1
collatzof n
    | even n = n `div` 2
    | otherwise = 3 * n + 1

collatzleng 1 = 1
collatzleng n = 1 + collatzleng (collatzof n)
