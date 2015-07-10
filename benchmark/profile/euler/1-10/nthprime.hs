import Data.List

primes' :: [Integer]
primes' = unfoldr nextPrime (1, 2)

nextPrime :: (Integer, Integer) -> Maybe (Integer, (Integer, Integer))
nextPrime (prd, n)
    | gcd prd n /= 1 = nextPrime (prd, n + 1)
    | gcd prd n == 1 = Just (n, (prd * n, n + 1))
