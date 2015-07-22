import Data.List

nthPrime n = nthPrime' n (1, 2)

nthPrime' 0 (prd, p) = p
nthPrime' n (prd, p) = nthPrime' (n - 1) $ nextPrime (prd, p)

nextPrime :: (Integer, Integer) -> (Integer, Integer)
nextPrime (prd, n)
    | gcd prd n /= 1 = nextPrime (prd, n + 1)
    | gcd prd n == 1 = (prd * n, n)

main = do
    print $ nthPrime 99999
