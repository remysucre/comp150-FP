primes :: [Integer]
primes = filter isPrime [2..]
    where isPrime n = not $ any (\x -> n `mod` x == 0) [2..n-1]

ans = max [x | x <- primes, 600851475143 `mod` x == 0]
{-
ans' = filter (\n -> 600851475143 `mod` n == 0) primes

primeFactorsOf :: Integer -> [Integer]
primeFactorsOf n = aPF : primeFactorsOf (n `div` aPF)
    where aPF = aPFof n

aPFof :: Integer -> Integer
aPFof n = head $ filter (\p -> n `mod` p == 0) primes

ans = maximum $ primeFactorsOf 600851475143 
-}
