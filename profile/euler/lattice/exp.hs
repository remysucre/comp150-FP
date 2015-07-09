import Data.List

aps :: Int -> (a -> a) -> a -> a
aps n f = foldr (.) id (replicate n f)

paths :: Num a => Int -> Int -> a
paths r c = last $ r `aps` scanl (+) 0 $ replicate (c + 1) 1
