import Control.Exception

ans = sum [x | x <- [1..99999999], x `mod` 5 == 0 || x `mod` 3 == 0]

main = do
    evaluate ans
