{-# LANGUAGE BangPatterns #-}

leaky_sequence [] = [[]]
leaky_sequence (xs:xss) = [ y:ys | y <- xs, ys <- leaky_sequence xss ]

ans = leaky_sequence $ replicate 5 [1..9]

main = do
    print ans
