foldl' f z []     = z
foldl' f z (x:xs) = let z' = z `f` x 
                    in seq z' $ foldl' f z' xs
 
sum3 = foldl' (+) 0
  
try3 = sum3 veryBigList
