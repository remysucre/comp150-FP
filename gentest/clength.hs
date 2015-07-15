clength xs = length' xs 0
  where length' []     n = n
        length' (x:xs) n = length' xs $! (n + 1)

len = length [1..1000000]

main = 
    putStrLn $ show len
