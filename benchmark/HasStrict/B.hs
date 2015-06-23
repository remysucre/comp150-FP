import System.Environment

clength !xs = length' xs 0
  where length' []     n = n
        length' (x:xs) n = length' xs (n + 1)

main = do
    l <- getArgs
    putStrLn $ show (clength (take (read $ head l) [0..]))
