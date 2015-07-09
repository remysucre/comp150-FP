{-# LANGUAGE BangPatterns #-}
module Main (main) where
import Control.Exception
main = do evaluate (fst (f [1 .. 4000000] (0 :: Int, 1 :: Int)))
f [] !c = c
f (x : xs) c = f xs (tick x c)
tick x (c0, c1)
  | even x = (c0, c1 + 1)
  | otherwise = (c0 + 1, c1)