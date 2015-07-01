module Data.Sequence.IdMap.Profile
    ( prof1
    , prof2
    ) where

import Data.Sequence.IdMap
import Data.List

prof1 :: Int -> Int -> Int
prof1 a b 
    = sum $ toList $ foldl' (><) empty $ map fromList $ take a [take b [x..] | x<- [0::Int,10..]]

prof2 :: Int -> Int
prof2 n 
    = sum $ toList $ f n 0
 where
    f :: Int -> Int -> Seq Int
    f 0 i = singleton i
    f n' i = f (n'-1) i >< f (n'-1) (i+1)


