module Data.Sequence.Profile
    ( prof1
    , prof2
    ) where

import Data.Sequence hiding (take)
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

toList :: Seq a -> [a]
toList s = case viewl s of
    EmptyL  -> []
    a :< ss -> a: toList ss

