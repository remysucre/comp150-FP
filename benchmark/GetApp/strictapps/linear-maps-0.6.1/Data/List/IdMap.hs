module Data.List.IdMap
    where

import Data.IdMap

import qualified Data.List as List
import Data.Maybe

-----------------------------------------------------------------------------------

nubId :: I m => Set m a -> [Id a] -> (Set m a, [Id a])
nubId s [] = (s, [])
nubId s (h:t)
    | h `member` s   = nubId s t
    | otherwise         = h `add` nubId (setInsert h s) t
 where
    add :: a -> (b, [a]) -> (b, [a])
    x `add` ~(s, xs) = (s, x:xs)


fromList' :: I m => Map m a [b] -> [(Id a, b)] -> Map m a [b]
fromList' = List.foldl' f where

    f m (a,b) = case lookUp a m of
        Nothing -> insert a [b] m
        Just bs -> insert a (b:bs) m


