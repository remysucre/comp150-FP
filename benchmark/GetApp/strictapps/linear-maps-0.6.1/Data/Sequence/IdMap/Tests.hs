
module Data.Sequence.IdMap.Tests where


import Data.Sequence.IdMap
import Test.HUnit
import Data.List (foldl')

---------------------------

tests :: IO Counts
tests = runTestTT $ TestList 

    [ let l = [1 :: Int ..10] 
      in "dlist insert pop" ~: l ~=? (toList $ fromList l)

    , let (a,b) = ([1 :: Int ..11], [40..50])       -- ide nem szabad 10-et írni...
      in "dlist join" ~: (a++b) ~=? (toList $ fromList a >< fromList b)

    , let l = [[x..x+5] | x<-map (10*) [1 :: Int ..10]]
      in "dlist joins" ~: concat l ~=? (toList $ foldl' (><) empty (map fromList l))
    ]


