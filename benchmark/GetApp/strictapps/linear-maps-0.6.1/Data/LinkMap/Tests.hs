module Data.LinkMap.Tests 
    ( tests
    ) where

import Data.LinkMap
import Test.HUnit

import Prelude hiding (null, catch)

-------------------- 

run :: forall b. (forall x. (forall a. LinkMap I0 x a) -> [Id x] -> b) -> b
run f = runICC g  where

    g :: ICC I0 v b
    g _ m ids = f (linkMap m) ids

testList :: [Bool] -> Test
testList = TestList . map (TestCase . assert)

tests :: IO Counts
tests = runTestTT $ TestList
    [ "LinkMap tests" ~: TestList (map testList list)
    ]

list :: [[Bool]]
list =
    [ run $ \m (i1:_) -> 
        [same m i1 i1]
    , run $ \n (i1:_) -> 
        let m = link i1 i1 n
        in [same m i1 i1, notMember i1 m]
    , run $ \n (i1:_) -> 
        let m = link i1 i1 $ insert i1 (1 :: Int) n
        in [same m i1 i1, lookUp i1 m == Just 1]
    , run $ \n (i1:i2:_) -> 
        let m = link i1 i2 $ link i2 i1 $ insert i1 (1 :: Int) n
        in [same m i1 i2, lookUp i1 m == Just 1, lookUp i2 m == Just 1]
    , run $ \n (i1:i2:i3:_) -> 
        let m = link i3 i1 $ link i2 i3 $ link i2 i1 $ insert i1 (1 :: Int) n
        in [lookUp i1 m == Nothing, lookUp i2 m == Nothing, lookUp i3 m == Nothing]
    , run $ \n (i1:i2:_) -> 
        let m = link i1 i2 n 
        in [same m i1 i2, same m i2 i1]
    , run $ \n (i1:i2:_) -> 
        let m = n 
        in [not (same m i1 i2), not (same m i2 i1)]
    , run $ \n (i1:i2:i3:_) -> 
        let m = link i3 i2 $ link i2 i1 n
        in [same m i1 i2, same m i1 i3, same m i2 i3]
    , run $ \n (i1:i2:i3:_) -> 
        let m = link i3 i1 $ link i2 i1 n 
        in [same m i1 i2, same m i1 i3, same m i2 i3]
    , run $ \n (i1:i2:i3:_) -> 
        let m = link i2 i3 $ link i2 i1 n 
        in [same m i1 i2, same m i1 i3, same m i2 i3]
    , run $ \n (i1:i2:i3:_) -> 
        let m = link i1 i3 $ link i2 i1 n 
        in [same m i1 i2, same m i1 i3, same m i2 i3]
    , run $ \n (i1:i2:_) -> 
        let m = link i1 i2 $ insert i1 (1 :: Int) n 
        in [lookUp i1 m == Nothing, lookUp i2 m == Nothing]
    , run $ \n (i1:i2:_) -> 
        let m = link i1 i2 $ insert i2 (1 :: Int) n 
        in [lookUp i1 m == Just 1, lookUp i2 m == Just 1]
    , run $ \n (i1:i2:i3:_) -> 
        let m = link i3 i2 $ link i2 i1 $ insert i1 (1 :: Int) n 
        in [lookUp i1 m == Just 1, lookUp i2 m == Just 1, lookUp i3 m == Just 1]
    -- ...
    ]



