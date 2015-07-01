module Data.Graph.IdMap.Tests where

import Data.List.IdMap
import Data.IdMap
import Data.Graph.IdMap
import Test.HUnit

import Data.IORef
import System.IO.Unsafe

toFunction :: I i => Map i k [a] -> Id k -> [a]
toFunction m x = flattenJust $ lookUp x m

flattenJust Nothing = []
flattenJust (Just l) = l

relationToFunction :: Eq a => [(a, b)] -> a -> [b]
relationToFunction l x = [ns | (n, ns) <- l, n == x]

testWalk :: (forall k i. I i => Children (Id k) -> Set i k -> [Id k] -> [Id k]) -> [Char] -> [Char]
testWalk walk ns = withGraph (\toChar fromChar ch _ s _ _ -> map toChar $ walk (toFunction ch) s $ map fromChar ns)

testPrWalk :: (forall k i i'. (I i, I i') => Map i k [Id k] -> Map i' k Int -> Id k -> [Id k]) -> Char -> [Char]
testPrWalk walk n = withGraph (\toChar fromChar ch _ s _ m -> map toChar $ walk ch m $ fromChar n)

testMapWalk :: (forall k i. I i => Children (Id k) -> Set i k -> [Id k] -> [[Id k]]) -> [Char] -> [[Char]]
testMapWalk walk ns = withGraph (\toChar fromChar ch _ s _ _ -> map (map toChar) $ walk (toFunction ch) s $ map fromChar ns)

testSCC :: (forall k i. I i => Children (Id k) -> Children (Id k) -> Set i k -> [Id k] -> [[Id k]]) -> [Char] -> [[Char]]
testSCC scc ns = withGraph (\toChar fromChar ch revCh s _ _ -> map (map toChar) $ scc (toFunction ch) (toFunction revCh) s $ map fromChar ns)

withGraph :: forall a . (forall k i i' i'' i''' i4. (I i, I i', I i'', I i''', I i4) => (Id k -> Char) -> (Char -> Id k) -> Map i'' k [Id k] -> Map i''' k [Id k] -> Set i k -> Set i' k -> (forall x . Map i4 k x) -> a) -> a
withGraph fun = runICCS iccs  where

    iccs :: ICCS I3 k a
    iccs (m1 `PlusMap` m2 `PlusMap` m3 `PlusMap` _) (s1 `PlusSet` s2 `PlusSet` _) ids = fun toChar fromChar chm revchm s1 s2 m3      where

        ids' = take 11 ids

        (a:b:c:d:e:f:g:h:i:j:k:_) = ids

        chm = fromList' m1 $ reverse l
        revchm = fromList' m2 $ map swap $ reverse l

        fromChar = head . relationToFunction (zip ['A'..] ids')
        toChar = head . relationToFunction (zip ids' ['A'..])

        l = [ (a, b)
            , (a, c)
            , (b, d)
            , (b, e)
            , (c, f)
            , (c, g)
            , (f, a)
            , (g, h)
            , (h, g)
            , (i, h)
            , (j, k)
            ]

        swap (a, b) = (b, a)



tests :: IO Counts
tests = runTestTT $ TestList 

    [ "depthFirstWalk" ~: testWalk depthFirstWalk "A" ~=? "ABDECFGH"
    , "postOrderWalk" ~: testWalk postOrderWalk "A" ~=? "DEBFHGCA"
    ]
{-
 let l = [1 :: Int ..10] 
      in "dlist insert pop" ~: l ~=? (toList $ fromList l)

    , let (a,b) = ([1 :: Int ..11], [40..50])       -- ide nem szabad 10-et írni...
      in "dlist join" ~: (a++b) ~=? (toList $ fromList a >< fromList b)

    , let l = [[x..x+5] | x<-map (10*) [1 :: Int ..10]]
      in "dlist joins" ~: concat l ~=? (toList $ foldl' (><) empty (map fromList l))
    ]
-}



