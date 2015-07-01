module Data.Graph.IdMap where

import Data.IdMap

import qualified Data.List as List

------------------------------------

type Children a = a -> [a]

x .: ~(st, l) = (st, x: l)

depthFirstWalk' :: I m => Children (Id a) -> Set m a -> [Id a] -> (Set m a, [Id a])
depthFirstWalk' children s [] = (s, [])
depthFirstWalk' children s (h: t)
    | h `member` s = depthFirstWalk' children s t
    | otherwise = h .: depthFirstWalk' children (setInsert h s) (children h ++ t)


depthFirstWalk :: I m => Children (Id a) -> Set m a -> [Id a] -> [Id a]
depthFirstWalk children _s [] = []
depthFirstWalk children s (h: t)
    | h `member` s = depthFirstWalk children s t
    | otherwise = h : depthFirstWalk children (setInsert h s) (children h ++ t)

{-
postOrderWalk :: I m => Children (Id a) -> Set m a -> [Id a] -> [Id a]
postOrderWalk children _s [] = []
postOrderWalk children s (h: t)
        | h `member` s = postOrderWalk children s t
        | otherwise = postOrderWalk children (setInsert h s) (children h) ++ [h] : t
-}

data Task a = Return a | Visit a

postOrderWalk :: I m => Children (Id a) -> Set m a -> [Id a] -> [Id a]
postOrderWalk children s l = collect s $ map Visit l where

    collect _s [] = []
    collect s (Return h: t) = h: collect s t
    collect s (Visit h: t)
        | h `member` s = collect s t
        | otherwise = collect (setInsert h s) $ map Visit (children h) ++ Return h: t


scc :: I m => Set m a -> Set m a -> Children (Id a) -> Children (Id a) -> [Id a] -> [[Id a]]
scc k k' children revChildren l 
    = reverse $ filter (not . null) $ mapWalk k revChildren l' where

        l' = reverse (postOrderWalk children k' l)

mapWalk :: I m => Set m a -> Children (Id a) -> [Id a] -> [[Id a]]
mapWalk k children l = f k l
 where
    f _s [] = []
    f s (h:t) = c : f s' t
        where (s', c) = collect s [] [h]

    -- collect :: Set a -> [a] -> [a] -> (Set a, [a])
    collect s acc [] = (s, acc)
    collect s acc (h:t)
        | h `member` s = collect s acc t
        | otherwise = collect (setInsert h s) (h: acc) (children h ++ t)

-----------------------------------------------------------
{-
-- megkeressük azokat a csúcsokat, amelyekre többen is hivatkoznak
findShared 
    ::  k
    => Bool     -- számoljuk-e még egyszer a gyökereket
    -> Bool     -- nézzük-e a gyerektelen csúcsokat
    -> Children (Id a)
    -> [Id a]       -- roots
    -> [Id a]

findShared countRoots countLeafs ch roots = filter double nodes where

    nodes = walk k1 ch roots

    inv = inverse ch nodes

    double x 
        | countLeafs    = numOfParents x > 1
        | otherwise = length (ch x) > 0 && numOfParents x > 1

    numOfParents x
        | countRoots && isRoot x = 1 + length (inv x)
        | otherwise = length (inv x)

    isRoot = flipElem roots
-}

{-

data Task' a = Down a | Up a

downUp i = [Down i, Up i]

-- keresünk olyan csúcsokat, amelyeknek a kivétele megszünteti a ciklusokat
breakCycles :: Empty k -> Children (Id a) -> [Id a] -> [Id a]
breakCycles k children roots = collect (emptySet k) (emptySet k) $ concatMap downUp roots where

    -- collect :: Set a -> [a] -> [a]
    collect parents visited [] = []
    collect parents visited (Up h:t)
        = collect (delete h parents) visited t
    collect parents visited (Down h:t)
        | member h parents = h : collect parents visited t
        | member h visited = collect parents visited t
        | otherwise = collect (setInsert h parents) (setInsert h visited) $ concatMap downUp (children h) ++ t


cyclic, acyclic :: Empty k -> Children (Id a) -> [Id a] -> Bool

acyclic k ch r = List.null $ breakCycles k ch r

cyclic k ch r = not $ acyclic k ch r

---------

mapg :: Empty k1 -> Empty k2 -> Children (Id a) -> ((Id a->b) -> Id a -> b) -> [Id a] -> [b]
mapg k1 k2 ch h nodes = map f nodes where
    f = memo k1 (h f) (walk k2 ch nodes)
{-
mapg' :: Empty k -> Children (Id a) -> ((a->b) -> a -> PreIds p -> b) -> [a] -> PreIds p -> [b]
mapg' ch h nodes ids = map f nodes where
    f = memo' (h f) (walk ch nodes) ids
-}
-}


