import Data.IdMap

import Data.Graph.IdMap.Tests

type Children k = Id k -> [Id k]

depthFirstWalk :: I i => Children k -> Set i k -> [Id k] -> [Id k]
depthFirstWalk children _s [] = []
depthFirstWalk children s (h: t)
    | h `member` s = depthFirstWalk children s t
    | otherwise = h : depthFirstWalk children (setInsert h s) (children h ++ t)


data Task a = Return a | Visit a

postOrderWalk :: I i => Children k -> Set i k -> [Id k] -> [Id k]
postOrderWalk children s = collect s . map Visit where

    collect _s [] = []
    collect s (Return h: t) = h: collect s t
    collect s (Visit h: t)
        | h `member` s = collect s t
        | otherwise = collect (setInsert h s) $ map Visit (children h) ++ Return h: t


revPostOrderWalk :: I i => Children k -> Set i k -> [Id k] -> (Set i k, [Id k])
revPostOrderWalk children s = collect s [] . map Visit where

    collect s acc [] = (s, acc)
    collect s acc (Return h: t) = collect s (h: acc) t
    collect s acc (Visit h: t)
        | h `member` s = collect s acc t
        | otherwise = collect (setInsert h s) acc $ map Visit (children h) ++ Return h: t


mapWalk :: I i => Children k -> Set i k -> [Id k] -> [[Id k]]
mapWalk children = f
 where
    f _s [] = []
    f s (h:t) = c : f s' t
        where (s', c) = collect s [] [h]

    -- collect :: Set a -> [a] -> [a] -> (Set a, [a])
    collect s acc [] = (s, acc)
    collect s acc (h:t)
        | h `member` s = collect s acc t
        | otherwise = collect (setInsert h s) (h: acc) (children h ++ t)

revMapWalk :: I i => Children k -> Set i k -> [Id k] -> [[Id k]]
revMapWalk children = f []
 where
    f acc s [] = acc
    f acc s (h:t) = f (c: acc) s' t
        where (s', c) = collect s [] [h]

    -- collect :: Set a -> [a] -> [a] -> (Set a, [a])
    collect s acc [] = (s, acc)
    collect s acc (h:t)
        | not (h `member` s) = collect s acc t
        | otherwise = collect (delete h s) (h: acc) (children h ++ t)

scc :: I i => Children k -> Children k -> Set i k -> [Id k] -> [[Id k]]
scc children revChildren s
    = filter (not . null) . uncurry (revMapWalk revChildren) . revPostOrderWalk children s


----------------

prWalk :: (I i, I j) => Map i k [Id k] -> Map j k Int -> Id k -> [Id k]
prWalk m0 n t = follow m0 n t t where

    follow m n x t = case lookUp t n of
        Nothing    -> t: case lookUp t m of
            Just (l:r)  -> follow (insert t (x:r) m) (insert t 0 n) t l
            _           -> back m (insert t 0 n) t x
        _  -> back m n t x        -- the node or leaf is already visited

    back m n x t | x==t = unsafeEquivalent m0 m `seq` []
    back m n x t = case m ! t of
        l  -> case lookUp t n of
            Just i | i + 1 == length l -> back   (insert t (replaceLast l x) m) (insert t (i+1) n) t (last l)
            Just i    -> follow (insert t (replaceAndShiftOne i l x) m) (insert t (i+1) n) t (l !! (i+1))

replaceLast :: [a] -> a -> [a]
replaceLast [x] y = [y]
replaceLast (x:xs) y = x: replaceLast xs y

replaceAndShiftOne :: Int -> [a] -> a -> [a]
replaceAndShiftOne 0 (c:_:cs) x = (x:c:cs)
replaceAndShiftOne n (c:cs) x = c: replaceAndShiftOne (n-1) cs x


