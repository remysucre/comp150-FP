{-# LANGUAGE NoBangPatterns #-}

module Data.LinkMap 
    ( module Data.IdMap

    , LinkMap, linkMap
    , link, follow
    , lookUp, insert, delete, union
    , member, notMember
    , same
    , (!), fromList
    ) where

import qualified Data.IdMap as M
import Data.IdMap hiding 
    (lookUp, insert, inserts, delete, union, member, (!))

import Data.Maybe
import Data.List (foldl')

----------------------------------------

data Pointer k a
    = Link !k
    | Data a

instance Functor (Pointer k) where fmap _f = error "fmap@Pointer"
instance Functor2 Pointer where fmap2 _f = error "fmap2@Pointer"

newtype LinkMap i k a = L (M.Map i k (Pointer (Id k) a))

instance Functor (LinkMap i k) where fmap _f = error "fmap@LinkMap"


linkMap :: (forall b. Map i k b) -> LinkMap i k a
linkMap m = L m

-- Azonosítunk két kulcsot.
-- Az első kulcshoz tartozó érték elvész.
link    :: I i => Id k -> Id k -> LinkMap i k a -> LinkMap i k a
link a b m@(L mm)
    | equalBy mm ua ub   = m 
    | otherwise = L $ M.insert ua (Link ub) mm
 where
    ua = follow m a
    ub = follow m b

follow :: I i => LinkMap i k a -> Id k -> Id k
follow m@(L mm) a = case M.lookUp a mm of
    Just (Link b)   -> case M.lookUp b mm of
        Just (Link c)   -> let
                d = follow m c
            in d `seq` M.unsafeInsert a (Link d) mm `seq` d
        _   -> b
    _   -> a

{- slow version
follow (M m) a = case M.lookup a m of
    Nothing -> a
    Just b  -> follow (M m) b
-}

lookUp :: I i => Id k -> LinkMap i k a -> Maybe a
lookUp i m@(L mm) = case M.lookUp (follow m i) mm of
    Just (Data a)   -> Just a
    _               -> Nothing

insert :: I i => Id k -> a -> LinkMap i k a -> LinkMap i k a
insert i a m@(L mm) = L $ M.insert (follow m i) (Data a) mm

delete :: I i => Id k -> LinkMap i k a -> LinkMap i k a
delete i m@(L mm) = L $ M.delete (follow m i) mm

infixr 2 `union`

union :: LinkMap i k a -> LinkMap i l a -> LinkMap i (k :|: l) a
L a `union` L b = L (fmap (fmap2 left) a `M.union` fmap (fmap2 right) b)


same :: I i => LinkMap i k a -> Id k -> Id k -> Bool
same m@(L mm) a b = equalBy mm (follow m a) (follow m b)

--------

-- fromList :: [(Id k, Id k)] -> FastLink k
-- fromList l = foldl (\m (a,b)->link a b m) empty l

--unions :: LinkKey k => [Link k] -> Link k
--unions = foldl union separated


----------------------

infixl 8 !      -- 9 lenne, de ~> miatt 8

(!) :: I i => LinkMap i k a -> Id k -> a
m ! i = maybe (error "Data.LinkMap.!") id (lookUp i m)

member :: I i => Id k -> LinkMap i k a -> Bool
member i = isJust . lookUp i

notMember :: I i => Id k -> LinkMap i k a -> Bool
notMember i = not . member i

fromList :: I i => (forall b. Map i k b) -> [(Id k, a)] -> LinkMap i k a
fromList = foldl' (\m (i,x) -> insert i x m) . linkMap

