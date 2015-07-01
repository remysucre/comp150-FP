module Data.IdSequence
    ( Seq
    , previous
    , next
    , value
    , member
    , update
    , delete
    , insertBefore
    , fromList
    ) where

import Data.IdMap hiding (insert, delete, member)
import qualified Data.IdMap as M

import Prelude hiding (last)


------------------------------------------

data Seq k a = Seq
            {-# UNPACK #-} !(Map I0 k (Id k))
            {-# UNPACK #-} !(Map I1 k (Id k))
            {-# UNPACK #-} !(Map I2 k a)

previous :: Seq k a -> Id k -> Maybe (Id k)
previous (Seq p _n _v) i = lookUp i p

next :: Seq k a -> Id k -> Maybe (Id k)
next (Seq _p n _v) i = lookUp i n

value :: Seq k a -> Id k -> Maybe a
value (Seq _p _n v) i = lookUp i v

member :: Id k -> Seq k a -> Bool
member i (Seq _p _n v) = M.member i v

delete :: Id k -> Seq k a -> Seq k a
delete i s@(Seq p n v) 
    | member i s = case (previous s i, next s i) of
        (Just j, Just k) -> Seq (M.insert k j p) (M.insert j k n) (M.delete i v)
        (Just j, _)      -> Seq p                (M.delete j n)   (M.delete i v)
        (_,      Just k) -> Seq (M.delete k p)   n                (M.delete i v)
        _                -> Seq p                n                (M.delete i v)

update :: Id k -> a -> Seq k a -> Seq k a
update i a s@(Seq p n v) 
    | member i s = Seq p n (M.insert i a v)

insertBefore :: forall k a e . Id k -> a -> Seq k a -> (forall k'. Seq (k :|: k') a -> e) -> e
insertBefore i a s@(Seq p n v) f 
    | member i s = runICC g where

        g :: ICC I2 v e
        g (v' `PlusMap` n' `PlusMap` _) p' (k:_) = case previous s i of

            Just j -> f $ Seq (M.insert i' k' $ M.insert k' (left j) $ fmap left p `union` fmap right p') 
                              (M.insert (left j) k' $ M.insert k' i' $ fmap left n `union` fmap right n')
                              (M.insert k' a $ v `union` v')
            _      -> f $ Seq (M.insert i' k' $ fmap left p `union` fmap right p') 
                              (M.insert k' i' $ fmap left n `union` fmap right n')
                              (M.insert k' a $ v `union` v')

         where
            k' = right k
            i' = left i

fromList :: forall a e . [a] -> (forall k. Id k -> Id k -> Seq k a -> e) -> e
fromList [] _ = error "IdSequence.fromList: empty list"
fromList (a:as) f = runICC g where

    g :: ICC I2 k e
    g (v `PlusMap` n `PlusMap` _) p (i:is) = h as is i i (Seq p n (M.insert i a v))

    h :: [a] -> [Id k] -> Id k -> Id k -> Seq k a -> e
    h (b:bs) (i:is) k j !(Seq p n v) = h bs is k i $ Seq (M.insert i j p) (M.insert j i n) (M.insert i b v)
    h []     _      k j s = f k j s
    h _      _      _ _ _ = error "impossible: no more ids!"





