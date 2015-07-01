{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeOperators, GADTs #-}
{-# OPTIONS_GHC -fcontext-stack=33 #-}
module Data.IdMap.Core.Pure
    ( module Data.Subtyping
    , module Data.TypeInt
    , module Control.Functor

    -- * Identifiers
    , Id
    , equalBy

    -- * Finite maps and sets
    , Map
    , Set
    , insert
    , delete
    , lookUp
    , member
    , union

    , unsafeInsert
    , unsafeEquivalent

    -- * Range of sets and maps
    , Sets (PlusSet)
    , Maps (PlusMap)

    -- * Creation of sets, maps and identifiers
    , ICC,  runICC
    , ICCS, runICCS
    ) where

------------------------------------

import qualified Data.Map as M

import Data.Subtyping
import Data.TypeInt
import Control.Functor

-------------------------------- Interface

-- | Identifiers indexed by @k@. @(Id k)@ can be seen as a set of identifiers. 
--
-- The possible identifier indexes form a recursive set. An identifier index is either
--
-- * an uninstantiated type variable (inside invocations of 'runICC' and 'runICCS'), or
--
-- * @(a :|: b)@, where @a@ and @b@ are identifier indexes.

newtype Id k 
    = Id IdCore     deriving (Eq)

data IdCore 
    = I Integer
    | L IdCore
    | R IdCore
        deriving (Eq, Ord)


instance Incl Id where

    left  (Id k) = Id (L k)
    right (Id k) = Id (R k)


-- | Equality check of identifiers.
-- The first parameter has a role only in the other implementations.

equalBy :: Map i k a -> Id k -> Id k -> Bool
equalBy _ a b = a == b


-- | Finite maps from keys @('Id' k)@ to values @a@.
-- The first parameter has a role only in the other implementations.

newtype Map i k a 
    = Map (M.Map IdCore a)

instance Functor (Map i k)  where 
    fmap  f (Map m) = Map $ fmap f m

-- | Finite sets of @('Id' k)@ values.
-- The first parameter has a role only in the other implementations.

type Set i k 
    = Map i k ()


-- | Insert a new key and value in the map. If the key is already present in the map, the associated value is replaced with the supplied value.
insert :: Id k -> a -> Map i k a -> Map i k a
insert (Id k) a (Map m) = Map $ M.insert k a m

-- | Delete a key and its value from the map. When the key is not a member of the map, the original map is returned.
delete :: Id k -> Map i k a -> Map i k a
delete (Id k)   (Map m) = Map $ M.delete k m

-- | Look up the value at a key in the map.
lookUp :: Id k -> Map i k a -> Maybe a
lookUp (Id k)   (Map m) = M.lookup k m

member :: Id k -> Map i k a -> Bool
member (Id k)   (Map m) = M.member k m


-- It is actually safe in this implementation, but does nothing.

unsafeInsert :: I i => Id k -> a -> Map i k a -> ()
unsafeInsert _ _ _ = ()



-- | Union of two maps.

infixr 2 `union`

union :: Map i k1 a -> Map i k2 a -> Map i (k1 :|: k2) a
union  (Map m)  (Map m') = Map $ M.union (M.mapKeys L m) (M.mapKeys R m')

-- | Unsafe equality coercion of maps.
--
-- The two maps are equal, so every link to the first map could be safely replaced by a link to the second map.
-- It is actually safe in this implementation.

unsafeEquivalent :: Map i k a -> Map i k a -> Map i k a
unsafeEquivalent _ m = m



-- | Helps to store a range of sets numbered from 0 to @i@-1.
-- For example, @(Sets I3 k)@ is similar to @(Set I2 k, Set I1 k, Set I0 k)@.

infixr 2 `PlusSet`

data Sets i k where

    NoSets  :: Sets Zero k
    PlusSet :: Set i k -> Sets i k -> Sets (Succ i) k

-- | Helps to store a range of maps numbered from 0 to @i@-1.
-- For example, @(Maps0 I3 k)@ is similar to @(forall a . Map I2 k a, forall a . Map I1 k a, forall a . Map I0 k a)@.

infixr 2 `PlusMap`

data Maps i k where

    NoMaps  :: Maps Zero k
    PlusMap :: (forall a . Map (Succ i) k a) -> Maps i k -> Maps (Succ i) k

-- | Identifier-consuming computation. @i@ is a type-level integer.
-- A computation of type @(ICC i k a)@ 
-- gets @i@ maps numbered from 0 to @i@-1, an infinite list of different identifiers, 
-- and returns a value of type @a@. 

type ICC i k a
    =  Maps i k
    -> (forall x . Map I0 k x)
    -> [Id k]           
    -> a

-- | Return the value computed by an identifier-consuming computation. 
-- @forall k@ ensures that the identifiers indexed by @k@ are inaccessible to the rest of the program. 

runICC :: I i => (forall k . ICC i k a) -> a
runICC f = f maps1 (Map M.empty) [Id (I n) | n<-[1..]]


-- | Identifier-consuming computation with sets. @i@ is a type-level integer.
-- A computation of type @(ICCS i k a)@ 
-- gets 32 sets numbered from 0 to 31, @i@ maps numbered from 1 to @i@, an infinite list of different identifiers, 
-- and returns a value of type @a@. 

type ICCS i k a
    =  Maps i k  
    -> Sets I32 k
    -> [Id k]
    -> a

-- | Return the value computed by an identifier-consuming computation with sets. 
-- @forall k@ ensures that the identifiers indexed by @k@ are inaccessible to the rest of the program. 

runICCS :: I i => (forall k . ICCS i k a) -> a
runICCS f = f maps1 sets [Id (I n) | n<-[1..]]



maps1 :: forall i k. I i => Maps i (Id k)
maps1 = induction'' NoMaps (\x-> Map M.empty `PlusMap` x)

sets :: forall i k. I i => Sets i (Id k)
sets = induction'' NoSets (\x -> Map M.empty `PlusSet` x)



