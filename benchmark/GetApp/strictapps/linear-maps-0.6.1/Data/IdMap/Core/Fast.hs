{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, RankNTypes, TypeOperators, GADTs, BangPatterns, EmptyDataDecls #-}

module Data.IdMap.Core.Fast
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

    -- * Unsafe operations
    , unsafeInsert
    , unsafeEquivalent

    -- * Range of sets and maps
    , Sets (PlusSet)
    , Maps (PlusMap)

    -- * Creation of sets, maps and identifiers
    , ICC,  runICC
    , ICCS, runICCS

    -- * For internal use
    , Maplike, MaplikeClass
    ) where

------------------------------------

#ifdef __CHECK__
import Data.Control.Kvantum
#else
import Data.Control.Kvantum.Void
#endif

import Data.Array.Simple
import Data.Bits (setBit, clearBit, testBit)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Base (Any)

import Data.Subtyping
import Data.TypeInt
import Control.Functor

import Unsafe.Coerce (unsafeCoerce)

-------------------------------- Interface

-- | Identifiers indexed by @k@. @(Id k)@ can be seen as a set of identifiers. 
--
-- The possible identifier indexes form a recursive set. An identifier index is either
--
-- * an uninstantiated type variable (inside invocations of 'runICC' and 'runICCS'), or
--
-- * @(a :|: b)@, where @a@ and @b@ are identifier indexes.

newtype Id k 
    = Id (Array (Maybe Any))

instance Incl Id where
    left = unsafeCoerce
    right = unsafeCoerce


-- | Equality check of identifiers.
-- The first map parameter is the witness that the identifiers are sane.
--
-- The first parameter prevents identifiers of type @'Id' (a :|: a)@ which could cause strange runtime behaviour. 
-- For example, @('left' x == 'right' x)@ should be @False@ in theory, but during runtime @('left' x)@ and @('right' x)@ are exactly the same identifiers.

equalBy :: Maplike i k a -> Id k -> Id k -> Bool
equalBy !_ a b = a == b

instance Eq (Id a) where

    Id a == Id b = a == b

-- | Family of finite maps from keys @('Id' k)@ to values @a@.
-- For efficiency reasons, use only with concrete type integers:
--
-- > Map I0 k a
-- > Map I1 k a
-- > Map I2 k a
-- > ...

type Map i k a  
    = Maplike (M i) k a

data M i

newtype Maplike i k a 
    = Maplike K


-- | /O(1)/. Insert a new key and value in the map. If the key is already present in the map, the associated value is replaced with the supplied value.
--
-- After insertion, the original map may not be used.

{-# SPECIALIZE insert :: Id k -> a -> Map I0 k a -> Map I0 k a #-}
{-# SPECIALIZE insert :: Id k -> a -> Map I1 k a -> Map I1 k a #-}
{-# SPECIALIZE insert :: Id k -> a -> Map I2 k a -> Map I2 k a #-}
insert :: forall i k a. MaplikeClass i a => Id k -> a -> Maplike i k a -> Maplike i k a
insert !(Id a) x (Maplike k) = unsafePerformIO $ do
    k' <- renew "insert" k
    set (undefined :: i) (Just x) a
    return $ Maplike k'


-- | /O(1)/. Delete a key and its value from the map. When the key is not a member of the map, the original map is returned.
--
-- After deletion, the original map may not be used.

delete :: forall i k a. MaplikeClass i a => Id k -> Maplike i k a -> Maplike i k a

-- | /O(1)/. Look up the value at a key in the map.

lookUp :: forall i k a. MaplikeClass i a => Id k -> Maplike i k a -> Maybe a
lookUp {-!-}(Id a) (Maplike k) = unsafePerformIO $ do       
    hit k
    x <- get (undefined :: i) a
    return x


member :: MaplikeClass i a => Id k -> Maplike i k a -> Bool
member i m = case lookUp i m of
    Just _  -> True
    _       -> False


unsafeInsert :: forall i k a. MaplikeClass i a => Id k -> a -> Maplike i k a -> ()
unsafeInsert !(Id a) x !_ = unsafePerformIO $ do
    set (undefined :: i) (Just x) a
    return ()




-- | /O(0)/. Union of two maps.
--
-- Linearity constraints:
--
-- * After union, the component maps /may/ also be used. 
--
-- * After insertion into either components, the union map may not be used.
--
-- * After insertion into the union map, the components may not be used.

infixr 2 `union`

union :: Maplike i k1 a -> Maplike i k2 a -> Maplike i (k1 :|: k2) a

-- | Unsafe equality coercion of maps.
--
-- The two maps are equal, so every link to the first map could be safely replaced by a link to the second map.

unsafeEquivalent :: Maplike i k a -> Maplike i k a -> Maplike i k a


-- | Family of finite sets of keys @('Id' k)@.
-- For efficiency reasons, use only with concrete type integers:
--
-- > Set I0 k
-- > Set I1 k
-- > Set I2 k
-- > ...

type Set i k 
    = Maplike (S Zero i) k ()

data S i j



-- | Helps to store a range of sets numbered from 0 to @i@-1.
-- For example, @(Sets I3 k)@ is similar to @(Set I2 k, Set I1 k, Set I0 k)@.

infixr 2 `PlusSet`

data Sets i k where

    PlusSet :: Set i k -> Sets i k -> Sets (Succ i) k


-- | Helps to store a range of maps numbered from 1 to @i@.
-- For example, @(Maps1 I3 k)@ is similar to @(forall a . Map I3 k a, forall a . Map I2 k a, forall a . Map I1 k a)@.

infixr 2 `PlusMap`

data Maps i k where

    PlusMap :: (forall a . Map (Succ i) k a) -> Maps i k -> Maps (Succ i) k

-- | Identifier-consuming computation. @i@ is a type-level integer.
-- A computation of type @(ICC i k a)@ 
-- gets @i@ maps numbered from 0 to @i@-1, an infinite list of different identifiers, 
-- and returns a value of type @a@. 

type ICC i k a
    =  Maps i k
    -> (forall b . Map Zero k b)
    -> [Id k]           
    -> a

-- | Return the value computed by an identifier-consuming computation. 
-- @forall k@ ensures that the identifiers indexed by @k@ are inaccessible to the rest of the program. 

runICC :: I i => (forall k . ICC i k a) -> a


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




newId :: Int -> IO (Id k)
newId n = fmap Id $ newArray (n + 1) Nothing

newIdS :: Int -> IO (Id k)
newIdS n = fmap Id $ do
    a <- newArray (n+1) Nothing
    writeArray a 0 $ unsafeCoerce (0 :: Int)
    return a

---------------------------------------------

---------------------------------------------

class MaplikeClass i x where

    set :: i -> Maybe x -> Array (Maybe Any) -> IO ()

    get :: i -> Array (Maybe Any) -> IO (Maybe x)


instance I i => MaplikeClass (M i) a where

    {-# SPECIALIZE instance MaplikeClass (M I0) a #-}
    {-# SPECIALIZE instance MaplikeClass (M I1) a #-}
    {-# SPECIALIZE instance MaplikeClass (M I2) a #-}

    set m x a = writeArray a (ind m) $ unsafeCoerce x

    get m a = fmap unsafeCoerce $ readArray a (ind m)


instance (I i, I j) => MaplikeClass (S j i) () where

    set m (Just _) a = do
        z <- readArray' m a
        writeArray' m a $ z `setBit` indS m

    set m Nothing a = do
        z <- readArray' m a
        writeArray' m a $ z `clearBit` indS m

    get m a = do
        z <- readArray' m a
        return $ if z `testBit` indS m then Just () else Nothing



delete !(Id a) (Maplike k) = unsafePerformIO $ do
    k' <- renew "delete" k
    set (undefined :: i) (Nothing :: Maybe a) a
    return $ Maplike k'


-----------

ind :: forall i. I i => M i -> Int
ind _ = num (undefined :: i)

--------

indS :: forall i j. I i => S j i -> Int
indS _ = num (undefined :: i)


readArray' :: forall i j. I i => S i j -> Array (Maybe Any) -> IO Int
readArray' _ a = fmap unsafeCoerce $ readArray a $ num (undefined :: i)

writeArray' :: forall i j. I i => S i j -> Array (Maybe Any) -> Int -> IO ()
writeArray' _ a i = writeArray a (num (undefined :: i)) $ unsafeCoerce i
 
----------




union (Maplike k1) (Maplike k2) = unsafePerformIO $ do
    k <- join k1 k2
    return $ Maplike k




instance Functor (Maplike i k)  where 
    fmap  _ _ = error "fmap on Map"

instance Functor2 (Maplike i)   where 
    fmap2 _ _ = error "fmap2 on Map"


unsafeEquivalent !_ b = b


--------------------------------

runICC = runICC'

runICC' :: forall i a . I i =>  (forall k . ICC i k a) -> a

runICCS = runICCS'

runICCS' :: forall i a . I i =>  (forall k . ICCS i k a) -> a

#ifdef __CHECK__
runICC' f = f (maps_ f) (map0_ f) $ unsafeRepeat (newId (num (undefined :: i))) f

runICCS' f = f (maps_ f) (sets_ f) $ unsafeRepeat (newIdS (num (undefined :: i))) f
#else
runICC' f = f maps map0 $ unsafeRepeat (newId (num (undefined :: i))) f

runICCS' f = f maps sets $ unsafeRepeat (newIdS (num (undefined :: i))) f
#endif


map0 :: Map Zero k a
map0 = Maplike kk

maps :: Maps i k
maps = unsafeCoerce (Maplike kk `PlusMap` maps)

sets :: Sets i k
sets = unsafeCoerce (Maplike kk `PlusSet` sets)

kk :: K
kk = unsafePerformIO create


map0_ :: x -> Map I0 k a
map0_ a = unsafePerformIO $ do
    k <- create
    return $ unsafeCoerce (do_nothing a `seq` Maplike k)

maps_ :: a -> Maps i k
maps_ a = unsafePerformIO $ do
    k <- create
    return $ unsafeCoerce (Maplike k `PlusMap` maps_ (do_nothing a))

sets_ :: a -> Sets i k
sets_ a = unsafePerformIO $ do
    k <- create
    return $ unsafeCoerce (Maplike k `PlusSet` sets_ (do_nothing a))


unsafeRepeat :: IO x -> a -> [x]
unsafeRepeat f g = unsafePerformIO $ do
    i <- f
    return (i: unsafeRepeat f (do_nothing g))

{-# NOINLINE do_nothing #-}
do_nothing :: a -> a
do_nothing i = i


