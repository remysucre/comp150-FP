{-# LANGUAGE NoBangPatterns, CPP #-}

module Data.IdMap.Static
    ( module Data.IdMap

    , (:.)((:.))

    , insert, delete, lookUp
    , (!), member, inserts

    , setInsert
    , setInserts
    ) where

------------------------------------

import qualified Data.IdMap as I
import Data.IdMap hiding
    ( insert, delete, lookUp
    , (!), member, inserts

    , setInsert, setInserts
    )

import Data.Maybe
import Data.List (foldl')

------------------------------------

-- | Identifiers with static data.

data k :. x = !(Id k) :. !x

instance Incl2 (:.) where
    left2  (i :. x) = left  i :. x
    right2 (i :. x) = right i :. x

instance Functor ((:.) x)   where fmap f (i :. x) = i :. (f x)

---------------------------------------------

#ifdef __PURE__
lookUp :: k :. d -> Map i k a -> Maybe a

insert :: k :. d -> a -> Map i k a -> Map i k a

delete :: k :. d -> Map i k a -> Map i k a

member :: k :. d -> Map i k a -> Bool
#else
lookUp :: MaplikeClass i a => k :. d -> Maplike i k a -> Maybe a

insert :: MaplikeClass i a => k :. d -> a -> Maplike i k a -> Maplike i k a

delete :: MaplikeClass i a => k :. d -> Maplike i k a -> Maplike i k a

member :: MaplikeClass i a => k :. d -> Maplike i k a -> Bool
#endif

lookUp (a :. _) m = I.lookUp a m

insert (a :. _) x m = I.insert a x m

delete (a :. _) m = I.delete a m

member i = isJust . lookUp i

infixl 8 !      -- 9 lenne, de ~> miatt 8

(!) :: I i => Map i k a -> k :. d -> a
m ! i = maybe (error "Data.IdMap.!") id (lookUp i m)

inserts :: I i => Map i k a -> [(k :. d, a)] -> Map i k a
inserts = foldl' (\m (i,x) -> insert i x m)

setInsert :: I i => k :. d -> Set i k -> Set i k
setInsert (a :. _) m = I.insert a () m

setInserts :: I i => Set i k -> [k :. d] -> Set i k
setInserts = foldl' (flip setInsert)

