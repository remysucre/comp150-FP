{-# LANGUAGE NoBangPatterns #-}

  --
-----------------------------------------------------------------------------
-- | Linearly usable maps and sets on identifiers
-----------------------------------------------------------------------------

module Data.IdMap 
    ( module Data.IdMap.Core

    , inserts
    , (!)

    , setInsert
    , setInserts
    ) where

------------------------------------

import Data.IdMap.Core

import Data.List (foldl')

------------------------------------

infixl 8 !      -- better to be weaker than (~>)

(!) :: I i => Map i k a -> Id k -> a
m ! i = maybe (error "Data.IdMap.!") id (lookUp i m)

inserts :: I i => Map i k a -> [(Id k, a)] -> Map i k a
inserts = foldl' (\m (i,x) -> insert i x m)


-- | /O(1)/. Insert a new key in the set. If the key is already in the set, the original set is returned.
--
-- After insertion, the original set may not be used.

setInsert   :: I i => Id k -> Set i k -> Set i k
setInsert k = insert k ()

setInserts :: I i => Set i k -> [Id k] -> Set i k
setInserts = foldl' (flip setInsert)


