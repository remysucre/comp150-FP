
-- | Concatenation of tuples. Requires MPTCs and FunDeps. 

{-# OPTIONS_GHC -cpp -pgmPcpphs -optP--cpp -optP-ansi -optP--hashes #-}
{-# LANGUAGE CPP, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

module Data.Tup.Concat where

--------------------------------------------------------------------------------

import Data.Tup.Class

import qualified Data.Tup.Tup.Lazy   as L
import qualified Data.Tup.Tup.Strict as S
import qualified Data.Tup.Vec        as V
import qualified Data.Tup.Newtype    as N

--------------------------------------------------------------------------------

class (Tup f, Tup g, Tup h) => TupConcat f g h | f g -> h where
  tupConcat :: f a -> g a -> h a
  tupConcat x y = tupFromList (tupToList x ++ tupToList y)

--------------------------------------------------------------------------------
-- Vecs

instance Tup v => TupConcat V.Empty v v where
  tupConcat V.Empty v = v

-- This seems to need UndecidableInstances?
instance (Tup u, Tup v, TupConcat u v w) => TupConcat (V.Cons u) v (V.Cons w) where
  tupConcat (V.Cons x u) v = V.Cons x (tupConcat u v)

--------------------------------------------------------------------------------

#define TUPCONCAT_L(A,B,C) \
instance TupConcat L.Tup##A L.Tup##B L.Tup##C 

#define TUPCONCAT_S(A,B,C) \
instance TupConcat S.Tup##A S.Tup##B S.Tup##C 

#define TUPCONCAT_N(A,B,C) \
instance TupConcat N.NTup##A N.NTup##B N.NTup##C 

--------------------------------------------------------------------------------
-- lazy Tups

TUPCONCAT_L(0,0,0)
TUPCONCAT_L(0,1,1)
TUPCONCAT_L(0,2,2)
TUPCONCAT_L(0,3,3)
TUPCONCAT_L(0,4,4)
TUPCONCAT_L(0,5,5)
TUPCONCAT_L(0,6,6)
TUPCONCAT_L(0,7,7)
TUPCONCAT_L(0,8,8)
TUPCONCAT_L(0,9,9)

TUPCONCAT_L(1,0,1)
TUPCONCAT_L(1,1,2)
TUPCONCAT_L(1,2,3)
TUPCONCAT_L(1,3,4)
TUPCONCAT_L(1,4,5)
TUPCONCAT_L(1,5,6)
TUPCONCAT_L(1,6,7)
TUPCONCAT_L(1,7,8)
TUPCONCAT_L(1,8,9)

TUPCONCAT_L(2,0,2)
TUPCONCAT_L(2,1,3)
TUPCONCAT_L(2,2,4)
TUPCONCAT_L(2,3,5)
TUPCONCAT_L(2,4,6)
TUPCONCAT_L(2,5,7)
TUPCONCAT_L(2,6,8)
TUPCONCAT_L(2,7,9)

TUPCONCAT_L(3,0,3)
TUPCONCAT_L(3,1,4)
TUPCONCAT_L(3,2,5)
TUPCONCAT_L(3,3,6)
TUPCONCAT_L(3,4,7)
TUPCONCAT_L(3,5,8)
TUPCONCAT_L(3,6,9)

TUPCONCAT_L(4,0,4)
TUPCONCAT_L(4,1,5)
TUPCONCAT_L(4,2,6)
TUPCONCAT_L(4,3,7)
TUPCONCAT_L(4,4,8)
TUPCONCAT_L(4,5,9)

TUPCONCAT_L(5,0,5)
TUPCONCAT_L(5,1,6)
TUPCONCAT_L(5,2,7)
TUPCONCAT_L(5,3,8)
TUPCONCAT_L(5,4,9)

TUPCONCAT_L(6,0,6)
TUPCONCAT_L(6,1,7)
TUPCONCAT_L(6,2,8)
TUPCONCAT_L(6,3,9)

TUPCONCAT_L(7,0,7)
TUPCONCAT_L(7,1,8)
TUPCONCAT_L(7,2,9)

TUPCONCAT_L(8,0,8)
TUPCONCAT_L(8,1,9)

TUPCONCAT_L(9,0,9)

--------------------------------------------------------------------------------
-- strict Tups

TUPCONCAT_S(0,0,0)
TUPCONCAT_S(0,1,1)
TUPCONCAT_S(0,2,2)
TUPCONCAT_S(0,3,3)
TUPCONCAT_S(0,4,4)
TUPCONCAT_S(0,5,5)
TUPCONCAT_S(0,6,6)
TUPCONCAT_S(0,7,7)
TUPCONCAT_S(0,8,8)
TUPCONCAT_S(0,9,9)

TUPCONCAT_S(1,0,1)
TUPCONCAT_S(1,1,2)
TUPCONCAT_S(1,2,3)
TUPCONCAT_S(1,3,4)
TUPCONCAT_S(1,4,5)
TUPCONCAT_S(1,5,6)
TUPCONCAT_S(1,6,7)
TUPCONCAT_S(1,7,8)
TUPCONCAT_S(1,8,9)

TUPCONCAT_S(2,0,2)
TUPCONCAT_S(2,1,3)
TUPCONCAT_S(2,2,4)
TUPCONCAT_S(2,3,5)
TUPCONCAT_S(2,4,6)
TUPCONCAT_S(2,5,7)
TUPCONCAT_S(2,6,8)
TUPCONCAT_S(2,7,9)

TUPCONCAT_S(3,0,3)
TUPCONCAT_S(3,1,4)
TUPCONCAT_S(3,2,5)
TUPCONCAT_S(3,3,6)
TUPCONCAT_S(3,4,7)
TUPCONCAT_S(3,5,8)
TUPCONCAT_S(3,6,9)

TUPCONCAT_S(4,0,4)
TUPCONCAT_S(4,1,5)
TUPCONCAT_S(4,2,6)
TUPCONCAT_S(4,3,7)
TUPCONCAT_S(4,4,8)
TUPCONCAT_S(4,5,9)

TUPCONCAT_S(5,0,5)
TUPCONCAT_S(5,1,6)
TUPCONCAT_S(5,2,7)
TUPCONCAT_S(5,3,8)
TUPCONCAT_S(5,4,9)

TUPCONCAT_S(6,0,6)
TUPCONCAT_S(6,1,7)
TUPCONCAT_S(6,2,8)
TUPCONCAT_S(6,3,9)

TUPCONCAT_S(7,0,7)
TUPCONCAT_S(7,1,8)
TUPCONCAT_S(7,2,9)

TUPCONCAT_S(8,0,8)
TUPCONCAT_S(8,1,9)

TUPCONCAT_S(9,0,9)

--------------------------------------------------------------------------------
-- NTups

TUPCONCAT_N(0,0,0)
TUPCONCAT_N(0,1,1)
TUPCONCAT_N(0,2,2)
TUPCONCAT_N(0,3,3)
TUPCONCAT_N(0,4,4)
TUPCONCAT_N(0,5,5)
TUPCONCAT_N(0,6,6)
TUPCONCAT_N(0,7,7)
TUPCONCAT_N(0,8,8)
TUPCONCAT_N(0,9,9)

TUPCONCAT_N(1,0,1)
TUPCONCAT_N(1,1,2)
TUPCONCAT_N(1,2,3)
TUPCONCAT_N(1,3,4)
TUPCONCAT_N(1,4,5)
TUPCONCAT_N(1,5,6)
TUPCONCAT_N(1,6,7)
TUPCONCAT_N(1,7,8)
TUPCONCAT_N(1,8,9)

TUPCONCAT_N(2,0,2)
TUPCONCAT_N(2,1,3)
TUPCONCAT_N(2,2,4)
TUPCONCAT_N(2,3,5)
TUPCONCAT_N(2,4,6)
TUPCONCAT_N(2,5,7)
TUPCONCAT_N(2,6,8)
TUPCONCAT_N(2,7,9)

TUPCONCAT_N(3,0,3)
TUPCONCAT_N(3,1,4)
TUPCONCAT_N(3,2,5)
TUPCONCAT_N(3,3,6)
TUPCONCAT_N(3,4,7)
TUPCONCAT_N(3,5,8)
TUPCONCAT_N(3,6,9)

TUPCONCAT_N(4,0,4)
TUPCONCAT_N(4,1,5)
TUPCONCAT_N(4,2,6)
TUPCONCAT_N(4,3,7)
TUPCONCAT_N(4,4,8)
TUPCONCAT_N(4,5,9)

TUPCONCAT_N(5,0,5)
TUPCONCAT_N(5,1,6)
TUPCONCAT_N(5,2,7)
TUPCONCAT_N(5,3,8)
TUPCONCAT_N(5,4,9)

TUPCONCAT_N(6,0,6)
TUPCONCAT_N(6,1,7)
TUPCONCAT_N(6,2,8)
TUPCONCAT_N(6,3,9)

TUPCONCAT_N(7,0,7)
TUPCONCAT_N(7,1,8)
TUPCONCAT_N(7,2,9)

TUPCONCAT_N(8,0,8)
TUPCONCAT_N(8,1,9)

TUPCONCAT_N(9,0,9)

--------------------------------------------------------------------------------
