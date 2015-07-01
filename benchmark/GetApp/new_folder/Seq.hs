{-# LANGUAGE TypeFamilies, TypeSynonymInstances, TypeOperators, PackageImports, CPP #-}
module NoSlow.Backend.DPH.Prim.Seq (
  module U,
  null, imap, enumFromTo_Int, cons, head, tail, index, append, take, slice,
  prescanl',
  backpermute, update_, minIndex, maxIndex, unstablePartition,
  prescanr',
  pair, from2, fst, snd, triple, from3

#if __GLASGOW_HASKELL__ < 612
  , filter, drop, update, and
#endif
) where

import NoSlow.Util.Computation
import NoSlow.Util.Base ( Unsupported(..) )

import "dph-prim-seq" Data.Array.Parallel.Unlifted as U
import qualified Data.Array.Parallel.Unlifted.Sequential as S
import Data.Array.Parallel.Base ( (:*:)(..), fstS, sndS, uncurryS )
import qualified Prelude
import Prelude ( Num, Ord, Int, Bool, (.), flip, not )

instance DeepSeq (U.Array a)

instance (TestData a, U.Elt a) => TestData (U.Array a) where
  testData = testList

instance U.Elt a => ListLike U.Array a where
  fromList = U.fromList

null :: Elt a => Array a -> Bool
{-# INLINE null #-}
null = S.nullU

imap :: (Elt a, Elt b) => (Int -> a -> b) -> Array a -> Array b
{-# INLINE imap #-}
imap f xs = U.map (uncurryS f) (U.indexed xs)

#if __GLASGOW_HASKELL__ < 612
filter :: Elt a => (a -> Bool) -> Array a -> Array a
{-# INLINE filter #-}
filter = S.filterU
#endif

cons :: Elt a => a -> Array a -> Array a
{-# INLINE cons #-}
cons = S.consU

head :: Elt a => Array a -> a
{-# INLINE head #-}
head xs = xs !: 0

tail :: Elt a => Array a -> Array a
{-# INLINE tail #-}
tail = S.tailU

enumFromTo_Int :: Int -> Int -> Array Int
{-# INLINE enumFromTo_Int #-}
enumFromTo_Int = U.enumFromTo

prescanl' :: (Elt a, Elt b) => (a -> b -> a) -> a -> Array b -> Array a
{-# INLINE prescanl' #-}
prescanl' = S.scanlU

prescanr' :: (Elt a, Elt b) => (a -> b -> b) -> b -> Array a -> Array b
{-# INLINE prescanr' #-}
prescanr' f z xs = S.scanlU (flip f) z (S.reverseU xs)

index :: Elt a => Array a -> Int -> a
{-# INLINE index #-}
index = (!:)

append :: Elt a => Array a -> Array a -> Array a
{-# INLINE append #-}
append = (+:+)

take :: Elt a => Int -> Array a -> Array a
{-# INLINE take #-}
take = S.takeU

#if __GLASGOW_HASKELL__ < 612
drop :: Elt a => Int -> Array a -> Array a
{-# INLINE drop #-}
drop = S.dropU
#endif

slice :: Elt a => Array a -> Int -> Int -> Array a
{-# INLINE slice #-}
slice = S.sliceU

backpermute :: Elt a => Array a -> Array Int -> Array a
{-# INLINE backpermute #-}
backpermute = bpermute

update_ :: Elt a => Array a -> Array Int -> Array a -> Array a
{-# INLINE update_ #-}
#if __GLASGOW_HASKELL__ < 612
update_ xs is ys = S.updateU xs (zip is ys)
#else
update_ xs is ys = update xs (zip is ys)
#endif

#if __GLASGOW_HASKELL__ < 612
update :: Elt a => Array a -> Array (Int :*: a) -> Array a
{-# INLINE update #-}
update = S.updateU
#endif

#if __GLASGOW_HASKELL__ < 612
and :: Array Bool -> Bool
{-# INLINE and #-}
and = S.andU
#endif

minIndex :: (Ord a, Elt a) => Array a -> Int
{-# INLINE minIndex #-}
minIndex = S.minimumIndexU

maxIndex :: (Ord a, Elt a) => Array a -> Int
{-# INLINE maxIndex #-}
maxIndex = S.maximumIndexU

unstablePartition :: Elt a => (a -> Bool) -> Array a -> (Array a, Array a)
{-# INLINE unstablePartition #-}
#if __GLASGOW_HASKELL__ < 612
unstablePartition f xs = (S.filterU f xs, S.filterU (not . f) xs)
#else
unstablePartition f xs = (U.filter f xs, U.filter (not . f) xs)
#endif

pair :: a -> b -> a :*: b
{-# INLINE pair #-}
pair = (:*:)

from2 :: a :*: b -> (a,b)
{-# INLINE from2 #-}
from2 (a :*: b) = (a,b)

fst :: a :*: b -> a
{-# INLINE fst #-}
fst = fstS

snd :: a :*: b -> b
{-# INLINE snd #-}
snd = sndS

triple :: a -> b -> c -> a :*: b :*: c
{-# INLINE triple #-}
triple a b c = a :*: b :*: c

from3 :: a :*: b :*: c -> (a,b,c)
{-# INLINE from3 #-}
from3 (a :*: b :*: c) = (a,b,c)

