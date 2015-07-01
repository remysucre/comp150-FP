{-# LANGUAGE RecordWildCards #-}

-- | External loops are complete substructures, of which zero to many sit on
-- the chain of nucleotides.

module BioInf.RNAwolf.Extern where

import qualified Data.Vector.Unboxed as VU

import Biobase.Primary
import Biobase.Secondary
import Data.PrimitiveArray
import Data.PrimitiveArray.Ix

import BioInf.Params
import BioInf.RNAwolf.Types



-- * Unpaired left nucleotide

-- | An external loop with an unpaired nucleotide to the left

fLeftUnpaired :: BaseF (NExtn -> Features (VU.Vector (PairIdx,Double)))
fLeftUnpaired Params{..} inp (NExtn nExtn) i j
  | i<0 || j>n || i>=j = error $ "Extern.fLeftUnpaired: " ++ show (i,j)
  | otherwise = VU.singleton ( (i+1,j)
                             , nExtn ! (i+1,j)
                             )
  where
    n = VU.length inp -1
{-# INLINE fLeftUnpaired #-}

-- | Backtracking a structure with an unpaired nucleotide to the left.
--
-- FIXME In btLeftUnpaired, allow only non-empty structures on the right. It
-- would be nice to make the recursion scheme take care of that.

btLeftUnpaired
  :: Params
  -> Primary
  -> NExtn
  -> NBT
  -> NBT
btLeftUnpaired ps inp nExtn btE i j d =
  [ (x,z)
  | i>=0,i<j,j<=n
  , ((k,l),enext) <- VU.toList $ fLeftUnpaired ps inp nExtn i j
  , let d' = newD d ehere enext
  , testD d'
  , (x,z) <- btE k l d'
  , testD z
  , not $ null x -- FIXME ? we only allow left-unpaired "structures" to the left of a real structure
  ] where
      ehere = unNExtn nExtn !(i,j)
      n = VU.length inp -1



-- * exactly one stem with indices (i,k), i<k<=j

-- | Energy for exactly one stem at (i,k)

fStem :: BaseF (NStem -> Features (VU.Vector (PairIdx,Double)))
fStem Params{..} inp (NStem nStem) i j
  | i<0 || j>n || i>=j = error $ "Extern.fStem: " ++ show (i,j)
  | otherwise = VU.map f $ VU.enumFromN (i+1) (j-i)
  where
    f k = ( (i,k)
          , nStem !(i,k)
          )
    n = VU.length inp -1
{-# INLINE fStem #-}

-- | Backtrack one stem with right index k.

btStem
  :: Params
  -> Primary
  -> NExtn
  -> NStem
  -> NBT
  -> NBT
btStem ps inp nExtn nStem btNStem i j d =
  [ (x,z)
  | i>=0,i<j,j<=n
  , ((_,k),enext) <- VU.toList $ fStem ps inp nStem i j
  , let d' = newD d ehere enext
  , testD d'
  , (x,z) <- btNStem i k d'
  , testD z
  ] where
      ehere = unNExtn nExtn !(i,j)
      n = VU.length inp -1



-- * The neutral element for energy. This is an unpaired stretch between (i,j)

-- | This one is important as otherwise, some stretches of nucleotides will
-- always have to be paired. (Obviously, I forgot to add this one for a
-- time...)

fOne :: BaseF (Features (VU.Vector (PairIdx,Double)))
fOne Params{..} inp i j
  | i<0 || j>n || i>j = error $ "Extern.fOne: " ++ show (i,j)
  | otherwise = VU.singleton ( (i,j)
                             , 0
                             )
  where
    n = VU.length inp -1
{-# INLINE fOne #-}

btOne
  :: Params
  -> Primary
  -> NExtn
  -> NBT
btOne ps inp nExtn i j d =
  [ (x,z)
  | i>=0,i<=j,j<=n
  , (_,enext) <- VU.toList $ fOne ps inp i j
  , let d' = newD d ehere enext
  , testD d'
  , let x = []
  , let z = d'
  , testD z
  ] where
      ehere = unNExtn nExtn !(i,j)
      n = VU.length inp -1



-- * Two or more stems in the external structure

-- | External structures with more than one stem have a NStem on the left and
-- an external NExtn structure on the right.

fStems :: BaseF (NStem -> NExtn -> Features (VU.Vector (Int,Double)))
fStems Params{..} inp (NStem nStem) (NExtn nExtn) i j
  | i<0 || j>n || i>j = error $ "Extern.fStems: " ++ show (i,j)
  | otherwise = VU.map f $ VU.enumFromN (i+1) (j-i-1)
  where
    f k = ( k
          , nStem !(i,k) + nExtn !(k+1,j)
          )
    n = VU.length inp -1
{-# INLINE fStems #-}

-- | Backtracking of an external structure with more than one stem

btStems
  :: Params
  -> Primary
  -> NStem
  -> NExtn
  -> NBT
  -> NBT
  -> NBT
btStems ps inp nStem nExtn btNS btE i j d =
  [ (x++y,z)
  | i>=0,i<j,j<=n
  , (k,enext) <- VU.toList $ fStems ps inp nStem nExtn i j
  , let d' = newD d ehere enext
  , testD d'
  , (x,z') <- btNS i k d'
  , testD z'
  , (y,z) <- btE (k+1) j z'
  , testD z
  ] where
      ehere = unNExtn nExtn !(i,j)
      n = VU.length inp -1
