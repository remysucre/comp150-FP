
{-# LANGUAGE RecordWildCards #-}

module BioInf.RNAwolf.Stem where

import qualified Data.Vector.Unboxed as VU

import Biobase.Primary
import Biobase.Secondary
import Data.PrimitiveArray
import Data.PrimitiveArray.Ix

import BioInf.Params
import BioInf.RNAwolf.Types



-- * Collapses an extended stem into a normal stem, or allows going back from a
-- normal stem to an extended stem.

-- | A normal stem is created by taking the minimum over all possible basepairs
-- of the extended stem.

fNstem :: BaseF (EStem -> Features (VU.Vector (ExtPairIdx,Double)))
fNstem Params{..} inp (EStem eStem) i j
  | i<0 || j>n = error $ "Stem.fNstem: " ++ show (i,j)
  | j-i<=2      = VU.empty
  | otherwise   = VU.map f $ VU.fromList [ (ct,eI,eJ) | ct<-citr, eI<-wsh, eJ<-wsh ]
  where
    f ext = ( idx
            , eStem !idx
            ) where idx = ((i,j),ext)
    n = VU.length inp -1
{-# INLINE fNstem #-}

-- | Backtrack from a normal stem back into the extended stem.

btNstem
  :: Params
  -> Primary
  -> NStem
  -> EStem
  -> ExtBT
  -> NBT
btNstem ps inp nStem eStem btES i j d =
  [ (x,z)
  | i>=0,i<j,j<=n
  , ((_,(ct,eI,eJ)),enext) <- VU.toList $ fNstem ps inp eStem i j
  , let d' = newD d ehere enext
  , testD d'
  , (x,z) <- btES i j ct eI eJ d'
  , testD z
  ] where
     ehere = unNStem nStem !(i,j)
     n = VU.length inp -1



-- * Stacking with extended basepair information

-- | A stem is extended by another pair. The score contribution is dependent on
-- the previous pair. Note that for score lookup purposes, the inner pair is
-- switched.

fStem :: BaseF (EStem -> ExtFeatures (VU.Vector (ExtPairIdx,Double)))
fStem Params{..} inp (EStem eStem) i j ct eI eJ
  | j-i<3     = VU.empty
  | otherwise = VU.map f $ eKLs
  where
    f (ctKL,eK,eL) = ( ((k,l),(ctKL,eK,eL))
                     , eStem ! ((k,l),(ctKL,eK,eL))
                     + stem ! (ijExt,((inp VU.! l,inp VU.! k),(ctKL,eL,eK)))
--                     + if j-i-1<=maxDistance then pairDistance ! (j-i-1) else 0
                     )
    ijExt = ((inp VU.! i, inp VU.! j),(ct,eI,eJ))
    k = i+1
    l = j-1
    eKLs = VU.fromList [ (ctKL,eK,eL) | eK<-wsh, eL<-wsh, ctKL<-citr ]
    {-# NOINLINE eKLs #-} -- speed-up for small input sizes
{-# INLINE fStem #-}

-- | Stem backtracking.

btStem
  :: Params
  -> Primary
  -> EStem
  -> ExtBT -- recursive backtracking function
  -> ExtBT
btStem ps inp eStem btES i j ct eI eJ d =
  [ (ij:x,z)
  | i>=0,i<j,j<=n
  , (((k,l),(ctKL,eK,eL)),enext) <- VU.toList $ fStem ps inp eStem i j ct eI eJ
  , let d' = newD d ehere enext
  , testD d'
  , (x,z) <- btES k l ctKL eK eL d'
  ] where
      ij = ((i,j),(ct,eI,eJ))
      ehere = unEStem eStem !ij
      n = VU.length inp -1
