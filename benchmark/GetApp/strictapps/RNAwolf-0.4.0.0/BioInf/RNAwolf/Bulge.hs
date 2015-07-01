{-# LANGUAGE RecordWildCards #-}

module BioInf.RNAwolf.Bulge where

import qualified Data.Vector.Unboxed as VU

import Biobase.Primary
import Biobase.Secondary
import Data.PrimitiveArray
import Data.PrimitiveArray.Ix

import BioInf.Params
import BioInf.RNAwolf.Types



-- * Outer part of a bulge

-- | The outer closing pair of a bulge loop (one unpaired region).

fBulgeOuter :: BaseF (NBulgLoop -> ExtFeatures (VU.Vector (PairIdx,Double)))
fBulgeOuter Params{..} inp (NBulgLoop nBulgLoop) i j ct eI eJ
  | i<0 || j>n = error $ "fBulgeOuter: " ++ show (i,j)
  | otherwise = VU.singleton ( (i,j)
                             , nBulgLoop ! (i,j)
                             + bulgeClose ! ((nI,nJ),(ct,eI,eJ))
                             )
  where
    nI = inp VU.! i
    nJ = inp VU.! j
    n = VU.length inp -1
{-# INLINE fBulgeOuter #-}

-- | Outer part of a normal bulge

btBulgeOuter
  :: Params
  -> Primary
  -> EStem
  -> NBulgLoop
  -> NBT
  -> ExtBT
btBulgeOuter ps inp (EStem eStem) nBulgLoop btBULoop i j ct eI eJ d =
  [ (ij:x,z)
  | i>=0,i<j,j<=n
  , ((k,l),enext) <- VU.toList $ fBulgeOuter ps inp nBulgLoop i j ct eI eJ
  , let d' = newD d ehere enext
  , testD d'
  , (x,z) <- btBULoop k l d'
  ] where
      ij = ((i,j),(ct,eI,eJ))
      ehere = eStem!ij
      n = VU.length inp -1



-- * Loop part of a bulge

-- | The loop-part of bulges. Increases speed by 2x

fBulgeLoop :: BaseF (NBulg -> Features (VU.Vector (PairIdx,Double)))
fBulgeLoop Params{..} inp (NBulg nBulg) i j
  | j-i<=6 = VU.empty
  | otherwise = VU.map f kls
  where
    f (k,l) =
      ( (k,l)
      , nBulg ! (k,l)
      + bulgeLength ! (max (k-i-1) (j-l-1))
      )
    kls = fBulgeLoopIndices i j

-- | Index generator for bulged loops

fBulgeLoopIndices i j = ks VU.++ ls where
    ks = VU.fromList [ (k,l)
                     | k <- takeWhile (\k -> k-i-1<=maxLength) [i+2 .. j-4], let l = j-1
                     ]
    ls = VU.fromList [ (k,l)
                     | let k = i+1, l <- takeWhile (\l -> j-l-1<=maxLength) [j-2,j-3 .. i+4]
                     ]
{-# INLINE fBulgeLoop #-}

-- | Backtrack the bulge loop part.

btBulgeLoop
  :: Params
  -> Primary
  -> NBulgLoop
  -> NBulg
  -> NBT
  -> NBT
btBulgeLoop ps inp (NBulgLoop nBulgLoop) nBulg btBU i j d =
  [ (x,z)
  | i>=0,i<j,j<=n
  , ((k,l),enext) <- VU.toList $ fBulgeLoop ps inp nBulg i j
  , let d' = newD d ehere enext
  , testD d'
  , (x,z) <- btBU k l d'
  , testD z
  ] where
      ehere = nBulgLoop!(i,j)
      n = VU.length inp -1


-- * Inner part of a bulge

-- | Inner part of a bulge to speed up bulge calculations

fBulgeInner :: BaseF (EStem -> Features (VU.Vector (ExtPairIdx,Double)))
fBulgeInner Params{..} inp (EStem eStem) i j
  | j-i<2 = VU.empty
  | otherwise = VU.map f kls
  where
    f ext =
      ( ij
      , eStem ! ij
--      + bulgeClose ! ((nJ,nI),ext)
      ) where nI = inp VU.! i
              nJ = inp VU.! j
              ij = ((i,j),ext)
    kls = VU.fromList [ (ct,eI,eJ)
                      | eI<-wsh, eJ<-wsh, ct<-citr
                      ]
{-# INLINE fBulgeInner #-}

-- | Backtrack the inner part of a bulge

btBulgeInner
  :: Params
  -> Primary
  -> NBulg
  -> EStem
  -> ExtBT
  -> NBT
btBulgeInner ps inp (NBulg nBulg) eStem btES i j d =
  [ (x,z)
  | i>=0,i<j,j<=n
  , ((_,(eI,eJ,ct)),enext) <- VU.toList $ fBulgeInner ps inp eStem i j
  , let d' = newD d ehere enext
  , testD d'
  , (x,z) <- btES i j eI eJ ct d'
  ] where
      n = VU.length inp -1
      ehere = nBulg!(i,j)
