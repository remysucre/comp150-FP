{-# LANGUAGE RecordWildCards #-}

module BioInf.RNAwolf.Hairpin where

import qualified Data.Vector.Unboxed as VU

import Biobase.Primary
import Biobase.Secondary
import Data.PrimitiveArray
import Data.PrimitiveArray.Ix

import BioInf.Params
import BioInf.RNAwolf.Types

-- | A hairpin is a number of 0 or more unpaired nucleotides, enclosed by the
-- nucleotides (i,j) which pair.
--
-- TODO should we allow hairpins with no unpaired nucleotides in the pin? They
-- do occur, but only under special circumstances which we should model
-- differently...
--
-- TODO re-allow IMI

fHairpin :: [Int] -> BaseF (ExtFeatures (VU.Vector (ExtPairIdx,Double)))
fHairpin imi Params{..} inp i j ct eI eJ
--  | checkIMI imi    = VU.singleton (k,interMolInit)
  | j-i<3           = VU.empty
  | j-i-1>maxLength = VU.empty
  | otherwise       = VU.singleton (k,v)
  where
    k = ((i,j),(ct,eI,eJ))
    v = 0
      + hairpinLength ! (j-i-1)
      + hairpinClose  ! (((inp VU.! i,inp VU.! j),(ct,eI,eJ)),inp VU.! (i+1), inp VU.! (j-1))
    checkIMI [] = False
    checkIMI (x:xs) = i<x && j>x || checkIMI xs
--      + pairDistance  ! (j-i-1)
{-# INLINE fHairpin #-}

-- | Backtracking hairpins.

btHairpin
  :: Params
  -> Primary
  -> EStem
  -> ExtBT
btHairpin ps inp (EStem eStem) i j ct eI eJ d =
  [ ([ij],d')
  | i>=0,j-i>=3,j<=n
  , (_,enext) <- VU.toList $ fHairpin imi ps inp i j ct eI eJ
  , let d' = newD d ehere enext
  , testD d'
  ] where
      ij = ((i,j),(ct,eI,eJ))
      ehere = eStem!ij
      n = VU.length inp -1
      imi = map fst . filter ((==nIMI).snd) $ zip [0..] (VU.toList inp)
