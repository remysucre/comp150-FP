{-# LANGUAGE TupleSections #-}

-- | RNA-folding parameter space.
--
-- TODO find better names for types, functions, and minima/maxima.

module BioInf.Params where

import Data.PrimitiveArray
import Data.PrimitiveArray.Ix

import Biobase.Primary
import Biobase.Secondary



-- | A (very) rich set of paramters.
--
-- TODO 1xn interior loops should be tested (how often do they occur?)
--
-- TODO external loop

data Params = Params
  { hairpinLength   :: PaLength
  , hairpinClose    :: PaExtPairNN
  , stem            :: Pa2ExtPairs
  , stemTriplet     :: Pa2ExtPairs
  , interiorLength  :: PaLength
  , interiorAsym    :: PaLength
  , interiorClose   :: PaExtPairNN
  , bulgeLength     :: PaLength
  , bulgeTriplet    :: Pa2ExtPairs
  , bulgeClose      :: PaExtPair
  , mbClose         :: PaExtPairNN
  , multiBranched   :: Double
  , multiHelix      :: Double
  , multiUnpaired   :: Double
  , pairDistance    :: PaDistance
  , interMolInit    :: Double
  } deriving (Read,Show)

maxLength = 1000 :: Int
maxDistance = 1000 :: Int
minExtPair = ((nN,nN),(cis,wc,wc))
maxExtPair = ((nT,nT),(trans,hoogsteen,hoogsteen))
min2ExtPairs = (minExtPair,minExtPair)
max2ExtPairs = (maxExtPair,maxExtPair)
minExtPairNN = (minExtPair,nN,nN)
maxExtPairNN = (maxExtPair,nT,nT)
minTriplet = min2ExtPairs
maxTriplet = max2ExtPairs

-- | A parameter set with all values set to zero.

zeroParams = Params
  { hairpinLength   = zeroLength
  , hairpinClose    = zeroExtPairNN
  , stem            = zero2ExtPairs
  , stemTriplet     = zeroTriplet
  , interiorLength  = zeroLength
  , interiorAsym    = zeroLength
  , interiorClose   = zeroExtPairNN
  , bulgeLength     = zeroLength
  , bulgeTriplet    = zeroTriplet
  , bulgeClose      = zeroExtPair
  , mbClose         = zeroExtPairNN
  , multiBranched   = 0
  , multiHelix      = 0
  , multiUnpaired   = 0
  , pairDistance    = zeroDistance
  , interMolInit    = 0
  }

zeroLength = fromAssocs 0 maxLength 0 []
zeroDistance = fromAssocs 0 maxDistance 0 []
zeroExtPair = fromAssocs minExtPair maxExtPair 0 []
zero2ExtPairs = fromAssocs min2ExtPairs max2ExtPairs 0 []
zeroExtPairNN = fromAssocs minExtPairNN maxExtPairNN 0 []
zeroTriplet = fromAssocs minTriplet maxTriplet 0 []



-- ** types

-- | An array which encodes "length" information

type PaLength = PrimArray Int Double

-- | This is an experimental annotation for long-distance interactions

type PaDistance = PrimArray Int Double

-- | An array holding information for one extended pair.

type PaExtPair   = PrimArray ExtPair Double

-- | An array holding information for two extended pairs, e.g. stems.

type Pa2ExtPairs = PrimArray (ExtPair,ExtPair) Double

-- | An array holding information for one extended pair and two unpaired
-- nucleotides, closes a loop.

type PaExtPairNN = PrimArray (ExtPair,Nuc,Nuc) Double -- pair and two unpaired nucleotides, for hairpins, etc

