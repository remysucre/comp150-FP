
-- | Given a list of doubles with the /exact required length/ import into a
-- 'Params' structure.

module BioInf.Params.Import where

import Data.PrimitiveArray as PA
import Data.PrimitiveArray.Ix as PA
import Data.Ix (rangeSize)

import BioInf.Params



-- | Cast a list of values to parameters.
--
-- NOTE This operation is rather fragile if there are layout changes. Consider
-- Repr for this.
--
-- NOTE BIG FAT WARNING: BE ABSOLUTELY SURE THAT ALL IMPORTS AND EXPORTS FOLLOW
-- THIS ORDERING EXACTLY, OTHERWISE KEYS WILL BE MAPPED TO WRONG POSITIONS
-- DURING LOOKUP AND VALUES END UP SOMEWHERE ELSE.

fromList :: [Double] -> Params
fromList xs = Params
  { hairpinLength   = PA.fromList 0            maxLength    hpl
  , hairpinClose    = PA.fromList minExtPairNN maxExtPairNN hpc
  , stem            = PA.fromList min2ExtPairs max2ExtPairs sp
  , stemTriplet     = PA.fromList minTriplet   maxTriplet   tp
  , interiorLength  = PA.fromList 0            maxLength    il
  , interiorAsym    = PA.fromList 0            maxLength    ia
  , interiorClose   = PA.fromList minExtPairNN maxExtPairNN ip
  , bulgeLength     = PA.fromList 0            maxLength    bl
  , bulgeTriplet    = PA.fromList minTriplet   maxTriplet   bt
  , bulgeClose      = PA.fromList minExtPair   maxExtPair   bu
  , mbClose         = PA.fromList minExtPairNN maxExtPairNN mbc
  , multiBranched   = head mbranched
  , multiHelix      = head mhelix
  , multiUnpaired   = head munpaired
  , pairDistance    = PA.fromList 0            maxDistance  dst
  , interMolInit    = head intermol
  } where
    rsExtPair   = rangeSize (minExtPair,maxExtPair)
    rs2ExtPairs = rangeSize (min2ExtPairs,max2ExtPairs)
    rsTriplet   = rangeSize (minTriplet,maxTriplet)
    rsExtPairNN = rangeSize (minExtPairNN,maxExtPairNN)
    [hpl,hpc,sp,tp,il,ia,ip,bl,bt,bu,mbc,mbranched,mhelix,munpaired,dst,intermol] = splitXs
      [ maxLength+1   -- hairpin length
      , rsExtPairNN   -- hairpin close
      , rs2ExtPairs   -- stem pair
      , rsTriplet     -- triplet pair
      , maxLength+1   -- interior loop length
      , maxLength+1   -- interior loop asymmetry
      , rsExtPairNN   -- interior pair
      , maxLength+1   -- bulge length
      , rsTriplet     -- bulge triplet
      , rsExtPair     -- normal bulge with non-overlapping nucs
      , rsExtPairNN   -- multibranch pair
      , 1             -- multibranched score
      , 1             -- helix in multibranch score
      , 1             -- unpaired nucleotide score in multibranch
      , maxDistance+1 -- pair long distance
      , 1
      ]
      xs

-- | split up a list accordings to given lengths

splitXs :: [Int] -> [Double] -> [[Double]]
splitXs [k] xs
  | length xs == k = [xs]
  | otherwise      = error "splitXs encountered wrong key length on last element"
splitXs (k:ks) xs  = let (here,rest) = splitAt k xs in here : splitXs ks rest
