{-# LANGUAGE RecordWildCards #-}

-- | Exporting parameters is a bit more involved as we need the ability to
-- export into a database format as well as linearize to list form.

module BioInf.Params.Export where

import Data.PrimitiveArray as PA
import Data.PrimitiveArray.Ix as PA

import BioInf.Params



-- | Just a long list of doubles.

toList :: Params -> [Double]
toList Params{..} = concat
  [ PA.toList hairpinLength
  , PA.toList hairpinClose
  , PA.toList stem
  , PA.toList stemTriplet
  , PA.toList interiorLength
  , PA.toList interiorAsym
  , PA.toList interiorClose
  , PA.toList bulgeLength
  , PA.toList bulgeTriplet
  , PA.toList bulgeClose
  , PA.toList mbClose
  , [multiBranched, multiHelix, multiUnpaired]
  , PA.toList pairDistance
  , [interMolInit]
  ]
