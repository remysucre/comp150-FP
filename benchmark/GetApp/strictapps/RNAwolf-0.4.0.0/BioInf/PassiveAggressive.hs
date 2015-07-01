{-# LANGUAGE RecordWildCards #-}

-- | Passive-aggressive optimization. Mainly based on:
--
-- Zakov, Shay and Goldberg, Yoav and Elhaded, Michael and Ziv-Ukelson, Michal
-- "Rich Parameterization Improves RNA Structure Prediction"
-- RECOMB 2011
--
-- and
--
-- Crammer, Koby and (et al)
-- "Online Passive-Aggressive Algorithms"
-- Journal of Machine Learning Research (2006)
--
-- TODO as always: move out of here and put in its own library

module BioInf.PassiveAggressive where

import Control.Arrow
import Control.DeepSeq
import Control.Parallel (pseq)
import Data.List as L
import Data.Map as M
import Data.Set as S
import qualified Data.Vector.Unboxed as VU
import Text.Printf

import Biobase.TrainingData
import BioInf.Keys

import qualified BioInf.Params as P
import qualified BioInf.Params.Export as P
import qualified BioInf.Params.Import as P

import Statistics.ConfusionMatrix
import Statistics.PerformanceMetrics



-- | Default implementation of P/A. We return a data structure that contains
-- all 'changes' required from this run, the 'enerDif' or energy difference
-- between the known and the predicted structure, and a structural difference
-- score. Furthermore, some errors are being reported in 'errors'.
--
-- The energy difference can be (i) in that case, a wrongly predicted structure
-- has better (lower) energy than the known one. (ii) It can be zero, then we
-- have either found a co-optimal structural or the correct structure. (iii) In
-- some cases, it can be positive, this is a formal error, but will not abort
-- the program. (The calling program may opt to abort on (not . null $ errors).
--
-- The structural difference is [0..1] with "0" for structurally identical
-- predictions and known structures and otherwise growing toward "1" for bad
-- predictions where nothing is correct.

defaultPA :: Double -> P.Params -> TrainingData -> PA
defaultPA aggressiveness params td@TrainingData{..}
  | L.null $ pOnly++kOnly = PA
      { changes = []
      , enerDif = edif
      , accMeas = struc
      , errors  = []
      }
  | struc >= 0.999 = PA
      { changes = []
      , enerDif = edif
      , accMeas = struc
      , errors  = []
      }
  | otherwise = PA
      { changes = changes
      , enerDif = edif
      , accMeas = struc
      , errors  = eError
      }
  where
    -- calculate changes
    pFeatures = featureVector primary predicted
    kFeatures = featureVector primary secondary
    pOnly = pFeatures L.\\ kFeatures
    kOnly = kFeatures L.\\ pFeatures
    numChanges = genericLength $ pOnly ++ kOnly
    changes = zip kOnly (repeat $ negate tau) ++ zip pOnly (repeat tau)
    cur = VU.fromList . P.toList $ params
    pScore = sum . L.map (cur VU.!) $ pFeatures
    kScore = sum . L.map (cur VU.!) $ kFeatures
    edif = kScore - pScore
    eError = if edif <= 0
               then []
               else ["S(known) < S(predicted)\n" ++ errorKnownTooGood td cur kFeatures pFeatures]
    struc = case fmeasure (mkConfusionMatrix td) of -- currently optimizing using F_1
            Left  _ -> 1
            Right v -> v
    -- weight calculation
    tau = min aggressiveness $ ( (min 0 $ kScore - pScore) + sqrt (1-struc)
                               ) / (numChanges ^ 2)

-- | In case that the known structure has a score 'epsilon' better than the
-- predicted, we have an error condition, as this should never be the case.

errorKnownTooGood TrainingData{..} curPs kFeatures pFeatures = z where
  z =  printf "S(known) = %7.4f, S(pred) = %7.4f, S(known) - S(pred) = %7.4f\n"
        kScore pScore (kScore - pScore)
    ++ printf "%s\n%s\n" primary (concat $ intersperse "\n" comments)
  kScore = sum . L.map (curPs VU.!) $ kFeatures
  pScore = sum . L.map (curPs VU.!) $ pFeatures

-- | Return a lot of information from each P/A call. We do not return the new
-- 'Params' anymore, only a list of changes. This allows us to do some things.
-- If the implementation of 'Params' is switched, we can update in place; or we
-- can perform calculations in parallel.

data PA = PA
  { changes :: [(Int,Double)] -- (index of change, change)
  , enerDif :: Double         -- how much pressure from a wrong energy difference
  , accMeas :: Double         -- accuracy measure
  , errors  :: [String]       -- if something strange happens
  } deriving (Show)

instance NFData PA where
  rnf PA{..} = rnf changes `pseq` rnf enerDif `pseq` rnf accMeas `pseq` rnf errors

-- * Instances

-- | Pull in the statistical interface. From the confusion matrix, we
-- automagically get everything we need.
--
-- NOTE Unfortunately, StatisticalMethods has heavy dependencies.

instance MkConfusionMatrix TrainingData where
  mkConfusionMatrix TrainingData{..} = ConfusionMatrix
    { fn = Right . fromIntegral . S.size $ k `S.difference` p
    , fp = Right . fromIntegral . S.size $ p `S.difference` k
    , tn = Right . fromIntegral $ allPs - S.size (k `S.union` p)
    , tp = Right . fromIntegral . S.size $ k `S.intersection` p
    } where
        k = S.fromList secondary
        p = S.fromList predicted
        allPs = ((length primary) * (length primary -1)) `div` 2
