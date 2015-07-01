
-- | Applies folding constraints to extended stem calculations.

module BioInf.RNAwolf.Constraint where

import Data.PrimitiveArray

import Biobase.Secondary
import Biobase.Secondary.Constraint



-- | Applies a constraint bonus/malus in the backtracking phase

applyConstraint :: PairIdx -> PrimArray (Int,Int) Double -> Double -> Double
applyConstraint (i,j) cst score = score + cst!(i,j)



