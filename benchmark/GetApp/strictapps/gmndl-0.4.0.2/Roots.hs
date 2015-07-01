{-# LANGUAGE BangPatterns, Rank2Types, ScopedTypeVariables, FlexibleContexts, NoMonomorphismRestriction #-}

{-

    gmndl -- Mandelbrot Set explorer
    Copyright (C) 2010,2011,2014  Claude Heiland-Allen <claude@mathr.co.uk>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

-}

module Roots (root2, root4, FF, lift) where

import Prelude hiding (zipWith)
import Data.Maybe (fromJust)
import Data.Functor ((<$>))
import Data.Vec (toList, solve, fromList, matFromLists, zipWith, Vec2, Mat22, Vec4, Mat44, NearZero(nearZero))
import Data.Reflection (Reifies)
import Numeric.AD (jacobian', auto)
import Numeric.AD.Mode.Reverse (Reverse)
import Numeric.AD.Internal.Reverse (Tape)
import Numeric.QD (QuadDouble())

lift = auto
type FF f g a = forall s. Reifies s Tape => f (Reverse s a) -> g (Reverse s a)

root2' :: forall r . (Fractional r, NearZero r, Ord r) => r -> FF [] [] r -> Vec2 r -> Vec2 r
root2' eps f !x = go x
  where
    jf = jacobian' f
    go x0 = 
      let (ys, js) = unzip $ jf (toList x0)
          y = fromList (negate <$> ys) :: Vec2 r
          j = matFromLists js :: Mat22 r
          dx = fromJust $ solve j y
          x1 = zipWith (+) x0 dx
      in  if all (not . (> eps)) (abs <$> ys)
            then  x0
            else  if all (not . (> eps)) (abs <$> toList dx)
                    then x1
                    else go x1

root2 :: (Fractional r, NearZero r, Ord r) => r -> FF [] [] r -> [r] -> [r]
root2 eps f = toList . root2' eps f . fromList

root4' :: forall r . (Fractional r, NearZero r, Ord r) => r -> FF [] [] r -> Vec4 r -> Vec4 r
root4' eps f !x = go x
  where
    jf = jacobian' f
    go x0 = 
      let (ys, js) = unzip $ jf (toList x0)
          y = fromList (negate <$> ys) :: Vec4 r
          j = matFromLists js :: Mat44 r
          dx = fromJust $ solve j y
          x1 = zipWith (+) x0 dx
      in  if all (not . (> eps)) (abs <$> ys)
            then  x0
            else  if all (not . (> eps)) (abs <$> toList dx)
                    then x1
                    else go x1

root4 :: (Fractional r, NearZero r, Ord r) => r -> FF [] [] r -> [r] -> [r]
root4 eps f = toList . root4' eps f . fromList

instance NearZero QuadDouble where
  nearZero x = not (abs x > 1e-60) -- NearZero Double has 1e-14
