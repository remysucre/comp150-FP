{-# LANGUAGE BangPatterns, FlexibleContexts #-}

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

module Complex where

import Prelude hiding (atan2)

import Foreign.C (CDouble)

-- higher precision arithmetic using libqd
import Numeric.QD.DoubleDouble (DoubleDouble(DoubleDouble))
import Numeric.QD.QuadDouble (QuadDouble(QuadDouble))
import qualified Numeric.QD.DoubleDouble as DD
import qualified Numeric.QD.QuadDouble as QD
import Numeric.AD.Mode.Reverse (Reverse)
import Data.Reflection (Reifies)
import Numeric.AD.Internal.Reverse (Tape)

-- ugly! but the default realToFrac :: (C)Double -> (C)Double is slooow
import Unsafe.Coerce (unsafeCoerce)

-- don't look! this is really really ugly, and should be benchmarked
-- to see how really necessary it is, or at least made into a type class
convert :: (Real a, Fractional b) => a -> b
convert = realToFrac
{-# NOINLINE convert #-}
convertDouble2CDouble :: Double -> CDouble
convertDouble2CDouble !x = unsafeCoerce x
convertCDouble2Double :: CDouble -> Double
convertCDouble2Double !x = unsafeCoerce x
convertDouble2DoubleDouble :: Double -> DoubleDouble
convertDouble2DoubleDouble !x = convertCDouble2DoubleDouble . convertDouble2CDouble $ x
convertCDouble2DoubleDouble :: CDouble -> DoubleDouble
convertCDouble2DoubleDouble !x = DoubleDouble x 0
convertDoubleDouble2Double :: DoubleDouble -> Double
convertDoubleDouble2Double !(DoubleDouble x _) = convertCDouble2Double x
convertDoubleDouble2CDouble :: DoubleDouble -> CDouble
convertDoubleDouble2CDouble !(DoubleDouble x _) = x
{-# RULES "convert/Double2CDouble" convert = convertDouble2CDouble #-}
{-# RULES "convert/CDouble2Double" convert = convertCDouble2Double #-}
{-# RULES "convert/Double2DoubleDouble" convert = convertDouble2DoubleDouble #-}
{-# RULES "convert/CDouble2DoubleDouble" convert = convertCDouble2DoubleDouble #-}
{-# RULES "convert/DoubleDouble2Double" convert = convertDoubleDouble2Double #-}
{-# RULES "convert/DoubleDouble2CDouble" convert = convertDoubleDouble2CDouble #-}

{-
-- this is ugly too: can't use Data.Complex because the qd bindings do
-- not implement some low-level functions properly, leading to obscure
-- crashes inside various Data.Complex functions...
data Complex c = {-# UNPACK #-} !c :+ {-# UNPACK #-} !c deriving (Read, Show, Eq)

-- complex number arithmetic, with extra strictness and cost-centres
instance Num c => Num (Complex c) where
  (!(a :+ b)) + (!(c :+ d)) = {-# SCC "C+" #-} ((a + c) :+ (b + d))
  (!(a :+ b)) - (!(c :+ d)) = {-# SCC "C-" #-} ((a - c) :+ (b - d))
  (!(a :+ b)) * (!(c :+ d)) = {-# SCC "C*" #-} ((a * c - b * d) :+ (a * d + b * c))
  negate !(a :+ b) = (-a) :+ (-b)
  abs x = error $ "Complex.abs: " ++ show x
  signum x = error $ "Complex.signum: " ++ show x
  fromInteger !x = fromInteger x :+ 0
-}

-- an extra class for some operations that can be made faster for things
-- like DoubleDouble: probably should have given this a better name
class Num c => Turbo c where
  sqr :: c -> c
  sqr !x = x * x
  twice :: c -> c
  twice !x = x + x

-- the default methods are fine for simple primitive types...
instance Turbo Float where
instance Turbo Double where
instance Turbo CDouble where

-- ...and complex numbers
instance (Real c, Floating c, Turbo c) => Turbo (Complex c) where
  sqr !(r :+ i) = (sqr r - sqr i) :+ (twice (r * i))
  twice !(r :+ i) = (twice r) :+ (twice i)

-- use the specific implementations for the higher precision types
instance Turbo DoubleDouble where
  sqr !x = DD.sqr x
  twice !(DoubleDouble a b) = DoubleDouble (twice a) (twice b)
  
instance Turbo QuadDouble where
  sqr !x = QD.sqr x
  twice !(QuadDouble a b c d) = QuadDouble (twice a) (twice b) (twice c) (twice d)

instance (Reifies s Tape, Num r) => Turbo (Reverse s r) where


data Complex r = !r :+ !r
  deriving (Read, Show, Eq)

instance (Real r, Floating r, Turbo r) => Num (Complex r) where
  (a :+ b) + (x :+ y) = (a + x) :+ (b + y)
  (a :+ b) - (x :+ y) = (a - x) :+ (b - y)
  (a :+ b) * (x :+ y) = (a * x - b * y) :+ (a * y + b * x)
  negate (a :+ b) = negate a :+ negate b
  abs c = magnitude c :+ 0
  signum = normalize
  fromInteger n = fromInteger n :+ 0

instance (Real r, Floating r, Turbo r) => Fractional (Complex r) where
  (a:+b) / (c:+d) = ((a * c + b * d)/m2) :+ ((b * c - a * d)/m2) where m2 = sqr c + sqr d
  fromRational r = fromRational r :+ 0

magnitude :: (Real c, Floating c, Turbo c) => Complex c -> c
magnitude (re:+im) = sqrt $ sqr re + sqr im

cis :: (Real c, Floating c) => c -> Complex c
cis a = cos a :+ sin a

mkPolar :: (Real c, Floating c) => c -> c -> Complex c
mkPolar r a = (r * cos a) :+ (r * sin a)

phase :: (Real c, Floating c) => Complex c -> c
phase (re:+im) = atan2 im re

normalize :: (Real c, Floating c, Turbo c) => Complex c -> Complex c
normalize z@(re:+im) = let m = magnitude z in (re / m) :+ (im / m)

atan2 :: (Real c, Floating c) => c -> c -> c
atan2 y x
      | x > 0            =  atan (y/x)
      | x == 0 && y > 0  =  pi/2
      | x <  0 && y > 0  =  pi + atan (y/x)
      | x <= 0 && y < 0  = -atan2 (-y) x
      | y == 0 && x < 0  =  pi    -- must be after the previous test on zero y
      | x == 0 && y == 0 =  y     -- must be after the other double zero tests
      | otherwise        =  x + y -- x or y is a NaN, return a NaN (via +)
