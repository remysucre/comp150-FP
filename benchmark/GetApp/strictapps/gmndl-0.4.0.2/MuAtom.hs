{-# LANGUAGE BangPatterns, RecordWildCards, Rank2Types, FlexibleContexts #-}

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

module MuAtom (muAtom, refineNucleus) where

import Data.Ratio (numerator, denominator)
import Data.List (genericIndex, genericSplitAt)
import Data.Vec (NearZero())
import Numeric.QD (QuadDouble())
import Roots (root2, root4, lift, FF)
import Complex (Complex((:+)), magnitude, phase, cis, mkPolar, normalize, Turbo)

type N = Integer
type Q = Rational
type R = QuadDouble
type C = Complex R

-- a Mandelbrot Set mu-atom

data Atom = Atom{ nucleus :: !C, period :: !N, root :: !C, cardioid :: !Bool }
  deriving Show

continent :: Atom
continent = Atom 0 1 1 True

-- finding bond points

fdf :: (Integral i, Num c) => i -> c -> c -> (c, c)
fdf !n !z !c = let (fzs, fz:_) = genericSplitAt n $ iterate (\w -> w * w + c) z
               in  (fz, 2 ^ n * product fzs)  -- [ f i z c | i <- [0 .. n - 1] ]

bondIter :: (Real r, Floating r) => Integer -> Complex r -> FF [] [] r
bondIter !n !(br:+bi) [x0, x1, x2, x3] =
  let !z = x0:+x1
      !c = x2:+x3
      !b = lift br :+ lift bi
      (!fz, !dfz) = fdf n z c
      !(y0 :+ y1) =  fz - z -- f n z c - z
      !(y2 :+ y3) = dfz - b -- df n z c - (lift br :+ lift bi)
  in  [y0, y1, y2, y3]
bondIter _ _ _ = error "MuAtom.bondIter: internal error"

-- finding nucleus

l :: (Integral i, Num c) => i -> c -> c
l !n !c = (`genericIndex` n) . iterate (\z -> z * z + c) $ 0

nucleusIter :: (Real r, Floating r) => Integer -> FF [] [] r
nucleusIter !n [x0, x1] =
  let !c = x0 :+ x1
      !(y0 :+ y1) = l n c
  in  [y0, y1]
nucleusIter _ _ = error "MuAtom.nucleusIter: internal error"


refineNucleus :: (NearZero r, Real r, Floating r, Turbo r) => Integer -> Complex r -> (r, r, r)
refineNucleus p root@(gr :+ gi) =
  let eps = 1e-20 -- FIXME
      [cr, ci] = root2 eps (nucleusIter p) [gr, gi]
      [_, _, b0r, b0i] = root4 eps (bondIter p ( 1)) [cr, ci, cr, ci]
      [_, _, b1r, b1i] = root4 eps (bondIter p (-1)) [cr, ci, cr, ci]
      bond0 = b0r :+ b0i
      bond1 = b1r :+ b1i
      r = magnitude (bond1 - bond0)
  in  (cr, ci, r)

-- finding descendants

muChild :: Atom -> Q -> Atom
muChild !Atom{..} !address =
  let -- some properties of the parent and its relation to the child
      !size = magnitude (root - nucleus)
      !address' = fromIntegral (numerator address) / fromIntegral (denominator address)
      !angle = 2 * pi * address'
      !(bar :+ bai) = cis angle
      !bondAngle = bar :+ bai
      !child = period * denominator address
      -- perturb from the stable nucleus to help ensure convergence to the bond point
      !_initial@(ir :+ ii) = nucleus + mkPolar (size / 2) (phase (root - nucleus) + angle)
      [_, _, br, bi] = {-# SCC "bond" #-} root4 eps (bondIter period bondAngle) [ ir, ii, ir, ii ]
      !bondPoint = br :+ bi
      -- estimate where the nucleus will be
      !radiusEstimate
        | cardioid  = size / m2 * sin (pi * address')
        | otherwise = size / m2
        where m2 = fromIntegral (denominator address) ^ (2 :: N)
      !deltaEstimate = bondPoint - nucleus
      !_guess@(gr :+ gi) = bondPoint + (radiusEstimate :+ 0) * normalize deltaEstimate
      -- refine the nucleus estimate
      [cr, ci] = {-# SCC "nucleus" #-} root2 eps (nucleusIter child) [gr, gi]
      !childNucleus = cr :+ ci
      eps = radiusEstimate / 10000000
  in  Atom childNucleus child bondPoint False

muChildren :: Atom -> [Q] -> [Atom]
muChildren !a [] = [a]
muChildren !a (q:qs) = let b = muChild a q in a : muChildren b qs

-- interface to the outside world

muAtom :: [Q] -> (R, R, R, N)
muAtom qs =
  let Atom{..} = last $ muChildren continent qs
      r :+ i = nucleus
      s = magnitude (nucleus - root)
      p = period
  in (r, i, s, p)
