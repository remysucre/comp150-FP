{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
module Number (R, toRational') where

import Data.Bits (bit)
import Data.Ratio ((%))
import Data.Vec (NearZero)

#ifdef HAVE_MPFR

import Data.Vec (nearZero)
import Control.Monad (guard)
import Data.Ratio (numerator, denominator)
import Numeric (readSigned)
import Data.Number.MPFR (MPFR, RoundMode(Near, Up), Precision, getPrec, int2w, fromIntegerA, stringToMPFR_, toString)
import Data.Number.MPFR.Instances.Near ()

#else

import Numeric.QD (QuadDouble)
import Numeric.QD.Vec ()

#endif

#ifdef HAVE_MPFR

instance NearZero MPFR where
  nearZero x = let p = getPrec x in not (abs x > int2w Up p 1 (4 - fromIntegral p))

newtype R = R MPFR
  deriving (Eq, Ord, Floating, Real, RealFrac, NearZero)

instance Num R where
  R a + R b = R (a + b)
  R a * R b = R (a * b)
  R a - R b = R (a - b)
  negate (R a) = R (negate a)
  abs (R a) = R (abs a)
  signum (R a) = R (signum a)
  fromInteger i = R (fromIntegerA Near bits i)

instance Fractional R where
  R a / R b = R (a / b)
  recip (R a) = R (recip a)
  fromRational r = R (fromIntegerA Near bits (numerator r) / fromIntegerA Near bits (denominator r))

instance Read R where
  readsPrec _ = readParen False . readSigned $ \s -> do
    (f, r) <- lex s
    let (n, k) = stringToMPFR_ Near bits 10 f
    guard (k == 0)
    return (R n, r)

instance Show R where
  show (R m) = toString (ceiling $ (2::Double) + log 2 / log 10 * fromIntegral (getPrec m)) m

bits :: Precision
bits = 1000

#else

newtype R = R QuadDouble
  deriving (Eq, Ord, Num, Fractional, Floating, Real, RealFrac, NearZero)

instance Show R where
  show (R m) = show m

instance Read R where
  readsPrec p = map (\(m, s) -> (R m, s)) . readsPrec p

#endif

toRational' :: RealFrac a => Int -> a -> Rational
toRational' l a = round (a * fromInteger b) % b
  where b = bit (l + 16)
