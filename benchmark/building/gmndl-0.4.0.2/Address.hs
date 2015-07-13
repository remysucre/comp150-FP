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

module Address(Address(..), angledInternalAddress, externalAngles, rayEnd, parameter, parse, pretty) where

import Prelude hiding (isNaN)

import Control.Monad (guard)
import Control.Monad.Identity (Identity())
import Data.Char (digitToInt)
import Data.List (genericDrop, genericLength, genericTake, unfoldr)
import Data.Maybe (listToMaybe)
import Data.Ratio ((%), numerator, denominator)
import Data.Vec (NearZero())
import Text.Parsec (ParsecT(), choice, digit, eof, many, many1, sepBy, string, try)
import Text.Parsec.Prim (runP)

import Complex (Complex((:+)), mkPolar, Turbo)
import MuAtom (refineNucleus)

isNaN x = not (x == x)

double :: Rational -> Rational
double angle = wrap (2 * angle)    

wrap :: Rational -> Rational
wrap angle
  | frac <  0 = frac + 1
  | otherwise = frac
  where
    _i :: Integer
    (_i, frac) = properFraction angle

data Knead = Zero | One | Star
  deriving (Eq, Show)

knead :: Rational -> [Knead]
knead angle
  | angle == 0 || angle == 1 = [Star]
  | otherwise = (++[Star]) . takeWhile (/= Star) . map k . iterate double $ angle
  where
    k a
      | a `elem` [ angle / 2 , (angle + 1) / 2 ] = Star
      | angle / 2 < a && a < (angle + 1) / 2     = One
      | a < angle / 2 || (angle + 1) / 2 < a     = Zero

period :: Rational -> Integer
period angle = genericLength (knead angle)

internalAddress :: [Knead] -> [Integer]
internalAddress v = iA 1 [Star]
  where
    iA sk vk
      | sk == genericLength v = [ ]
      | otherwise = sk' : iA sk' vk'
      where
        sk' = (+) 1 . genericLength . takeWhile id $ zipWith (==) (cycle v) (cycle vk)
        vk' = genericTake sk' v

orbit :: Eq a => (a -> Maybe a) -> a -> [a]
orbit f x = x : unfoldr (fmap both . f) x
  where
    both z = (z, z)

rho :: [Knead] -> Integer -> Maybe Integer
rho v r = listToMaybe . concat $ zipWith3 f [r+1 .. 1000000] (zipWith (flip const) v (genericDrop r (cycle v))) v
  where
    f k a b
      | a /= b = [k]
      | otherwise = []

denominators :: [Knead] -> [Integer]
denominators v = zipWith f a (tail a)
  where
    a = internalAddress v
    f sk sk1
      | sk `elem` orbit (rho v) r = (sk1 - r) `div` sk + 1
      | otherwise                 = (sk1 - r) `div` sk + 2
      where
        r | sk1 `mod` sk == 0 = sk
          | otherwise = sk1 `mod` sk

numerators :: Rational -> [Integer] -> [Integer] -> [Integer]
numerators angle = zipWith f
  where
    f qk sk = genericLength . filter (<= angle) $ [ wrap $ 2^(i * sk) * angle | i <- [0 .. qk - 2] ]

data Address = P Integer | S Integer Rational Address
  deriving (Eq, Ord, Show)

angledInternalAddress :: Rational -> Address
angledInternalAddress angle = foldr (\(s, pq) a -> S s pq a) (P (last ss)) (zip ss rs)
  where
    rs = zipWith (%) ns ds
    ns = numerators angle ds ss
    ds = denominators ks
    ss = internalAddress ks
    ks = knead angle

externalAngles :: Address -> Maybe (Rational, Rational)
externalAngles = externalAngles' 1 (0, 1)

externalAngles' :: Integer -> (Rational, Rational) -> Address -> Maybe (Rational, Rational)
externalAngles' p0 lohi a0@(P p)
  | p0 /= p = case wakees lohi p of
      [lh] -> externalAngles' p lh a0
      _ -> Nothing
  | otherwise = Just lohi
externalAngles' p0 lohi a0@(S p r a)
  | p0 /= p = case wakees lohi p of
      [lh] -> externalAngles' p lh a0
      _ -> Nothing
  | otherwise = do
      let num = numerator r
          den = denominator r
          q = p * den
          ws = wakees lohi q
          nums = [ num' | num' <- [ 1.. den - 1 ], let r' = num' % den, denominator r' == den ]
          nws, nnums :: Integer
          nws = genericLength ws
          nnums = genericLength nums
      guard (nws == nnums)
      i <- genericElemIndex num nums
      lh <- safeGenericIndex ws (i :: Integer)
      externalAngles' q lh a

wakees :: (Rational, Rational) -> Integer -> [(Rational, Rational)]
wakees (lo, hi) q =
  let gaps (l, h) n
        | n == 0 = [(l, h)]
        | n > 0 = let gs = gaps (l, h) (n - 1)
                      cs = candidates n gs
                  in  accumulate cs gs
      candidates n gs =
        let den = 2 ^ n - 1
        in  [ r
            | (l, h) <- gs
            , num <- [ ceiling (l * fromInteger den)
                      .. floor (h * fromInteger den) ]
            , let r = num % den
            , l < r, r < h
            , period r == n
            ]
      accumulate [] ws = ws
      accumulate (l : h : lhs) ws =
        let (ls, ms@((ml, _):_)) = break (l `inside`) ws
            (_s, (_, rh):rs) = break (h `inside`) ms
        in  ls ++ [(ml, l)] ++ accumulate lhs ((h, rh) : rs)
      inside x (l, h) = l < x && x < h
  in  chunk2 . candidates q . gaps (lo, hi) $ (q - 1)

chunk2 :: [t] -> [(t, t)]
chunk2 [] = []
chunk2 (x:y:zs) = (x, y) : chunk2 zs

genericElemIndex :: (Eq a, Integral b) => a -> [a] -> Maybe b
genericElemIndex _ [] = Nothing
genericElemIndex e (f:fs)
  | e == f = Just 0
  | otherwise = (1 +) `fmap` genericElemIndex e fs

safeGenericIndex :: Integral b => [a] -> b -> Maybe a
safeGenericIndex [] _ = Nothing
safeGenericIndex (x:xs) i
  | i < 0 = Nothing
  | i > 0 = safeGenericIndex xs (i - 1)
  | otherwise = Just x

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

radius :: (Real r, Floating r) => r
radius = 2 ** 24

sharpness :: Int
sharpness = 4

limit :: Int
limit = 64

distance :: Int
distance = 64

ray :: (Real r, Floating r, Turbo r) => Rational -> [Complex r]
ray angle = map fst . iterate (step angle) $ (mkPolar radius (2 * pi * fromRational angle), (0, 0))

step :: (Real r, Floating r, Turbo r) => Rational -> (Complex r, (Int, Int)) -> (Complex r, (Int, Int))
step angle (c, (k0, j0))
  | j > sharpness = step angle (c, (k0 + 1, 0))
  | otherwise = (c', (k0, j0 + 1))
  where
    k = k0 + 1
    j = j0 + 1
    m = (k - 1) * sharpness + j
    r = radius ** ((1/2) ** (fromIntegral m / fromIntegral sharpness))
    t = mkPolar (r ** (2 ** fromIntegral k0)) ((2 ** fromIntegral k0) * 2 * pi * fromRational angle)
    c' = iterate n c !! limit
    n z = z - (cc - t) / dd
     where
      (cc, dd) = ncnd k
      ncnd 1 = (z, 1)
      ncnd i = let (nc, nd) = ncnd (i - 1) in (nc * nc + z, 2 * nc * nd + 1)

rayEnd :: (Real r, Floating r, Turbo r) => Rational -> Maybe (Complex r)
rayEnd = safeLast . takeWhile (\(r:+i) -> not (isNaN r || isNaN i)) . take (sharpness * distance) . ray

parameter :: (NearZero r, Real r, Floating r, Turbo r) => Address -> Maybe (r, r, r)
parameter a = do
  (lo, hi) <- externalAngles a
  c1 <- rayEnd lo
  c2 <- rayEnd hi
  let c = 0.5 * (c1 + c2)
  return $ refineNucleus (addressPeriod a) c

addressPeriod :: Address -> Integer
addressPeriod (P p) = p
addressPeriod (S _ _ a) = addressPeriod a

parse :: String -> Maybe Address
parse s = case runP parser () "" s of
  Left _ -> Nothing
  Right a -> Just a

data Token = Number Integer | Fraction Integer Integer

type Parse t = ParsecT String () Identity t

parser :: Parse Address
parser = do
  ts <- pTokens
  accum 1 ts
  where
    accum p [] = return $ P p
    accum _ [Number n] = return $ P n
    accum _ (Number n : ts@(Number _ : _)) = do
      a <- accum n ts
      return $ S n (1%2) a
    accum _ (Number n : Fraction t b : ts) = do
      a <- accum (n * b) ts
      return $ S n (t%b) a
    accum p (Fraction t b : ts) = do
      a <- accum (p * b) ts
      return $ S p (t % b) a

pTokens :: Parse [Token]
pTokens = do
  _ <- pOptionalSpace
  ts <- pToken `sepBy` pSpace
  _ <- pOptionalSpace
  eof
  return ts

pToken :: Parse Token
pToken = choice [ try pFraction, pNumber ]

pFraction :: Parse Token
pFraction = do
  Number top <- pNumber
  _ <- pOptionalSpace
  _ <- string "/"
  _ <- pOptionalSpace
  Number bottom <- pNumber
  guard  $ top < bottom
  return $ Fraction top bottom

pNumber :: Parse Token
pNumber = do
  n <- foldl (\x y -> 10 * x + y) 0 `fmap` map (toInteger . digitToInt) `fmap` many1 digit
  guard  $ 0 < n
  return $ Number n

pSpace :: Parse [String]
pSpace = many1 (string " ")

pOptionalSpace :: Parse [String]
pOptionalSpace = many (string " ")

pretty :: Address -> String
pretty (P p) = show p
pretty (S p r a) = show p ++ " " ++ show (numerator r) ++ "/" ++ show (denominator r) ++ " " ++ pretty a
