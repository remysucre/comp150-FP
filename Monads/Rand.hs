{-# OPTIONS -Wall -Werror -fno-warn-name-shadowing #-}

{-
    Rand: A module for randomness. Evaluation will require
          a StdGen.

    There are functions that are refined after speaking with Zhe.

    Diogenes A. Nunez
-}

module Monads.Rand
( eval
  , zhe, randRange, flip'
  , choose, weightedFlip
)
where

import System.Random

newtype Rand a = Rand (StdGen -> (a, StdGen))

-- Use of split comes from Zhe
instance Monad Rand where
    return x = Rand (\g -> (x, g))
    Rand r >>= f = Rand (\g -> let (g', g'') = split g
                                   (x, _) = r g'
                                   Rand s = (f x) 
                               in s g''
                        )

eval :: Rand a -> (StdGen -> (a, StdGen))
eval (Rand r) = r

-- 50/50 chance of holding true
flip' :: Rand Bool
flip' = Rand (\g -> let (x, g') = next g
                    in (even x, g')
             )


-- returns an integer in this range
-- Implementation is Zhe's
randRange :: (Int, Int) -> Rand Int
randRange (start, end) = Rand (\g -> let (x, g') = next g
                                         y = (x `mod` end) + start
                                     in (y, g')
                              )
{-
-- Remnants of a bad idea
randRange (start, end) = Rand (\g -> let
                                     -- inRange :: (Int, t) -> Bool
                                        inRange (x, _) = x > start && x < end
                                     -- nextSrc :: RandomGen b => (t, b) -> (Int, b)
                                        nextSrc (_, g) = (y, g')
                                                         where
                                                             (x, g') = next g 
                                                             y = x `mod` end
                                     in until inRange nextSrc $ next g 
                              )
-}

-- Zhe wants a Random integer
zhe :: Rand Int
zhe = Rand (\g -> next g)

-- choose a random element from the list
choose :: [a] -> Rand a
choose [] = error "No choice from empty"
choose as = do
              x <- randRange (0, length as)
              return (as !! x)

-- turns up True with probability chance up to 1
weightedFlip :: Double -> Rand Bool
weightedFlip chance = Rand (\g -> let (x, g') = next g
                                      x' = toInteger x
                                      x'' = fromInteger $ x' `mod` 100
                                      y = x'' / 100.0
                                  in if y < chance then (True, g')
                                                   else (False, g')
                           )
