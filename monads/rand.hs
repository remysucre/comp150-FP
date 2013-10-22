{-# OPTIONS -Wall -Werror -fno-warn-name-shadowing #-}

import System.Random

newtype Rand a = Rand (StdGen -> (a, StdGen))

instance Monad Rand where
    return x = Rand (\g -> (x, g))
    Rand r >>= f = Rand (\g -> let (x, g') = r g
                                   Rand s = (f x) 
                               in s g'
                        )

-- 50/50 chance of holding true
flip :: Rand Bool
flip = Rand (\g -> let g' = fst $ split g
                       (x, g'') = next g'
                   in (even x, g'')
            )

-- always has 7
seven :: Rand Int
seven = return 7

-- returns an integer in this range
randRange :: (Int, Int) -> Rand Int
randRange (start, end) = Rand (\g -> let
                                     -- inRange :: (Int, t) -> Bool
                                        inRange (x, _) = x >= start && x <= end
                                     -- nextSrc :: RandomGen b => (t, b) -> (Int, b)
                                        nextSrc (_, g) = next g 
                                     in until inRange nextSrc $ next $ fst $ split g 
                              )
