{-# LANGUAGE BangPatterns #-}
-- ==================================
-- Module name: PylosAI
-- Project: Pylos
-- Copyright (C) 2008  Bartosz Wojcik
-- Created on: 17.10.2008
-- Last update: 06.11.2008
-- Version: %

{-  This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
-- =========================================================
-- |
-- This module delivers AI functions that work over tree of moves.
-- It has been created using directly ideas of John Hughes presented
-- in his paper "Why Functional Programming Matters".
-- As Miranda can be translated into Haskell almost directly,
-- the code presented in the paper has big simillarity to ones in this module.

module PylosAI (Algorithm (..),
                evaluate,
                maximise,
                minimise,
                sizeGT,
                drawGT
               )
where

-- =========================================================
import Graphics.Rendering.OpenGL (GLint)
import qualified Data.Map as Map (fold,
                                  (!))
import Data.List
import Data.Tree
import Data.Array (listArray,(!))
import PylosBoard (Pylos (..),
                   Coordinate,
                   Player (..),
                   NbrOfBalls,
                   putable,
                   moveable,
                   moveOnCoordinate,
                   anyTakeable,
                   nextPlayer,
                   initPylos,
                   terminator
                   )
import PylosMove (Move (..),
                  moves,
                  coordinatesOfLastMove,
                  movesResult
                  )
-- =========================================================

-- =========================================================
data Algorithm = SimpleDiff
               | SimpleDiffMiddle
               | SimpleDiffPlusVal
               | SimpleDiffAdvance
               | BalancedDiffPlusVal
               | MixedDiff
   deriving (Eq,Read,Show,Enum,Ord)
-- =========================================================

-- =========================================================
-- |
-- AI of Pylos depends on algorithm and depth of analysis.
data AIPylos = AIPylos Algorithm              --  ^ Playing algorithm
                       Move                   --  ^ move
   deriving (Eq,Read,Show)
-- =========================================================

-- =========================================================
instance Ord (AIPylos) where
-- ---------------------------------------------------------
   -- Rule of simplest difference of number of balls comparison
   compare (AIPylos SimpleDiff (NextMove _ _ (w1,b1) _))
           (AIPylos _          (NextMove _ _ (w2,b2) _))    = compare (w1 - b1) (w2 - b2)

   -- Rule that compares difference of balls. In case they are equal, it compares number of
   -- built 3s.
   compare (AIPylos SimpleDiffPlusVal (NextMove p1 _ (w1,b1) _))
           (AIPylos _                 (NextMove p2 _ (w2,b2) _))
            | d1 == d2                            = compare (pylosVal (+|) p1) (pylosVal (+|) p2)
            | otherwise                           = compare d1 d2
            where d1 = w1 - b1
                  d2 = w2 - b2

   -- Rule that compares difference of balls. In case they are equal, it perfers middle board positions
   compare (AIPylos SimpleDiffMiddle (NextMove p _ (w1,b1) cs1))
           (AIPylos _                (NextMove _ _ (w2,b2) cs2))
            | d1 == d2                            = compare ((fromMiddle s . head . last) cs1) ((fromMiddle s . head . last) cs2)
            | otherwise                           = compare d1 d2
            where d1 = w1 - b1
                  d2 = w2 - b2
                  s = size p

   -- Rule that compares difference of balls. In case they are equal, it perfers positions of bigger 'advance' value.
   compare (AIPylos SimpleDiffAdvance (NextMove p1 _ (w1,b1) cs1))
           (AIPylos _                 (NextMove p2 _ (w2,b2) cs2))
            | d1 == d2                            = compare v1 v2
            | otherwise                           = compare d1 d2
            where d1 = w1 - b1
                  d2 = w2 - b2
                  v1 = (-1) * advance p1 Map.! (head . last) cs1
                  v2 = (-1) * advance p2 Map.! (head . last) cs2

   -- Rule that compares balanced difference of balls. In case they are equal, it compares number of already
   -- built 3s. "Balanced" means that in case of not many balls in hand, it may be better to diminish
   -- number of balls for both players equally than to increase difference of 1. Why? Because it brings faster
   -- towards victory.
   compare (AIPylos BalancedDiffPlusVal (NextMove p1 pl1 (w1,b1) cs1))
           (AIPylos _                   (NextMove p2 pl2 (w2,b2) cs2))
            | d1 == d2                            = compare ((pylosVal (+|) p1) // sum1) ((pylosVal (+|) p2) // sum2)
            | otherwise                           = compare (d1 // sum1) (d2 // sum2)
            where d1 = w1 - b1
                  d2 = w2 - b2
                  sum1 = w1 + b1
                  sum2 = w2 + b2

   -- The idea is following. Addtitionall to counted balls on both hands one can try
   -- to recognize board situation. Player who's next on move and has at least one row, column or square
   -- of 3 balls with empty 4th place can in next move have one ball more on hand. This rule is
   -- not 100% correct, but at least can be used as rough estimation on next move advantages.
   -- Player who's done last move and have at least 2 sets of rows, columns or squares full of 3 own
   -- balls is in same situation - can get one ball more in his||her next move. So is such situation
   -- is recognized, the difference of balls on hands is updated accordingly.
   compare (AIPylos MixedDiff (NextMove p1 pl1 (w1,b1) cs1))
           (AIPylos _         (NextMove p2 pl2 (w2,b2) cs2)) = compare (d1 + mixedDiff p1 pl1) (d2 + mixedDiff p2 pl2)
            where d1 = w1 - b1
                  d2 = w2 - b2

   -- Just in case
   compare _ _  = EQ
-- =========================================================

-- =========================================================
fromMiddle :: GLint -> Coordinate -> Float
-- ---------------------------------------------------------
fromMiddle size (l,(x,y)) = fromIntegral size - abs (fromIntegral x - p) - abs (fromIntegral y - p)
           where p = ((size - l) + 1) // 2 - 1
-- =========================================================

-- =========================================================
-- | Recognition of future advantages. Advantage in next move is recognized if there is at least on set
-- of 3 own balls with empty 4th filed. Advantage of 2nd next move is recognized if there are at least
-- 2 such sets.
mixedDiff :: (Integral a) => Pylos -> Player -> a
-- ---------------------------------------------------------
mixedDiff pyl player | player == WhitePlayer && nbr3s <= 0 ||
                       player == BlackPlayer && nbr3s >= 0 = (signum . round) nbr3s
                     | abs nbr3s < 2 = 0
                     | otherwise = (signum . round) nbr3s
          where nbr3s = fromIntegral (pylosVal (+|) pyl) / 3 -- tailored for pylos size 4
-- =========================================================

-- =========================================================
pylosVal :: (GLint -> GLint -> GLint) -> Pylos -> GLint
-- ---------------------------------------------------------
pylosVal f pylos = pVal f (squaresOf4 pylos) + pVal f (rows pylos) + pVal f (columns pylos)
-- =========================================================

-- =========================================================
pVal :: (GLint -> GLint -> GLint) -> NbrOfBalls -> GLint
-- ---------------------------------------------------------
pVal f = Map.fold f 0
-- =========================================================

-- =========================================================
takeTerminated :: [Coordinate] -> Bool
-- ---------------------------------------------------------
takeTerminated cs = head cs == terminator
-- =========================================================

-- =========================================================
-- |
-- This functions adds only agruments >= than 3 (in abs sense).
-- It's used to sum up all long enough sequences of balls.
-- "Long enough" have been chosen on experimental way for pylos size 4 and won't work properly for other sizes.
(+|) :: (Num n, Ord n) => n -> n -> n
-- ---------------------------------------------------------
a +| b | not $ abs a == 3 = b
       | not $ abs b == 3 = a
--       | abs a < 3 = b
--       | abs b < 3 = a
       | otherwise = a + b
-- =========================================================

-- =========================================================
(//) :: (Integral a, Fractional b, Integral a1) => a -> a1 -> b
-- ---------------------------------------------------------
a // b = fromIntegral a / fromIntegral b
-- =========================================================

-- =========================================================
moveOfAIPylos :: AIPylos -> Move
moveOfAIPylos (AIPylos _ move) = move
-- =========================================================

-- =========================================================
repTree :: Ord a => (a -> [a]) -> a -> Tree a
-- ---------------------------------------------------------
repTree f m = Node m (map (repTree f) (f m))
-- =========================================================

-- =========================================================
gameTree :: Int -> Move -> Tree Move
-- ---------------------------------------------------------
gameTree 0       = repTree (sort . moves)
gameTree breadth = repTree (take breadth . sort . moves)
-- =========================================================

-- =========================================================
-- |
-- Dynamic pruning of tree. It pruns at least to given depth,
-- deeper if foreseen size of tree is not achieved yet.
pruneD :: Int -> Int -> Int -> Tree Move -> Tree Move
-- ---------------------------------------------------------
pruneD depth 0     _       tree = prune   depth tree
pruneD depth limit breadth tree = pruneD' depth limit 1 breadth tree
-- =========================================================
pruneD' :: Int             --  ^ Depth of pruning - how many leves more are to stay.
        -> Int             --  ^ Limit of number of nodes to be kept. In format 16^limit.
        -> Integer         --  ^ Current number of nodes -- estimated!
        -> Int             --  ^ Max breadth of each subtree - important for current number of nodes estimation.
        -> Tree Move       --  ^ Input tree
        -> Tree Move       --  ^ Output tree
-- ---------------------------------------------------------
pruneD' 0 0 _  _ (Node a sub) = Node a []
pruneD' d n 0  b _            = error $ "pruneD': broken counter: " ++ show d ++ " " ++ show n ++ " " ++ show b
pruneD' 0 n !m 0        (Node a sub) | len == 0 = error $ " result1: " ++ show i ++ " " ++ show n ++ " " ++ show m
                                      | 16^n + 100 <= m * len^2  = Node a []
                                    | otherwise      = Node a (map (pruneD' 1 n (m*len) 0) sub)
                          where len = maxBreadth ! i
                                i = movesResult a
pruneD' d n !m 0       (Node a sub)  | len == 0 = error $ " result2: " ++ show i ++ " " ++ show n
                                     | otherwise = Node a (map (pruneD' (d-1) n (m*len) 0) sub)
                          where len = maxBreadth ! i
                                i = movesResult a
pruneD' 0 n !m breadth (Node a sub) | abs i > 300 = error $ " result3: " ++ show i ++ " " ++ show n
                                      | 16^n + 100 <= m * len^2  = Node a []
                                    | otherwise      = Node a (map (pruneD' 1 n (m*len) breadth) sub)
                          where len = min (fromIntegral breadth) (maxBreadth ! i)
                                i = movesResult a
pruneD' d n !m breadth (Node a sub)  | abs i > 300 = error $ " result4: " ++ show i ++ " " ++ show n
                                     | otherwise = Node a (map (pruneD' (d-1) n (m*len) breadth) sub)
                          where len = min (fromIntegral breadth) (maxBreadth ! i)
                                i = movesResult a
-- =========================================================

-- =========================================================
prune :: Int -> Tree Move -> Tree Move
-- ---------------------------------------------------------
prune 0 (Node a sub) = Node a []
prune n (Node a sub) = Node a (map (prune (n-1)) sub)
-- =========================================================

-- =========================================================
sizeGT :: Int -> Int -> Int -> Move -> Int
-- ---------------------------------------------------------
sizeGT depth limit b = sizeT . pruneD depth limit b . gameTree b
-- =========================================================

-- =========================================================
sizeT :: Tree a -> Int
-- ---------------------------------------------------------
sizeT (Node a sub) = foldl (\x y -> x + sizeT y) 1 sub
-- =========================================================

-- =========================================================
-- | This table's figures have been created on experimental way.
-- maxBreadth = listArray (-30,0) [16,15,14,13,13,12,12,12,15,20,22,25,30,34,40,50,57,40,32,26,20,15,10,6,4,3,4,3,2,1,0]
-- maxBreadth = listArray (-30,30) ([16,15,14,13,13,12,12,12,15,20,20,20,20,20,20,20,20,20,20,20,15,15,10,6,4,3,4,3,2,1,100] ++ replicate 30 100)
maxBreadth = listArray (-30,300) ([16,15,14,13,13,12,12,12,15,15,16,16,16,16,16,16,16,16,16,15,10,10,10,6,4,3,4,3,2,1,100] ++ replicate 300 100)
-- =========================================================

drawGT depth limit b = drawTree . fmap showShort . pruneD depth limit b . gameTree b

showShort (NextMove _ player result moves) = show player ++ " " ++ show result ++ " " ++ show moves

-- =========================================================
-- |
-- The function omitMin is passed a "potential maximum" - the largest minimum seen
-- so far - and omits any minima which are less than this.
omitMin :: (Ord a) => a -> [[a]] -> [a]
-- ---------------------------------------------------------
omitMin pot [] = []
omitMin pot (nums:rest)
   | minleq nums pot       = omitMin pot rest
   | otherwise             = minNums:(omitMin minNums rest)
   where minNums = minimum nums
-- =========================================================
omitMax :: (Ord a) => a -> [[a]] -> [a]
-- ---------------------------------------------------------
omitMax pot [] = []
omitMax pot (nums:rest)
   | maxleq nums pot       = omitMax pot rest
   | otherwise             = maxNums:(omitMax maxNums rest)
   where maxNums = maximum nums
-- =========================================================

-- =========================================================
-- |
-- Minleq takes a list of numbers and a potential maximum, and returns true if the
-- minimum of the list of numbers is less than or equal to the potential maximum.
-- To do this, it does not need to look at all the list! If there is any element in the
-- list less than or equal to the potential maximum, then the minimum of the list
-- is sure to be. All elements after this particular one are irrelevant. Therefore minleq can be defined by
minleq :: (Ord t) => [t] -> t -> Bool
-- ---------------------------------------------------------
minleq [] pot                       = False
minleq (num:rest) pot | num <= pot  = True
                      | otherwise   = minleq rest pot
-- =========================================================
maxleq :: (Ord t) => [t] -> t -> Bool
-- ---------------------------------------------------------
maxleq [] pot                       = False
maxleq (num:rest) pot | num >= pot  = True
                      | otherwise   = maxleq rest pot
-- =========================================================

-- =========================================================
-- |
-- mapmin omits the minima of lists whose minimum doesn't matter.
mapmin :: (Ord a) => [[a]] -> [a]
-- ---------------------------------------------------------
mapmin (nums:rest) = minNums:(omitMin minNums rest)
   where minNums = minimum nums
-- =========================================================
mapmax :: (Ord a) => [[a]] -> [a]
-- ---------------------------------------------------------
mapmax (nums:rest) = maxNums:(omitMax maxNums rest)
   where maxNums = maximum nums
-- =========================================================


-- =========================================================
-- |
-- maximise' and minimise' function implement alpha-beta algortihm
maximise' :: (Ord a) => Tree a -> [a]
-- ---------------------------------------------------------
maximise' (Node n []) = n:[]
maximise' (Node n l) = mapmin (map minimise' l)
-- =========================================================
minimise' :: (Ord a) => Tree a -> [a]
-- ---------------------------------------------------------
minimise' (Node n []) = n:[]
minimise' (Node n l) = mapmax (map maximise' l)
-- =========================================================

-- =========================================================
maximise :: (Ord a) => Tree a -> a
maximise = maximum . maximise'
-- =========================================================
minimise :: (Ord a) => Tree a -> a
minimise = minimum . minimise'
-- =========================================================

-- =========================================================
compRev :: (Ord a) => a -> a -> Ordering
-- ---------------------------------------------------------
compRev x y | x == y    = EQ
            | x <= y    = GT
            | otherwise = LT
-- =========================================================

-- =========================================================
-- | Evaluates next move.
evaluate :: Algorithm                          --  ^ Using this algorithm
         -> Int                                --  ^ Until this depth of tree of moves
         -> Int                                --  ^ or deeper until this limit of nodes
         -> (Tree AIPylos -> AIPylos)          --  ^ Using this minimax alpha beta function
         -> Int                                --  ^ Limit of subtrees of tree of moves (if > 0)
         -> Move                               --  ^ Last move.
         -> [Coordinate]                       --  ^ Returns move in terms of coordinates
-- ---------------------------------------------------------
evaluate alg depth limit minmax breadth = 
         coordinatesOfLastMove . moveOfAIPylos . minmax . fmap (AIPylos alg) . pruneD depth limit breadth . gameTree breadth
-- =========================================================


