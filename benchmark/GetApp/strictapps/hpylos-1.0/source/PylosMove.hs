-- ==================================
-- Module name: PylosMove
-- Project: Pylos
-- Copyright (C) 2008  Bartosz Wójcik
-- Created on: 10.10.2008
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
-- | This module delivers tree of moves over goven status of Pylos game.
module PylosMove (Move (..),
                  moves,
                  coordinatesOfLastMove,
                  movesResult
                  )
where

-- =========================================================
import Graphics.Rendering.OpenGL (GLint)
import Data.List (partition,
                  sort)
import qualified Data.Map as Map ((!))
import PylosBoard (Pylos (..),
                   Coordinate,
                   Player (..),
                   putable,
                   moveable,
                   takeable,
                   moveOnCoordinate,
                   anyTakeable,
                   ifTakeAfterPut,
                   nextPlayer,
                   terminator
                   )
-- =========================================================

-- =========================================================
-- | Move constitutes tree of moves over Pylos structure.
data Move = NextMove Pylos          --  ^ Game state before the move.
                     Player         --  ^ Player on current move.
                     (Int,Int)      --  ^ Number of balls of both players not yet on board after the move. Fst for white player.
                     [[Coordinate]] --  ^ List of moves where move is list of actions
          | Take1Ball Pylos
                      Player
                      (Int,Int)      --   ^ Number of balls of both players not yet on board. Fst for white player.
                      [[Coordinate]] -- ^ List of moves where move is list of actions
          | PutBall  Pylos
                     Player
                     (Int,Int)      --  ^ Number of balls of both players not yet on board. Fst for white player.
                     GLint          --  ^ Minimum level the ball has to be put on.
                     [[Coordinate]] --  ^ List of moves where move is list of actions
   deriving (Eq,Read,Show)
-- =========================================================

-- =========================================================
-- |
-- List of next moves can be sorted unsing expected value of move which is precalaculated.
instance Ord (Move) where
   compare (NextMove pyl1 WhitePlayer (w1,b1) _)
           (NextMove pyl2 _           (w2,b2) _) =           compare (w1 - b1) (w2 - b2)-- compare (w2 - b2) (w1 - b1)
   compare (NextMove pyl1 BlackPlayer (w1,b1) _)
           (NextMove pyl2 _           (w2,b2) _) =           compare (w2 - b2) (w1 - b1) --compare (w1 - b1) (w2 - b2) --
   compare m1 m2 = error $ "Move compare: not allowed values " ++ show m1 ++ show m2
-- =========================================================

-- =========================================================
pylosOfMove :: Move -> Pylos
-- ---------------------------------------------------------
pylosOfMove (NextMove pylos _ _ _)     = pylos
-- pylosOfMove (Take2Balls pylos _ _ _ _)   = pylos
pylosOfMove (Take1Ball pylos _ _ _)  = pylos
pylosOfMove (PutBall  pylos _ _ _ _)   = pylos
-- =========================================================

-- =========================================================
playerOnMove :: Move -> Player
-- ---------------------------------------------------------
playerOnMove (NextMove _ player _ _)     = player
-- playerOnMove (Take2Balls _ player _ _ _)   = player
playerOnMove (Take1Ball _ player _ _)  = player
playerOnMove (PutBall  _ player _ _ _)   = player
-- =========================================================

-- =========================================================
movesResult :: Move -> Int
-- ---------------------------------------------------------
movesResult (NextMove _ _ (w,b) _) = (-w) - b
movesResult m                      = error $ "movesRsult: " ++ show m
-- =========================================================

-- =========================================================
coordinatesOfLastMove :: Move -> [Coordinate]
-- ---------------------------------------------------------
coordinatesOfLastMove (NextMove _ _ _ cs) = last cs
coordinatesOfLastMove _                   = []       -- this should never happen
-- =========================================================

-- =========================================================
unfinishedMove :: Move -> Bool
-- ---------------------------------------------------------
unfinishedMove (NextMove _ _ _ _) = False
unfinishedMove _                  = True
-- =========================================================
-- finishedMove :: Move -> Bool
-- ---------------------------------------------------------
-- finishedMove (NextMove _ _ _ _) = True
-- finishedMove _                  = False
-- =========================================================

-- =========================================================
-- |
-- This function takes current game status and returns list of possible moves out of given status.
-- netxPass creates a pair of lists. First list contains moves not finished yet, i.e. where part of the
-- move has been performed. E.g. ball has been taken but not yet put. This list has to be injected
-- to moves function again in order to be finished.
-- Second list contains all properly finished moves.
-- Finished move is recognized by player on move. Same player before and after move means
-- that move is to be continued. Different player - opposite.
-- It can be recognized by Move contructor. Only NextMove constructor points finished move.
moves :: Move -> [Move]
-- ---------------------------------------------------------
moves move@(NextMove pylos player _ _) = (concat.map moves) (fst nextPass) ++ snd nextPass
      where nextPass = partition unfinishedMove (map (nextTaken move) (allMoveables player pylos) ++
                                                 map (nextPut move) (allPutables 0 pylos))
-- moves move@(Take2Balls pylos player _ _ _) = (concat.map moves) (fst nextPass) ++ snd nextPass
--      where nextPass = partition unfinishedMove (map (nextTaken move) (allTakeables player pylos coordinateForAny))
moves move@(Take1Ball pylos player _ _) = (concat.map moves) (fst nextPass ) ++ snd nextPass ++ [terminate move]             --
      where nextPass = partition unfinishedMove (map (nextTaken move) (allTakeables player pylos)) -- coordinate))
moves move@(PutBall pylos player _ l _) = (concat.map moves) (fst nextPass) ++ snd nextPass
      where nextPass = partition unfinishedMove (map (nextPut move) (allPutables l pylos))
-- =========================================================

-- =========================================================
-- coordinateForAny :: Coordinate
-- coordinateForAny = (1000,(0,0))
-- =========================================================

-- =========================================================
-- | This function returns new move after ball has been taken.
nextTaken :: Move -> Coordinate -> Move
-- ---------------------------------------------------------
nextTaken (NextMove pylos player (whites,blacks) cs) coordinate@(l,(x,y)) =
          PutBall newPylos player (newQuantity player) (l+1) ([coordinate]:cs)
          where newPylos = moveOnCoordinate player coordinate pylos
                newQuantity WhitePlayer = (whites + 1,blacks)
                newQuantity BlackPlayer = (whites,blacks + 1)
{- nextTaken (Take2Balls pylos player plA (whites,blacks) (c:cs)) coordinate
          | anyTakeable player newPylos               = Take1Ball newPylos player plA (newQuantity player) coordinate ((coordinate:c):cs)
          | otherwise                                 = NextMove newPylos (nextPlayer player) plA (newQuantity player) ((coordinate:c):cs)
          where newPylos = moveOnCoordinate player coordinate pylos
                newQuantity WhitePlayer = (whites + 1,blacks)
                newQuantity BlackPlayer = (whites,blacks + 1) -}
nextTaken (Take1Ball pylos player (whites,blacks) (c:cs)) coordinate
          =          NextMove newPylos (nextPlayer player) (newQuantity player) ((coordinate:c):cs)
          where newPylos = moveOnCoordinate player coordinate pylos
                newQuantity WhitePlayer = (whites + 1,blacks)
                newQuantity BlackPlayer = (whites,blacks + 1)
nextTaken move coordinate                                              = error "nextTaken of PutBall"
-- =========================================================

-- =========================================================
-- | This function returns new move after ball has been put.
nextPut :: Move -> Coordinate -> Move
-- ---------------------------------------------------------
nextPut m@(NextMove pylos player (whites,blacks) cs) coordinate
          | ifTakeAfterPut newPylos player coordinate  && notYetWon player (whites,blacks)
                                                      -- First ball is taken always from same place the last one has been put. This makes number of
                                                      -- possible moves smaller.
                                                      = Take1Ball pylos player (whites,blacks) ([coordinate,coordinate]:cs)  -- must be not existing coordinate of not existing great level!!
--                                                      = Take2Balls newPylos player plA (newQuantity player) ([coordinate]:cs)  -- must be not existing coordinate of not existing great level!!
          | ifTakeAfterPut newPylos player coordinate = NextMove newPylos (nextPlayer player) (newQuantity player) ([terminator,coordinate]:cs) -- to finish won game
          | otherwise                                 = NextMove newPylos (nextPlayer player) (newQuantity player) ([coordinate]:cs)
          where newPylos = moveOnCoordinate player coordinate pylos
                newQuantity WhitePlayer = (whites - 1,blacks)
                newQuantity BlackPlayer = (whites,blacks - 1)
                notYetWon WhitePlayer (_,0) = False
                notYetWon WhitePlayer _     = True
                notYetWon BlackPlayer (0,_) = False
                notYetWon BlackPlayer _     = True
nextPut (PutBall pylos player (whites,blacks) level (c:cs)) coordinate
          | ifTakeAfterPut newPylos player coordinate = Take1Ball pylos player (whites,blacks) ((coordinate:coordinate:c):cs)
--                                                      Take2Balls newPylos player plA (newQuantity player) ((coordinate:c):cs)
          | otherwise                                 = NextMove newPylos (nextPlayer player) (newQuantity player) ((coordinate:c):cs)
          where newPylos = moveOnCoordinate player coordinate pylos
                newQuantity WhitePlayer = (whites - 1,blacks)
                newQuantity BlackPlayer = (whites,blacks - 1)
nextPut move _  = error "nextPut of Take2Balls"
-- =========================================================

-- =========================================================
-- | Instead of taking 2nd ball player can decide to finish own turn in status quo.
terminate :: Move -> Move
-- ---------------------------------------------------------
terminate (Take1Ball pylos player (w,b) (c:cs)) = NextMove pylos (nextPlayer player) (newQuantity player) ((terminator:c):cs) --(w,b) ((terminator:c):cs) --
          -- Following 2 lines are supposed to cause not choosing terminator.
          where newQuantity WhitePlayer = (w,b + 1)
                newQuantity BlackPlayer = (w + 1,b)
terminate _                                      = error "terminate: input not matched"
-- =========================================================

-- =========================================================
-- |
-- Checks if player is allowed to take any ball after he||she's just put one (eg. any square of 4 is full)
-- and if he||she can take any ball - which is obvious, because recently put ball always can be taken.
-- anyBallTakeable              :: Pylos -> Player -> Bool
-- anyBallTakeable pylos player = ifTakeAfterPut pylos player && anyTakeable player pylos
-- =========================================================

-- =========================================================
-- | Gives list of all coordinates where one can put a ball.
-- Takes level and current game situation.
allPutables :: GLint -> Pylos -> [Coordinate]
-- ---------------------------------------------------------
allPutables level pylos = filter (putable pylos) keys
            where keys = [(l,(x,y)) | l<-[level..s-1], x<-[0..s-l-1], y<-[0..s-l-1]]
                  s = size pylos
-- =========================================================

-- =========================================================
-- | Gives list of all moveable fields on the board.
allMoveables :: Player -> Pylos -> [Coordinate]
-- ---------------------------------------------------------
allMoveables player pylos = filter (moveable player pylos) keys
            where keys = [(l,(x,y)) | l<-[0..s-1], x<-[0..s-l-1], y<-[0..s-l-1]]
                  s = size pylos
-- =========================================================

-- =========================================================
-- | Gives list of all takeable fields on the board, that fulfil additional condition.
-- This condition is order condition which prevents taking two same balls in different order.
--allTakeables :: Player -> Pylos -> Coordinate -> [Coordinate]
allTakeables :: Player -> Pylos -> [Coordinate]
-- ---------------------------------------------------------
--allTakeables player pylos c = filter (takeable player pylos) keys
allTakeables player pylos = filter (takeable player pylos) keys
            where keys = [(l,(x,y)) | l<-[0..s-1], x<-[0..s-l-1], y<-[0..s-l-1]] -- , inProperOrder c (l,(x,y))]
                  s = size pylos
-- =========================================================

{-- =========================================================
inProperOrder :: Coordinate -> Coordinate -> Bool
-- ---------------------------------------------------------
inProperOrder oldC@(l1,(x1,y1)) newC@(l2,(x2,y2)) = l1 > l2 ||
                                                    l1 == l2 && (x1 > x2 ||
                                                                 x1 == x2 && y1 > y2)
-- =========================================================
-}

