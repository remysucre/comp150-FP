-- ==================================
-- Module name: PylosBoard
-- Project: Pylos
-- Copyright (C) 2008  Bartosz Wójcik
-- Created on: 09.06.2008
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
-- ==================================
-- | This module defines board of Pylos game.
--   It contains also board manipulation and verification functions.
module PylosBoard (Stone (..),
                   Coordinate,
                   Board,
                   IntMapBoard,
                   NbrOfBalls,
                   Pylos (..),
                   Player(..),
                   initPylos,
                   putStone,
                   putable,
                   putableForMove,
                   takeStone,
                   takeable,
                   moveable,
                   updateAllBases,
                   updateAllSuccessors,
                   anyTakeable,
                   moveOnCoordinate,
                   ifTakeAfterPut,
                   nextPlayer,
                   isFree,
                   stone2Player,
                   nbrOfBalls,
                   terminator
                  )
where


-- ===================================
import Graphics.Rendering.OpenGL (GLint)
import qualified Data.Map as Map
-- import Mapext (mapAny)
-- ===================================

-- ===================================
-- | Each field of 'Board' must be one of following type.
data Stone = NoStone
           | WhiteStone
           | BlackStone
   deriving (Eq,Ord,Enum,Read,Show)

-- | Playes definition
data Player = WhitePlayer
            | BlackPlayer
   deriving (Eq,Ord,Enum,Read,Show)

-- | Casts Players on Stones
player2Stone :: Player -> Stone
-- ----------------------------------
player2Stone WhitePlayer = WhiteStone
player2Stone BlackPlayer = BlackStone

-- | Casts Stones of Players
stone2Player :: Stone -> Player
-- ----------------------------------
stone2Player WhiteStone = WhitePlayer
stone2Player BlackStone = BlackPlayer
stone2Player _          = error "stone2Player: Player of NoStone doesn't exist"

-- | Same like not for Bool
nextPlayer :: Player -> Player
-- ---------------------------------
nextPlayer BlackPlayer = WhitePlayer
nextPlayer WhitePlayer = BlackPlayer

-- | Coordinate within 'Board' consists of level (0 to size) and position within the level.
-- Size is a parameter that indicates number of tiles of the board, which is (size * size).
type Coordinate = (GLint,(GLint,GLint))

-- | Terminator terminates move.
terminator :: Coordinate
terminator = (-1,(0,0))

-- | Board is map of stones indexed by coordinates.
type Board = Map.Map Coordinate Stone

-- | 'Board' has two shadow-structures of following type.
--  * 1st of them called 'bases'.
--  * 2nd of them called 'successors'.
type IntMapBoard = Map.Map Coordinate GLint

-- | Any ball on the board increases nbr of balls of own row, colums and some number of 4 balls squares.
-- Any time ball is placed on or removed from the board, corresponding counters are increased or diminished.
-- This should accelerate taking decision whether move is good or bad.
type NbrOfBalls = Map.Map Coordinate GLint
-- ===================================

-- ===================================
-- | Aggregated status on the board. It contains 'Board' itself and additional data that accelerate analysis.
data Pylos = 
     Pylos { size       :: GLint        -- ^ size of game
           , board      :: Board        -- ^ board
           , bases      :: IntMapBoard  -- ^ Number of stones that constitute base of each stone,
                                        -- that have already been put onto board. If and only if this number = 4
                                        -- then field is putable. For instance all level 0 fields have this value always = 4.
           , successors :: IntMapBoard  -- ^ number of stones that covers each stone.
                                        -- 'Stone' can be removed from the 'Board' only if this number = 0.
           , rows       :: NbrOfBalls   -- ^ Indicator of content of each row. Row is full of white stones when this equals size of the game.
                                        -- It is full of black stones when equals (-size).
                                        -- Rows are indexed by (number of row,0) pair.
           , columns    :: NbrOfBalls   -- ^ Indicator of content of each column. Column is full of white stones when this equals size of the game.
                                        -- It is full of black stones when equals (-size).
                                        -- Columns are indexed by (0,number of column) pair.
           , squaresOf4 :: NbrOfBalls   -- ^ Indicator of content of each 4 balls square. It is full of white stones when this equals 4.
                                        -- It is full of black stones when equals (-4). What 4 balls square is? Each 4 tiles of
                                        -- the board that have 1 common vertex constitute 4 balls square.
                                        -- 4 balls squares are indexed by their upper left tile (or Nord-West if one prefers).
           , advance    :: IntMapBoard  -- ^ Each place on board gets own advance value. It is supposed to express expectation of being a good
                                        -- move by populating this place.
           }
   deriving (Eq,Read,Show)
-- ===================================
-- | Initial game situation. Input: size.
initPylos :: GLint -> Pylos
-- -----------------------------------
initPylos s = Pylos { size        = s,
                      board       = initBoard s,
                      bases       = basesOfFields s,
                      successors  = succesorsOfFields s,
                      rows        = Map.fromList [((level,(x,0)),0) | level <- [0 .. s-1], x <- [0 .. s-level-1] ] ,
                      columns     = Map.fromList [((level,(0,y)),0) | level <- [0 .. s-1], y <- [0 .. s-level-1] ] ,
                      squaresOf4  = Map.fromList [((level,(x,y)),0) | level <- [0 .. s-1], x <- [0 .. s-level-2], y <- [0 .. s-level-2] ] ,
                      advance     = Map.fromList $ (terminator,0):[((level,(x,y)),0) | level <- [0 .. s-1], x <- [0 .. s-level-1], y <- [0 .. s-level-1] ] }
-- ===================================

-- ===================================
-- Initial board. Input: size.
initBoard :: GLint -> Board
-- -----------------------------------
initBoard size = Map.fromList [((level,(x,y)),NoStone) | level<-[0..(size-1)],
                                                         x<-[0..(size-level-1)],
                                                         y<-[0..(size-level-1)]]
-- ===================================

-- ===================================
-- | Initial bases. Input: size.
basesOfFields :: GLint -> IntMapBoard
-- -----------------------------------
basesOfFields size = Map.fromList [((level,(x,y)),nbrBaseStones level) | level<-[0..(size-1)],
                                                                         x<-[0..(size-level-1)],
                                                                         y<-[0..(size-level-1)]]
           where nbrBaseStones 0 = 4
                 nbrBaseStones _ = 0
-- ===================================

-- ===================================
-- | Initial successors. Input: size.
succesorsOfFields :: GLint -> IntMapBoard
-- -----------------------------------
succesorsOfFields size = Map.fromList [((level,(x,y)),0) | level<-[0..(size-1)],
                                                           x<-[0..(size-level-1)],
                                                           y<-[0..(size-level-1)]]
-- ===================================

-- ===================================
-- | If field is free.
isFree :: Coordinate -> Pylos -> Bool
-- -----------------------------------
isFree c pylos = board pylos Map.! c == NoStone
-- ===================================

-- ===================================
-- | Is stone of given coordinates of given colour?
isMyColour :: Player -> Coordinate -> Pylos -> Bool
-- -----------------------------------
isMyColour player cor pylos = board pylos Map.! cor == player2Stone player
-- ===================================

-- ===================================
-- | Whether given field has all bases.
hasAllBases :: Coordinate -> Pylos -> Bool
-- -----------------------------------
hasAllBases c pylos = bases pylos Map.! c == 4
-- ===================================
-- ===================================
-- | Whether given field has no successors.
hasNoSuccessors :: Coordinate -> Pylos -> Bool
-- -----------------------------------
hasNoSuccessors c pylos = successors pylos Map.! c == 0
-- ===================================

-- ===================================
-- | Puts stone on the board on given coordinates. Uses 'Map.insert' function, so doesn't matter what was before value of given coordinates.
putStone :: Stone -> Coordinate -> Board -> Board
-- -----------------------------------
putStone stone coordinate board = Map.insert coordinate stone board
-- ===================================

-- ===================================
-- | Checks whether its allowed to put a ball on field given by coordinates.
putable :: Pylos -> Coordinate -> Bool
-- -----------------------------------
putable p c | hasAllBases c p && isFree c p = True
            | otherwise                     = False
-- ===================================

-- ===================================
-- | Removes stone from give filed. In other words, inserts 'NoStone' value.
takeStone :: Coordinate -> Board -> Board
-- -----------------------------------
takeStone coordinate board = Map.insert coordinate NoStone board
-- ===================================

-- ===================================
-- | Checks whether it's allowed to take given ball from board.
takeable :: Player -> Pylos -> Coordinate -> Bool
-- -----------------------------------
takeable pl py c | isMyColour pl c py && hasNoSuccessors c py = True
                 | otherwise                                  = False
-- ===================================

-- ===================================
-- | Checks whether it's allowed to take given ball from board and put it level above.
moveable :: Player -> Pylos -> Coordinate -> Bool
-- -----------------------------------
moveable pl pylos c@(l,(x,y)) = takeable pl pylos c && anyPutable (l+1) pylos'
         where pylos' = takeStoneAndUpdate c pylos
-- ===================================

-- ===================================
-- | Updates successor of given coordinate. This function works as well for bases.
updateSuccessor :: Stone -> Coordinate -> IntMapBoard -> IntMapBoard
-- -----------------------------------
updateSuccessor NoStone coordinate succesorsOfFields = Map.adjust (\x -> x-1) coordinate succesorsOfFields
updateSuccessor _       coordinate succesorsOfFields = Map.adjust (+ 1) coordinate succesorsOfFields
-- ===================================


-- ===================================
-- | Each time a stone is put or taken 4 (or 3 or 2 or 1) bases have to be updated.
--  Takes stone, coordinate, size, bases and returns new bases.
updateAllBases :: Stone -> Coordinate -> GLint -> IntMapBoard -> IntMapBoard
-- -----------------------------------
updateAllBases stone (level,(0,0)) size bases = updateSuccessor stone (level+1,(0,0)) bases
updateAllBases stone (level,(x,0)) size bases | x == size - 1 = updateSuccessor stone (level+1,(x-1,0)) bases
                                              | otherwise     = updateSuccessor stone (level+1,(x-1,0)) $
                                                                updateSuccessor stone (level+1,(x,0)) bases
updateAllBases stone (level,(0,y)) size bases | y == size - 1 = updateSuccessor stone (level+1,(0,y-1)) bases
                                              | otherwise     = updateSuccessor stone (level+1,(0,y-1)) $
                                                                updateSuccessor stone (level+1,(0,y)) bases
updateAllBases stone (level,(x,y)) size bases | x == size - 1 &&
                                                y == size - 1  = updateSuccessor stone (level+1,(x-1,y-1)) bases
                                              | x == size - 1  = updateSuccessor stone (level+1,(x-1,y)) $
                                                                 updateSuccessor stone (level+1,(x-1,y-1)) bases
                                              | y == size - 1  = updateSuccessor stone (level+1,(x,y-1)) $
                                                                 updateSuccessor stone (level+1,(x-1,y-1)) bases
                                              | otherwise      = updateSuccessor stone (level+1,(x-1,y-1)) $
                                                                 updateSuccessor stone (level+1,(x-1,y)) $
                                                                 updateSuccessor stone (level+1,(x,y-1)) $
                                                                 updateSuccessor stone (level+1,(x,y)) bases
-- ===================================


-- ===================================
-- | Each time a stone is put or taken 4 successors have to be updated.
-- Takes stone, coordinate, size, bases and returns new successors.
updateAllSuccessors :: Stone -> Coordinate -> GLint -> IntMapBoard -> IntMapBoard
-- -----------------------------------
updateAllSuccessors stone (0,(x,y))     size successors = successors
updateAllSuccessors stone (level,(x,y)) size successors = updateSuccessor stone (level-1,(x+1,y+1)) $ 
                                                          updateSuccessor stone (level-1,(x+1,y)) $ 
                                                          updateSuccessor stone (level-1,(x,y+1)) $ 
                                                          updateSuccessor stone (level-1,(x,y)) successors
-- ===================================

-- ===================================
-- | Updates number of balls of given row.
updateRow :: Stone
          -> Bool                     -- ^ True - stone is put, False - stone is being removed from the board.
          -> Coordinate
          -> GLint                    -- ^ size
          -> NbrOfBalls
          -> NbrOfBalls
-- -----------------------------------
updateRow WhiteStone True (level,(x,y)) size rows = Map.adjust (+ 1) (level,(x,0)) rows
updateRow BlackStone True (level,(x,y)) size rows = Map.adjust (\x -> x - 1) (level,(x,0)) rows
updateRow WhiteStone False (level,(x,y)) size rows = Map.adjust (\x -> x - 1) (level,(x,0)) rows
updateRow BlackStone False (level,(x,y)) size rows = Map.adjust (+ 1) (level,(x,0)) rows
-- ===================================

-- ===================================
-- | 2nd argument points on the action: True - stone is put, False - stone is being removed from the board.
-- 4th - size.
updateColumn :: Stone -> Bool -> Coordinate -> GLint -> NbrOfBalls -> NbrOfBalls
-- -----------------------------------
updateColumn WhiteStone True (level,(x,y)) size columns = Map.adjust (+ 1) (level,(0,y)) columns
updateColumn BlackStone True (level,(x,y)) size columns = Map.adjust (\x -> x - 1) (level,(0,y)) columns
updateColumn WhiteStone False (level,(x,y)) size columns = Map.adjust (\x -> x - 1) (level,(0,y)) columns
updateColumn BlackStone False (level,(x,y)) size columns = Map.adjust (+ 1) (level,(0,y)) columns
-- ===================================

-- ===================================
-- Coordinate of squareOf4 is its NE ball.
-- | 2nd argument points on the action: True - stone is put, False - stone is being removed from the board.
-- 4th - size.
updateSquaresOf4 :: Stone -> Bool -> Coordinate -> GLint -> NbrOfBalls -> NbrOfBalls
-- -----------------------------------
updateSquaresOf4 WhiteStone True coordinate size squaresOf4 = (updateSquareNE 1 coordinate size .
                                                               updateSquareNW 1 coordinate size .
                                                               updateSquareSE 1 coordinate size .
                                                               updateSquareSW 1 coordinate size) squaresOf4
updateSquaresOf4 BlackStone True coordinate size squaresOf4 = (updateSquareNE (-1) coordinate size .
                                                               updateSquareNW (-1) coordinate size .
                                                               updateSquareSE (-1) coordinate size .
                                                               updateSquareSW (-1) coordinate size) squaresOf4
updateSquaresOf4 BlackStone False coordinate size squaresOf4 = (updateSquareNE 1 coordinate size .
                                                                updateSquareNW 1 coordinate size .
                                                                updateSquareSE 1 coordinate size .
                                                                updateSquareSW 1 coordinate size) squaresOf4
updateSquaresOf4 WhiteStone False coordinate size squaresOf4 = (updateSquareNE (-1) coordinate size .
                                                                updateSquareNW (-1) coordinate size .
                                                                updateSquareSE (-1) coordinate size .
                                                                updateSquareSW (-1) coordinate size) squaresOf4
-- ===================================
updateSquareNE :: GLint -> Coordinate -> GLint -> NbrOfBalls -> NbrOfBalls
-- -----------------------------------
updateSquareNE one (level,(x,y)) size squaresOf4 | x > 0 && y > 0 = Map.adjust (+ one) (level,(x-1,y-1)) squaresOf4
                                                 | otherwise      = squaresOf4
-- ===================================
updateSquareNW :: GLint -> Coordinate -> GLint -> NbrOfBalls -> NbrOfBalls
-- -----------------------------------
updateSquareNW one (level,(x,y)) size squaresOf4 | x > 0 && y < size - level -1 = Map.adjust (+ one) (level,(x-1,y)) squaresOf4
                                                 | otherwise                    = squaresOf4
-- ===================================
updateSquareSE :: GLint -> Coordinate -> GLint -> NbrOfBalls -> NbrOfBalls
-- -----------------------------------
updateSquareSE one (level,(x,y)) size squaresOf4 | x < size - level -1 && y > 0 = Map.adjust (+ one) (level,(x,y-1)) squaresOf4
                                                 | otherwise                    = squaresOf4
-- ===================================
updateSquareSW :: GLint -> Coordinate -> GLint -> NbrOfBalls -> NbrOfBalls
-- -----------------------------------
updateSquareSW one (level,(x,y)) size squaresOf4 | x < size - level -1 && y < size - level -1 = Map.adjust (+ one) (level,(x,y)) squaresOf4
                                                 | otherwise                                  = squaresOf4
-- ===================================

-- ===================================
updateAllAdvances :: Stone -> Coordinate -> GLint -> GLint -> IntMapBoard -> IntMapBoard
-- -----------------------------------
updateAllAdvances stone (l,(x,y)) one size = updateAdvances (valueOfStone stone) keysS .
                                             updateAdvances (valueOfStone stone) keysR .
                                             updateAdvances (valueOfStone stone) keysC
                  where keysR = [(l,(x,y')) | y' <- [0 .. size-l-1], y' /= y ]                  -- update all of given row
                        keysC = [(l,(x',y)) | x' <- [0 .. size-l-1], x' /= x ]                  -- update all of given column
                        keysS = [(l,(x',y')) | x' <- [(max 0 (x-1)) .. (min (size-l-1) (x+1))], -- update all of all squaresOf4
                                               y' <- [(max 0 (y-1)) .. (min (size-l-1) (y+1))], x' /= x || y' /= y]
                        valueOfStone WhiteStone = 1 * one
                        valueOfStone BlackStone = -1 * one
                        valueOfStone _          = 0
-- ===================================
updateAdvances :: GLint -> [Coordinate] -> IntMapBoard -> IntMapBoard
-- -----------------------------------
updateAdvances one keys advance = foldl (\adv key -> Map.adjust (+ one) key adv) advance keys
-- ===================================

-- ===================================
-- | If there is any puttable filed on the board.
-- Takes level and current game situation.
anyPutable :: GLint -> Pylos -> Bool
-- -----------------------------------
anyPutable level pylos = any (putable pylos) keys
            where keys = [(l,(x,y)) | l<-[level..s-1], x <- [0..s-l-1], y<-[0..s-l-1]]
                  s = size pylos
-- ===================================

-- ===================================
-- | 
-- Whether ball can be put to given position within the move turn.
putableForMove :: GLint -> Coordinate -> Pylos -> Bool
-- -----------------------------------
putableForMove level c@(l,(x,y)) pylos = l > level && putable pylos c
-- ===================================

-- ===================================
-- | If there is any takeable field on the board.
-- Takes stone to be taken and current game situation.
anyTakeable :: Player -> Pylos -> Bool
-- -----------------------------------
anyTakeable player pylos = any (takeable player pylos) keys
            where keys = [(l,(x,y)) | l<-[0..s-1], x <- [0..s-l-1], y<-[0..s-l-1]]
                  s = size pylos
-- ===================================

-- ===================================
-- |
-- Does only possible action on given field.
-- Gets stone colour which depends on what playes is on move, coordinates and game status.
-- Updates the whole status of Pylos structure.
moveOnCoordinate :: Player -> Coordinate -> Pylos -> Pylos
-- -----------------------------------
moveOnCoordinate player coordinate pylos | coordinate == terminator         = pylos
                                         | putable pylos coordinate         = putStoneAndUpdate stone coordinate pylos
                                         | takeable player pylos coordinate = takeStoneAndUpdate coordinate pylos
                                         | otherwise                        = pylos
                                         where stone = player2Stone player
-- ===================================

-- ===================================
-- |
-- Updates whole 'Pylos' structure after stone has been put.
putStoneAndUpdate :: Stone -> Coordinate -> Pylos -> Pylos
-- -----------------------------------
putStoneAndUpdate stone c@(l,(x,y)) pylos = Pylos { size       = s,
                                                    board      = putStone stone c (board pylos),
                                                    bases      = updateAllBases stone c s (bases pylos),
                                                    successors = updateAllSuccessors stone c s (successors pylos),
                                                    rows       = updateRow stone True c s (rows pylos),
                                                    columns    = updateColumn stone True c s (columns pylos),
                                                    squaresOf4 = updateSquaresOf4 stone True c s (squaresOf4 pylos),
                                                    advance    = updateAllAdvances stone c 1 s (advance pylos)
                                                    }
                                        where s = size pylos
-- ===================================

-- ===================================
-- |
-- Updates whole 'Pylos' structure after stone has been taken.
takeStoneAndUpdate :: Coordinate -> Pylos -> Pylos
-- -----------------------------------
takeStoneAndUpdate c@(l,(x,y)) pylos = Pylos { size       = s,
                                               board      = takeStone c (board pylos),
                                               bases      = updateAllBases NoStone c s (bases pylos),
                                               successors = updateAllSuccessors NoStone c s (successors pylos),
                                               rows       = updateRow stone False c s (rows pylos),
                                               columns    = updateColumn stone False c s (columns pylos),
                                               squaresOf4 = updateSquaresOf4 stone False c s (squaresOf4 pylos),
                                               advance    = updateAllAdvances stone c (-1) s (advance pylos)
                                             }
                                        where s = size pylos
                                              stone = board pylos Map.! c
-- ===================================

-- ===================================
-- |
-- Checks whether player can take balls after he||she puts one.
ifTakeAfterPut :: Pylos -> Player -> Coordinate -> Bool
-- -----------------------------------
ifTakeAfterPut pylos player c = isInFullCol player c (columns pylos) s ||           -- anyRowOrColFull s player (columns pylos) ||
                                isInFullRow player c (rows pylos) s    ||           -- anyRowOrColFull s player (rows pylos) ||
                                isInFullSquareOf4 player c (squaresOf4 pylos) s    -- anySquareOf4Full player (squaresOf4 pylos)
               where s = size pylos
-- ===================================

-- ===================================
isInFullRow :: Player -> Coordinate -> NbrOfBalls -> GLint -> Bool
-- -----------------------------------
isInFullRow player c@(l,(x,y)) row size = row Map.! (l,(x,0)) == value player
          where value WhitePlayer = size
                value _           = -size
-- ===================================

-- ===================================
isInFullCol :: Player -> Coordinate -> NbrOfBalls -> GLint -> Bool
-- -----------------------------------
isInFullCol player c@(l,(x,y)) col size = col Map.! (l,(0,y)) == value player
          where value WhitePlayer = size
                value _           = -size
-- ===================================

-- ===================================
ifFullSquareNE :: GLint -> Coordinate -> NbrOfBalls -> GLint -> Bool
-- -----------------------------------
ifFullSquareNE value (level,(x,y)) squaresOf4 size | x > 0 && y > 0 = squaresOf4 Map.! (level,(x-1,y-1)) == value 
                                                   | otherwise      = False
-- ===================================
ifFullSquareNW :: GLint -> Coordinate -> NbrOfBalls -> GLint -> Bool
-- -----------------------------------
ifFullSquareNW value (level,(x,y)) squaresOf4 size | x > 0 && y < size - level -1 = squaresOf4 Map.! (level,(x-1,y)) == value 
                                                   | otherwise                    = False
-- ===================================
ifFullSquareSE :: GLint -> Coordinate -> NbrOfBalls -> GLint -> Bool
-- -----------------------------------
ifFullSquareSE value (level,(x,y)) squaresOf4 size | x < size - level -1 && y > 0 = squaresOf4 Map.! (level,(x,y-1)) == value 
                                                   | otherwise                    = False
-- ===================================
ifFullSquareSW :: GLint -> Coordinate -> NbrOfBalls -> GLint -> Bool
-- -----------------------------------
ifFullSquareSW value (level,(x,y)) squaresOf4 size | x < size - level -1 && y < size - level -1 = squaresOf4 Map.! (level,(x,y)) == value
                                                   | otherwise                                  = False
-- ===================================

-- ===================================
isInFullSquareOf4 :: Player -> Coordinate -> NbrOfBalls -> GLint -> Bool
-- -----------------------------------
isInFullSquareOf4 player c squaresOf4 size = ifFullSquareNE (value player) c squaresOf4 size ||
                                      ifFullSquareNW (value player) c squaresOf4 size ||
                                      ifFullSquareSE (value player) c squaresOf4 size ||
                                      ifFullSquareSW (value player) c squaresOf4 size
          where value WhitePlayer = 4
                value _           = -4
-- ===================================

-- =========================================================
-- |
-- Returns total number of balls for given size of the game.
nbrOfBalls :: (Enum a, Num a) => a -> a
-- ---------------------------------------------------------
nbrOfBalls size = sum $ map (\x->x*x) [1..size]
-- =========================================================
