-- ==================================
-- Module name: PylosEvaluator
-- Project: Pylos
-- Copyright (C) 2008  Bartosz Wójcik
-- Created on: 07.11.2008
-- Last update: 07.11.2008
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
-- | This module is a simple interface to Pylos game <http://www.boardgamegeek.com/game/1419>
-- It uses AI module in order to follow game status or provide game between human and computer.
module PylosEvaluator (Action (..),
                       TileStatus (..),
                       Actions,
                       BoardState,
                       BallsState,
                       PlayerType,
                       State (..),
                       algPlayerType,
                       depPlayerType,
                       brePlayerType,
                       nbrPlayerType,
                       makeState,
                       stateProcessMain,
                       actionTerminate,
                       alreadyFinishedCase,
                       stateProcessSecondary,
                       stateProcess,
                       updateNbrBalls,
                       aiOnMove,
                       evaluatePylos
                      )

where

-- =========================================================
import Graphics.Rendering.OpenGL (GLint, 
                                  Position, 
                                  ($=))
import Data.Array ( Array, listArray, array, elems, (!) )
import Data.IORef
import Data.List
import PylosBoard (Pylos (..),
                   Coordinate,
                   Stone (..),
                   NbrOfBalls,
                   Player (..),
                   putable,
                   takeable,
                   moveable,
                   initPylos,
                   moveOnCoordinate,
                   anyTakeable,
                   ifTakeAfterPut,
                   nextPlayer,
                   nbrOfBalls,
                   terminator
                   )
import PylosMove (Move (NextMove))
import PylosAI (Algorithm (..),
                evaluate,
                maximise,
                minimise,
                sizeGT
               )
-- =========================================================
data Action = Puts              -- ^ player has to put ball on the higher level he/she's it just taken from.
            | PutsOrTakes       -- ^ player either takes a takeable ball or puts one.
            | PutsAfterTake2    -- ^ same like Puts, but keeps info, that there is one more ball to be taken
            | Takes2            -- ^ player can take 2 balls from board
            | Takes1            -- ^ player can take 1 ball from board
   deriving (Eq,Ord,Enum,Read,Show)

-- |
-- Board is displayed as square matrix of tiles. Each tile can be in either of following statuses.
data TileStatus = Tile
                | PointedTile
                | PickedTile
   deriving (Eq,Ord,Enum,Show)

-- |
-- Actions are status of game's move. Users are on move one after another and they have different types of moves that can be done.
type Actions = (Player,Action)

-- |
-- The 3 dimentions of board array states for:
-- 1st dimention is level of object: from -1 (tile) till size - 1 (0 - size-1 - ball)
-- 2nd and 3rd dimentions are coordinates within current level
type BoardState = Array (GLint,GLint,GLint) (IORef TileStatus)

type BallsState = IORef Pylos

type PlayerType = Maybe (Algorithm,    -- Playing algorithm
                         Int,          -- Depth of search tree
                         Int,          -- Max number of subtrees, or unlimited if 0
                         Int           -- Extend tree of search until this value if possible.
                        )

algPlayerType :: PlayerType -> Algorithm
algPlayerType (Just (algorithm,_,_,_)) = algorithm
algPlayerType _                        = error "alg (Nothing)"
depPlayerType (Just (_,depth,_,_)) = depth
depPlayerType _                    = error "dep (Nothing)"
brePlayerType (Just (_,_,breadth,_)) = breadth
brePlayerType _                      = error "bre (Nothing)"
nbrPlayerType (Just (_,_,_,nbr)) = nbr
nbrPlayerType _                  = error "nbr (Nothing)"

-- |
-- State states for current state of the game.
-- It is used for display, reaction analysis and game stearing purposes.
data State = State {sboard      :: BoardState,
                    balls       :: BallsState,
                    aBall       :: IORef Coordinate,                    -- ^ recently activated ball
                    rBall       :: IORef (Coordinate, Coordinate),      -- ^ recently removed balls
                    sBall       :: IORef (Maybe Coordinate),            -- ^ selected ball
                    pBall       :: IORef (Maybe Coordinate),            -- ^ mouse cursor pointed ball
                    action      :: IORef Actions,                       -- ^ action on the board
                    leftButton  :: IORef (Maybe Position),              -- ^ to turn a board around of axes
                    player1     :: IORef PlayerType,                    -- ^ Nothing => human plays
                    player2     :: IORef PlayerType,                    -- ^ Nothing => human plays
                    nbrBalls    :: IORef (Int,Int),                     -- ^ Number of balls of both players being not yet on board. Fst for white player.
                    dispStat    :: IORef Bool,                          -- ^ Status to first display balls after human move then evaluate next AI move.
                    verbose     :: IORef Int,                           -- ^ Verbosity level
                    moveNbr     :: IORef Int,                           -- ^ Number of next move
                    history     :: IORef [[Coordinate]]                 -- ^ List of done moves
                    }
-- =========================================================

-- =========================================================
-- | Creates initial status with empty board. White is on move.
makeState :: GLint -> PlayerType -> PlayerType -> Int -> IO State
-- ---------------------------------------------------------
makeState size pl1 pl2 verb = do
   boardRefs <- sequence $ (replicate (fromIntegral $ size^2) . newIORef $ Tile)
   ballRefs <- newIORef $ initPylos size
   aB <- newIORef (0,(0,0))
   rB <- newIORef ((-1,(0,0)),(-1,(0,0)))
   pB <- newIORef Nothing
   sB <- newIORef Nothing
   act <- newIORef (WhitePlayer,Puts)
   lButton <- newIORef Nothing
   p1 <- newIORef pl1
   p2 <- newIORef pl2
   nbrB <- newIORef $ (ceiling halfOfBalls, truncate halfOfBalls)
   dStat <-  newIORef False
   v <- newIORef verb
   mvN <- newIORef 1
   hist <- newIORef []
   return $ State { sboard = listArray ((0,0,0),(0,size-1,size-1)) boardRefs,
                    balls  = ballRefs,
                    aBall  = aB,
                    sBall  = sB,
                    pBall  = pB,
                    rBall  = rB,
                    action = act,
                    leftButton = lButton,
                    player1 = p1,
                    player2 = p2,
                    nbrBalls = nbrB,
                    dispStat = dStat,
                    verbose = v,
                    moveNbr = mvN,
                    history = hist
                   }
   where halfOfBalls = (fromIntegral.nbrOfBalls) size / 2
-- =========================================================

-- =========================================================
-- |
-- This "function" initiates game, opens window, and passes stearing to 'Graphics.Rendering.OpenGL.mainLoop'.
evaluatePylos :: GLint -> PlayerType -> PlayerType -> Int -> IO State
-- ---------------------------------------------------------
evaluatePylos size pl1 pl2 verbose = do
-- =========================================================
   state <- makeState size pl1 pl2 verbose
   untilM_ gameFinished evaluateNextMove state
   return state
-- =========================================================

-- =========================================================
-- | monadic version of until function
untilM_ :: (a -> IO Bool) -> (a -> IO ()) -> a -> IO ()
-- ---------------------------------------------------------
untilM_ conditionIO f value = do
        condition <- conditionIO value
        if condition
           then return ()
           else f value >> untilM_ conditionIO f value
-- =========================================================

-- =========================================================
-- | Next move evaluation with assumption both players are computer.
evaluateNextMove :: State -> IO ()
-- ---------------------------------------------------------
evaluateNextMove state = dispStat state $= True >> evaluateMove state
-- =========================================================

-- =========================================================
-- | Recognition of end of game (also tie is recognized).
gameFinished :: State -> IO Bool
-- ---------------------------------------------------------
gameFinished state = do
             (w,b) <- readIORef $ nbrBalls state
             hist <- readIORef $ history state
             mvN <- readIORef $ moveNbr state
             if w == 0  || b == 0 || checkLoop hist || mvN > 300
                then return True
                else return False
-- =========================================================

-- =========================================================
checkLoop :: [[Coordinate]] -> Bool
-- ---------------------------------------------------------
checkLoop (a:b:c:d:e:f:g:h:i:j:k:l:a':b':c':d':e':f':g':h':i':j':k':l':ls) = l == l' && k == k' && j == j' && i == i' && h == h'
checkLoop (a:b:c:d:e:f:g:h:i:j:a':b':c':d':e':f':g':h':i':j':ls) = h == h' && g == g' && j == j' && i == i'
checkLoop (a:b:c:d:e:f:g:h:a':b':c':d':e':f':g':h':ls) = h == h' && g == g' && f == f' && d == d' && e == e'
checkLoop (a:b:c:d:e:f:a':b':c':d':e':f':ls) = b == b' && c == c' && f == f' && d == d' && e == e'
checkLoop (a:b:c:d:e:f:g:h:ls) = a == e && b == f && c == g && d == h
checkLoop (a:b:c:d:ls) = a == c && b == d
checkLoop _            = False
-- =========================================================

-- =========================================================
stateProcessMain :: State -> Coordinate -> IO ()
-- ---------------------------------------------------------
stateProcessMain state c = do
   act <- readIORef $ action state
   verb <- readIORef $ verbose state
   dispStat state $= False
   if c == terminator
      then do
           (player,act) <- readIORef $ action state
           if act == PutsOrTakes
              then if verb > 1
                   then putStrLn "terminator: PutsOrTakes"
                   else return ()
              else actionTerminate state (player,act)
      else stateProcess act state c
   aiOnM <- aiOnMove state
   if aiOnM
      then pBall state $= Nothing
      else return ()
   if verb > 0
      then (readIORef $ nbrBalls state) >>= \n -> (putStrLn $ show n)
      else return ()
   alreadyFinishedCase state
-- =========================================================

-- =========================================================
actionTerminate :: State -> Actions -> IO ()
-- ---------------------------------------------------------
actionTerminate state (player,act) = do 
                action state $= (nextPlayer player,PutsOrTakes)
                verb <- readIORef $ verbose state
                if verb > 1
                   then putStrLn ("terminator: " ++ show (nextPlayer player) ++ "'s turn")
                   else return ()
-- =========================================================

-- =========================================================
-- |
-- Below action controlls if one of players hasn't lost already.
-- If yes, the opposite one can continue.
-- Situation when user put last ball but can take one is not already lost situation.
alreadyFinishedCase :: State -> IO ()
-- ---------------------------------------------------------
alreadyFinishedCase state = do
   (w,b) <- readIORef $ nbrBalls state
   (player,act) <- readIORef $ action state
   if w == 0 && b > 0 && player == WhitePlayer && act == PutsOrTakes ||
      w > 0 && b == 0 && player == BlackPlayer && act == PutsOrTakes
      then action state $= (nextPlayer player,act)
      else return ()
-- =========================================================

-- =========================================================
-- | This function process termination and ball on 1st level selection.
stateProcessSecondary :: State -> Coordinate -> IO ()
-- ---------------------------------------------------------
stateProcessSecondary state c = do
   (player,act) <- readIORef $ action state
   pyl <- readIORef $ balls state
   if putable pyl c
      then stateProcess (player,act) state c
      else if act == Takes2 || act == Takes1
           then actionTerminate state (player,act)
           else return ()
   alreadyFinishedCase state
-- =========================================================

-- =========================================================
stateProcess :: Actions -> State -> Coordinate -> IO ()
-- ---------------------------------------------------------
stateProcess (player,Puts) state c = do
             pyl <- readIORef $ balls state
             if putable pyl c
                then do
                   let pyl' = moveOnCoordinate player c pyl
                   balls state $= pyl'
                   aBall state $= c
                   rB <- readIORef $ rBall state
                   (readIORef $ nbrBalls state) >>= \x -> nbrBalls state $= updateNbrBalls x (player,Puts)
                   if c == fst rB || c == snd rB
                      then rBall state $= ((-1,(0,0)),(-1,(0,0)))
                      else return ()
                   if ifTakeAfterPut pyl' player c && anyTakeable player pyl'
                      then action state $= (player,Takes2)                  -- putable
                      else do
                         action state $= (nextPlayer player,PutsOrTakes)
                         (readIORef $ moveNbr state) >>= \n -> (moveNbr state $= n + 1)
                else do
                   putStrLn $ "@@ " ++ show c
                   error "stateProcess: Puts on not putable field"
stateProcess (player,PutsOrTakes) state c@(l,(x,y)) = do
             pyl <- readIORef $ balls state
             if moveable player pyl c
                then do
                   let pyl' = moveOnCoordinate player c pyl
                   balls state $= pyl'
                   aBall state $= (-1,(0,0))
                   rBall state $= (c,(-1,(0,0)))
                   action state $= (player,Puts)
                   (readIORef $ nbrBalls state) >>= \x -> nbrBalls state $= updateNbrBalls x (player,Takes1)
                else stateProcess (player,Puts) state c
stateProcess (player,Takes2) state c@(l,(x,y)) = do
             pyl <- readIORef $ balls state
             if takeable player pyl c
                then do
                   let pyl' = moveOnCoordinate player c pyl
                   balls state $= pyl'
                   rBall state $= (c,(-1,(0,0)))
                   (readIORef $ nbrBalls state) >>= \x -> nbrBalls state $= updateNbrBalls x (player,Takes2)
                   if anyTakeable player pyl'
                      then action state $= (player,Takes1)
                      else action state $= (nextPlayer player,PutsOrTakes) >>
                           (readIORef $ moveNbr state) >>= \n -> (moveNbr state $= n + 1)

                -- Click on empty tile - nothing happens
                else return () -- actionTerminate state (player,Takes2)
stateProcess (player,Takes1) state c@(l,(x,y)) = do
             pyl <- readIORef $ balls state
             if takeable player pyl c
                then do
                   let pyl' = moveOnCoordinate player c pyl
                   (rB1,rB2) <- readIORef $ rBall state
                   balls state $= pyl'
                   rBall state $= (rB1,c)
                   aBall state $= (-1,(0,0))
                   (readIORef $ nbrBalls state) >>= \x -> nbrBalls state $= updateNbrBalls x (player,Takes1)
                   action state $= (nextPlayer player,PutsOrTakes)
                   (readIORef $ moveNbr state) >>= \n -> (moveNbr state $= n + 1)
                -- Click on empty tile - player resigns and doesn't take 2nd ball
                else actionTerminate state (player,Takes1)

stateProcess _ _ _ = return ()
-- =========================================================

-- =========================================================
updateNbrBalls :: (Int,Int) -> Actions -> (Int,Int)
-- ---------------------------------------------------------
updateNbrBalls (w,b) (WhitePlayer,Puts) = (w - 1,b)
updateNbrBalls (w,b) (BlackPlayer,Puts) = (w,b - 1)
updateNbrBalls (w,b) (WhitePlayer,_)    = (w + 1,b)
updateNbrBalls (w,b) (BlackPlayer,_)    = (w,b + 1)
-- =========================================================


-- =========================================================
evaluateMove :: State -> IO ()
-- ---------------------------------------------------------
evaluateMove state = do
     (player,act) <- readIORef $ action state
     pyl <-readIORef $ balls state
     pl1 <- readIORef $ player1 state
     pl2 <- readIORef $ player2 state
     nbrB <- readIORef $ nbrBalls state
     displayed <- readIORef $ dispStat state
     verb <- readIORef $ verbose state
     mvNbr <- readIORef $ moveNbr state
     if displayed && player == WhitePlayer && not (pl1 == Nothing) && (not $ nbrB == (0,0))
        then do
           let evWhite = evaluate (algPlayerType pl1) (depPlayerType pl1) (nbrPlayerType pl1) maximise (brePlayerType pl1) (NextMove pyl player nbrB [])
           if verb > 0
              then putStrLn $ show mvNbr ++ ". WhitePlayer: " ++ show evWhite
              else return ()
           mapM_ (stateProcessMain state) (reverse evWhite)
           (readIORef $ history state) >>= \moves -> history state $= evWhite:moves
           if verb > 1
              then putStrLn $ show (sizeGT (depPlayerType pl1) (nbrPlayerType pl1) (brePlayerType pl1) (NextMove pyl player nbrB []))
              else return ()
        else if displayed && player == BlackPlayer && not (pl2 == Nothing) && (not $ nbrB == (0,0))
           then do
              let evBlack = evaluate (algPlayerType pl2) (depPlayerType pl2) (nbrPlayerType pl2) minimise (brePlayerType pl2) (NextMove pyl player nbrB [])
              if verb > 0
                 then putStrLn $ show mvNbr ++ ". BlackPlayer: " ++ show evBlack
                 else return ()
              mapM_ (stateProcessMain state) (reverse evBlack)
              (readIORef $ history state) >>= \moves -> history state $= evBlack:moves
              if verb > 1
                 then putStrLn $ show (sizeGT (depPlayerType pl2) (nbrPlayerType pl2) (brePlayerType pl2) (NextMove pyl player nbrB []))
                 else return ()
           else return ()
-- =========================================================

-- =========================================================
aiOnMove :: State -> IO Bool
-- ---------------------------------------------------------
aiOnMove state = do
     (player,act) <- readIORef $ action state
     pl1 <- readIORef $ player1 state
     pl2 <- readIORef $ player2 state
     nbrB <- readIORef $ nbrBalls state
     if (not $ nbrB == (0,0)) &&
        (player == WhitePlayer && not (pl1 == Nothing) ||
         player == BlackPlayer && not (pl2 == Nothing))
        then return True
        else return False
-- =========================================================

