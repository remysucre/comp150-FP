-- ==================================
-- Module name: hpylos
-- Project: Foo
-- Copyright (C) 2008  Bartosz Wójcik
-- Created on: 24.10.2008
-- Last update: 19.11.2008
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
module Main where

-- Simply user interface reading parameters from file or
-- aksing for initial game parameters if parameter file doesn't exist.

import PylosDisplay (displayPylos)
import PylosAI (Algorithm (..))
import EEConfig (ParameterInput,
                  ParameterOutput,
                  ParameterTree,
                  matchParamsL,
                  matchParamsT,
                  (!),
                  member)
import PylosEvaluator (PlayerType,
                       State (..),
                       evaluatePylos)
import PylosBoard (Pylos (..))
import System.IO.Error
import Data.IORef


main = do
     inputText <- catch (readFile "pylos.conf") (\e -> if isEOFError e || isDoesNotExistError e then return [] else ioError e)
     let paramsList = matchParamsL params inputText
     let paramsTree = matchParamsT paramsList
     if evaluatorAll paramsTree
        then evaluateAll (setVerbosity paramsTree)
        else do
             alg1 <- findAlgorithm 1 paramsTree paramsList
             alg2 <- findAlgorithm 2 paramsTree paramsList
             if evaluatorOnly paramsTree
                then evaluatePylos 4 alg1 alg2 (setVerbosity paramsTree) >>= \x -> return ()
                else displayPylos 4 alg1 alg2 (setVerbosity paramsTree)


-- =========================================================
evaluatorOnly :: ParameterTree -> Bool
evaluatorOnly paramsTree | "-evaluator" `member` paramsTree && paramsTree ! "-evaluator" == "only" = True
                         | otherwise                                                               = False
-- =========================================================

-- =========================================================
evaluatorAll :: ParameterTree -> Bool
evaluatorAll paramsTree | "-evaluator" `member` paramsTree && paramsTree ! "-evaluator" == "all" = True
                        | otherwise                                                              = False
-- =========================================================

-- =========================================================
allPlayers :: [PlayerType]
-- allPlayers = [Just (alg,depth,breadth,nbr) | alg <- [SimpleDiff ..], depth <- [1 .. 6], breadth <- [0,11,13,16], nbr <- 0:[depth + 2 .. 6]]
--pylos4 allPlayers = [Just (alg,depth,breadth,nbr) | alg <- [SimpleDiff .. SimpleDiffPlusVal], depth <- [4 .. 6], breadth <- [0,11], nbr <- 0:[depth .. 6]]
allPlayers = [Just (alg,depth,breadth,nbr) | alg <- [SimpleDiff .. MixedDiff], depth <- [6], breadth <- [0], nbr <- [0,7]]
-- =========================================================

-- =========================================================
evaluateAll :: Int -> IO ()
evaluateAll verb = mapM_ (\pl1 -> mapM_ (\pl2 -> evaluatePylos 4 pl1 pl2 verb >>= \x -> printResult x) allPlayers) allPlayers
-- =========================================================

-- =========================================================
printResult :: State -> IO ()
printResult state = do
            (w,b) <- readIORef $ nbrBalls state
            pl1 <- readIORef $ player1 state
            pl2 <- readIORef $ player2 state
            putStrLn $ show pl1 ++ ";" ++ resultW (w,b) ++ ";" ++ show w ++ " ; " ++
                       show pl2 ++ ";" ++ resultB (w,b) ++ ";" ++ show b
-- =========================================================

-- =========================================================
resultW :: (Int,Int) -> String
resultW (0,b) = "0"
resultW (w,0) = "2"
resultW _     = "1"
-- =========================================================
resultB :: (Int,Int) -> String
resultB (0,b) = "2"
resultB (w,0) = "0"
resultB _     = "1"
-- =========================================================

-- =========================================================
paramToAlg :: String -> (Maybe (Algorithm,Int))
paramToAlg "h" = Nothing
paramToAlg alg = Just ((toEnum . (\n -> n - fromEnum 'a') . fromEnum . head) alg,(read . tail) alg)
-- =========================================================

-- =========================================================
params :: [ParameterInput]
params = [("-pl1",algNum),("-pl2",algNum),
          ("-br1","0":(map show [5..30])),("-br2","0":(map show [5..30])),
          ("-dp1","0":(map show [2..8])),("-dp2","0":(map show [2..8])),
          ("-verbose",["0","1","2","3"]),
          ("-evaluator",["only","all"])]
-- =========================================================

-- =========================================================
algDisp :: String -> [String]
algDisp text = [text] ++ ["Human - h"] ++ map (\(a,n) -> (show a ++ " - " ++ [n] ++ "1 .. " ++ [n] ++ "8")) (zip [SimpleDiff ..] ['a'..])
               ++ ["Or select just level 0-9"]
-- =========================================================

-- =========================================================
-- | There are 6 levels of search depth hardcoded.
algNum :: [String]
algNum = "h" : (concat $ map (\(a,x) -> map (\n -> x:n:[]) ['1'..'8']) (zip [SimpleDiff ..] ['a'..])) ++ map show [0..9]
-- =========================================================

-- =========================================================
enterValue :: [String] -> [String] -> IO String
enterValue possibleValues messages = do
   mapM putStrLn messages
   word <- getLine
   if any (word ==) possibleValues
      then return word
      else enterValue possibleValues messages
-- =========================================================

-- =========================================================
-- |
-- Always selects presorted search tree
selectAlgorithm :: String -> IO PlayerType
selectAlgorithm text = do
      alg <- enterValue algNum (algDisp text)   -- >>= (\x -> return (read x))
      if head alg == 'h'
         then return Nothing
         else return $ selAlg alg (Just (SimpleDiff,0,0,0)) --(Just ((toEnum . (\n -> n - fromEnum 'a') . fromEnum . head) alg, (read . tail) alg, 0, 0))
-- =========================================================

-- =========================================================
-- | selects algorithm from pylos.conf file, if not exists from stdin
findAlgorithm :: Int -> ParameterTree -> [ParameterOutput] -> IO PlayerType
findAlgorithm 1 paramsTree paramList | "-pl1" `member` paramsTree = return $ findParam1 paramList -- paramToAlg (paramsTree Map.! param)
                                     | otherwise                  = selectAlgorithm "Select player 1"
findAlgorithm 2 paramsTree paramList | "-pl2" `member` paramsTree = return $ findParam2 paramList
                                     | otherwise                  = selectAlgorithm "Select player 2"
-- =========================================================

-- =========================================================
findParam1 :: [ParameterOutput] -> PlayerType
findParam1 input = foldl fParam1 defaultParam input
-- =========================================================
findParam2 input = foldl fParam2 defaultParam input
-- =========================================================

-- =========================================================
defaultParam :: PlayerType
defaultParam = Just (toEnum 0, 2, 0, 0)
-- =========================================================

-- =========================================================
fParam1 :: PlayerType -> ParameterOutput -> PlayerType
fParam1 _ ("-pl1","h") = Nothing
fParam1 (Just (a,b,c,d)) ("-pl1",val) = selAlg val (Just (a,b,c,d))
fParam1 (Just (a,b,c,d)) ("-br1",val) = Just (a,b,read val,d)
fParam1 (Just (a,b,c,d)) ("-dp1",val) = Just (a,b,c,read val)
fParam1 pl _ = pl
-- =========================================================
fParam2 :: PlayerType -> ParameterOutput -> PlayerType
fParam2 _ ("-pl2","h") = Nothing
fParam2 (Just (a,b,c,d)) ("-pl2",val) = selAlg val (Just (a,b,c,d))
fParam2 (Just (a,b,c,d)) ("-br2",val) = Just (a,b,read val,d)
fParam2 (Just (a,b,c,d)) ("-dp2",val) = Just (a,b,c,read val)
fParam2 pl _ = pl
-- =========================================================

-- =========================================================
selAlg :: String -> PlayerType -> PlayerType
selAlg "0" _ = Just (SimpleDiff,1,0,0)
selAlg "1" _ = Just (SimpleDiffAdvance,1,0,0)
selAlg "2" _ = Just (SimpleDiffAdvance,2,0,0)
selAlg "3" _ = Just (SimpleDiffAdvance,3,0,0)
selAlg "4" _ = Just (SimpleDiffAdvance,4,0,0)
selAlg "5" _ = Just (SimpleDiffAdvance,5,0,0)
selAlg "6" _ = Just (SimpleDiffAdvance,6,0,0)
selAlg "7" _ = Just (SimpleDiffAdvance,6,0,7)
selAlg "8" _ = Just (SimpleDiffAdvance,7,0,0)
selAlg "9" _ = Just (SimpleDiffAdvance,8,0,0)
selAlg val (Just (_,_,c,d)) = Just ((toEnum . (\n -> n - fromEnum 'a') . fromEnum . head) val, (read . tail) val, c, d)
-- =========================================================

-- =========================================================
setVerbosity :: ParameterTree -> Int
setVerbosity paramsTree | "-verbose" `member` paramsTree = read $ paramsTree ! "-verbose"
                        | otherwise                      = 1
-- =========================================================
