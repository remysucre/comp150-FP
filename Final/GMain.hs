{-# OPTIONS -Wall -Werror -fno-warn-name-shadowing #-}

module Main
where

import Genetic

import Debug.Trace

main :: IO ()
main = do 
          def <- createGeneFromFile filePath
          _ <- compile $ path $ head $ getStrand def
          print "Obtaining base time of program"
          !time <- fitness fitnessRuns (fromInteger . toInteger $ (maxBound :: Int)) def
          print $ "Base time is " ++ (show time)
          dnas <- trace (show time) $ geneticAlg [def] runs time fitnessRuns poolSize ((GR def time), 0) emptyGeneDict
          print $ "Best found: " ++ (show $ getStrand $ head dnas)
        where
           filePath = "files.txt"
           runs = 50
           fitnessRuns = 5
           poolSize = 5
