{-# OPTIONS -Wall -Werror -fno-warn-name-shadowing -XBangPatterns #-}

module Main
where

import Genetic
import System.Directory (createDirectoryIfMissing)
import Debug.Trace

main :: IO ()
main = do 
          _ <- createDirectoryIfMissing True "files"
          def <- createGeneFromFile filePath
	  writeGeneToDisk def
	  
          _ <- compile $ path $ head $ getStrand def
          print "Obtaining base time of program"
          !time' <- fitness fitnessRuns (fromInteger . toInteger $ (maxBound :: Int)) def
          time <- return $ if time' < 0.0 then (fromInteger . toInteger $ (maxBound :: Int)) else time'
          print $ "Base time is " ++ (show time)
          dnas <- trace (show time) $ geneticAlg [def] runs time fitnessRuns poolSize ((GR def time), 0) emptyGeneDict
          print $ "Best found: " ++ (show $ getStrand $ head dnas)
        where
           filePath = "files.txt"
           runs = 50
           fitnessRuns = 5
           poolSize = 5
