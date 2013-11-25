module Genetic
where

import System.IO
import Rewrite
import System.Process
import System.Exit
import System.Posix.Time
import System.Posix.Types
import System.FilePath.Posix
import System.IO.Unsafe
import System.Time
import Data.Bits
import Debug.Trace

data DNA = DNA String String Integer deriving Show
data DNARecord = DR { dna :: DNA, time :: Float } deriving Show

class Gene d where
   mutate :: d -> d
   merge :: d -> d -> d
   fitness :: d -> IO TimeDiff

mutateRounds :: Gene d => d -> Int -> d
mutateRounds gene 0 = gene
mutateRounds gene x = mutateRounds (mutate gene) (x-1)

instance Gene DNA where
   mutate = mutateDNA
   fitness = fitnessDNA
   merge = undefined

mutateDNA :: DNA -> DNA
mutateDNA (DNA fp program bits) = DNA fp' program' bits'
                                  where 
                                      (i, program') = flipRandomBang fp program
                                      bits' = complementBit bits i
                                      (name, handle) = unsafePerformIO $ openTempFile "files/" "Main.hs"
                                      action = do
                                                 hClose handle
                                                 writeFile name program'
                                                 return name
                                      fp' = dropExtension $ unsafePerformIO action
                                      toss = hClose handle
mergeDNA :: DNA -> DNA -> DNA
mergeDNA = undefined

mutateDNAWithSetH :: DNA -> Integer -> Int -> DNA
mutateDNAWithSetH d@(DNA fp prog oldBits) bits i = if i == (bitSize bits)
                                                   then DNA fp prog bits
                                                   else mutateDNAWithSetH d' bits (i+1)
                                                   where
                                                   d' = if not $ (testBit bits i) == (testBit oldBits i)
                                                        then DNA fp (flipBang fp prog i) oldBits
                                                        else d

mutateDNAWithSet :: Integer -> DNA -> DNA
mutateDNAWithSet bits d = mutateDNAWithSetH d bits 0

fitnessDNA :: DNA -> IO TimeDiff
fitnessDNA (DNA fp program _) = do
                                staticExit <- system $ "ghc -XBangPatterns -outputdir temp/ -o " ++ fp ++ " " ++ fp ++ ".hs"
                                start <- getClockTime
                                dynExit <- system $ "./" ++ fp
                                end <- getClockTime
                                system $ "rm " ++ fp
                                case (staticExit, dynExit) of
                                     (ExitSuccess, ExitSuccess)   -> return $ diffClockTimes end start
                                     otherwise                    -> return $ TimeDiff {tdYear = maxBound, tdMonth = maxBound, tdDay = maxBound
                                                                                       ,tdHour = maxBound, tdMin = maxBound, tdSec = maxBound
                                                                                       ,tdPicosec = 0}

-- TODO : go through program and build the bit vector
createDNA :: String -> String -> DNA
createDNA fp program = DNA fp program 0

main :: IO ()

main = do 
          time <- fitness $ dna
          print time
          print dna
       where
           dna = mutateDNAWithSet 11 $ createDNA "Main" program
           filePath = "Main" ++ ".hs"
           program = unsafePerformIO $ readFile filePath

-- 3) terminate if time exceeds base "System.Timeout"
