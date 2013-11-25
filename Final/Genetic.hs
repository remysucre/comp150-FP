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
import Data.List (sort)
import Debug.Trace

data DNA = DNA String String Integer Int deriving Show
data DNARecord = DR { dna :: DNA, time :: TimeDiff } deriving Show

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
   merge = mergeDNA

instance Eq DNA where
   d1@(DNA fp program bits _) == d2@(DNA fp' program' bits' _) = (fp == fp') && 
                                                                 (program == program') &&
                                                                 (bits == bits')

instance Eq DNARecord where
   dr1 == dr2 = (time dr1) == time dr2

instance Ord DNARecord where
   dr1 <= dr2 = (time dr1) <= time dr2

mutateDNA :: DNA -> DNA
mutateDNA (DNA fp program bits numPlaces) = writeDNAToDisk d'
                                  where 
                                      (i, program') = flipRandomBang fp program
                                      bits' = complementBit bits i
                                      numPlaces' = if numPlaces < i then i else numPlaces
                                      d' = DNA fp program' bits' numPlaces'

mergeDNA :: DNA -> DNA -> DNA
mergeDNA d1@(DNA fp program bits np) d2@(DNA fp' program' bits' np') = mutateDNAWithSet bits'' np'' d'
                                                                       where
                                                                           bits'' = bits .|. bits'
                                                                           np'' = if np < np' then np' else np
                                                                           d' = DNA fp program bits'' np''

writeDNAToDisk :: DNA -> DNA
writeDNAToDisk (DNA fp program bits numPlaces) = DNA fp' program bits numPlaces
                                                 where
                                                     (name, handle) = unsafePerformIO $ openTempFile "files/" "Main.hs"
                                                     action = do
                                                                hClose handle
                                                                writeFile name program
                                                                return name
                                                     fp' = dropExtension $ unsafePerformIO action

mutateDNAWithSetH :: DNA -> Integer -> Int -> Int -> DNA
mutateDNAWithSetH d@(DNA fp prog oldBits np) bits numPlaces i = if i == (numPlaces)
                                                               then writeDNAToDisk $ DNA fp prog bits numPlaces
                                                               else mutateDNAWithSetH d' bits numPlaces (i+1)
                                                               where
                                                                  d' = if not $ (testBit bits i) == (testBit oldBits i)
                                                                       then DNA fp (flipBang fp prog i) oldBits np
                                                                       else d

mutateDNAWithSet :: Integer -> Int -> DNA -> DNA
mutateDNAWithSet bits size d = mutateDNAWithSetH d bits size 0

fitnessDNA :: DNA -> IO TimeDiff
fitnessDNA (DNA fp program _ _) = do
                                    staticExit <- system $ "ghc -XBangPatterns -outputdir temp/ -o " ++ fp ++ " " ++ fp ++ ".hs"
                                    start <- getClockTime
                                    dynExit <- system $ "./" ++ fp
                                    end <- getClockTime
                                    system $ "rm " ++ fp
                                    system $ "rm temp/*"
                                    case (staticExit, dynExit) of
                                         (ExitSuccess, ExitSuccess)   -> return $ diffClockTimes end start
                                         otherwise                    -> return $ TimeDiff {tdYear = maxBound, tdMonth = maxBound, tdDay = maxBound
                                                                                           ,tdHour = maxBound, tdMin = maxBound, tdSec = maxBound
                                                                                           ,tdPicosec = 0}

-- TODO : go through program and build the bit vector
createDNA :: String -> String -> DNA
createDNA fp program = DNA fp program 0 $ placesToStrict fp program

massMutate :: Gene d => d -> Int -> [d]
massMutate g i = [g] ++ (take i $ map mutate $ repeat g)

createDNARecords :: DNA -> TimeDiff -> DNARecord
createDNARecords d time = DR d time

main :: IO ()
main = do 
          program <- readFile filePath
          def <- return $ createDNA "Main" program
          time <- fitness $ def
          dnas <- return $ massMutate def 5
          times <- sequence $ map fitness dnas
          records <- return $ sort $ map (uncurry createDNARecords) $ zip dnas times
          print records
       where
           bits = 33 :: Integer
           filePath = "Main" ++ ".hs"

-- 3) terminate if time exceeds base "System.Timeout"
