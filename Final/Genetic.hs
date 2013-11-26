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
import System.Timeout
import System.Random
import Debug.Trace

{------ General gene class for Genetic Algorithms ------}

class Gene d where
   mutate :: d -> d
   merge :: d -> d -> d
   fitness :: d -> IO TimeDiff

{- 
   Mutate a gene a certain number of times
   Return: a gene mutated n times
-}
mutateRounds :: Gene d => d -> Int -> d
mutateRounds gene 0 = gene
mutateRounds gene n = mutateRounds (mutate gene) (n-1)

{- 
   Mutate a single gene to multiple genes
   Return: a list of genes each mutated at least once
-}
massMutate :: Gene d => d -> Int -> [d]
massMutate g i = [g] ++ (take i $ map mutate $ repeat g)

{------ Gene for this Genetic Algorithm ------}

data DNA = DNA { path :: FilePath    -- filePath to program
               , prog :: String      -- program itself
               , vec  :: Integer     -- set of bits corresponding to places to insert strictness
               , n    :: Int         -- upper limit on size of the set of bits
               }
           deriving Show

instance Gene DNA where
   mutate = mutateDNA
   fitness = fitnessDNA
   merge = mergeDNA

instance Eq DNA where
   d1@(DNA fp program bits _) == d2@(DNA fp' program' bits' _) = (fp == fp') && 
                                                                 (program == program') &&
                                                                 (bits == bits')

{- 
   Mutate a single bit of the DNA
   Return: new piece of DNA that has been mutated
-}
mutateDNA :: DNA -> DNA
mutateDNA (DNA fp program bits numPlaces) = writeDNAToDisk d'
                                  where 
                                      (i, program') = flipRandomBang fp program
                                      bits' = complementBit bits i
                                      numPlaces' = if numPlaces < i then i else numPlaces
                                      d' = DNA fp program' bits' numPlaces'

{- 
   Mutate DNA according to a set of bits of a given size
   Return: new piece of DNA mutated according to bits
-}
mutateDNAWithSet :: Integer -> Int -> DNA -> DNA
mutateDNAWithSet bits size d = mutateDNAWithSetH d bits size 0

{- 
   Helper function: Goes through the bits and mutates DNA accordingly
-}
mutateDNAWithSetH :: DNA -> Integer -> Int -> Int -> DNA
mutateDNAWithSetH d@(DNA fp prog oldBits np) bits numPlaces i = if i == (numPlaces)
                                                               then writeDNAToDisk $ DNA fp prog bits numPlaces
                                                               else mutateDNAWithSetH d' bits numPlaces (i+1)
                                                               where
                                                                  d' = if not $ (testBit bits i) == (testBit oldBits i)
                                                                       then DNA fp (flipBang fp prog i) oldBits np
                                                                       else d

{- 
   Merge two parents to create a single child
   Return: new piece of DNA that results from both parents
-}
mergeDNA :: DNA -> DNA -> DNA
mergeDNA d1@(DNA fp program bits np) d2@(DNA fp' program' bits' np') = mutateDNAWithSet bits'' np'' d'
                                                                       where
                                                                           bits'' = bits .|. bits'
                                                                           np'' = if np < np' then np' else np
                                                                           d' = DNA fp program bits'' np''

{- 
   Helper function: Writes a program in DNA to a temp file and updates DNA
   Return: DNA updated to point to file just written on disk
-}
writeDNAToDisk :: DNA -> DNA
writeDNAToDisk (DNA fp program bits numPlaces) = DNA fp' program bits numPlaces
                                                 where
                                                     (name, handle) = unsafePerformIO $ openTempFile "files/" "Main.hs"
                                                     action = do
                                                                hClose handle
                                                                writeFile name program
                                                                return name
                                                     fp' = dropExtension $ unsafePerformIO action

{-
   Compile a program and return the exit code. Also deletes all temporary files.
-}
compile :: FilePath -> IO ExitCode
compile fp = do
               exit <- system $ "ghc -XBangPatterns -outputdir temp/ -o " ++ fp ++ " " ++ fp ++ ".hs"
               system $ "rm temp/*"
               return exit

{-
   Test fitness of DNA 
   Returns: infTime if there are any errors
            run time of the executable otherwise
-}
fitnessDNA :: DNA -> IO TimeDiff
fitnessDNA (DNA fp program _ _) = do
                                    --staticExit <- compile fp
                                    time <- timeIO $ system $ "./" ++ fp
                                    system $ "rm " ++ fp
                                    return time
                                    --case staticExit of
                                    --    ExitSuccess   -> return time
                                    --    ExitFailure _ -> return infTime

{-
   Create a new strand of DNA using a file path and a program
-}
-- TODO : go through program and build the bit vector
createDNA :: String -> String -> DNA
createDNA fp program = DNA fp program 0 $ placesToStrict fp program

{------ Records used to sort results of fitness ------}

{- 
   A record for a given piece of DNA after fitness 
-}
data DNARecord = DR { dna :: DNA, time :: TimeDiff } deriving Show

instance Eq DNARecord where
   dr1 == dr2 = (time dr1) == time dr2

instance Ord DNARecord where
   dr1 <= dr2 = (time dr1) <= time dr2

{-
   Create a record based off of DNA and the result of fitness function
-}
createDNARecords :: DNA -> TimeDiff -> DNARecord
createDNARecords d time = DR d time

{------ Functions for dealing with TimeDiffs ------}

{- 
   This denotes infinite time for nonterminating or failed computations
-}
infTime :: TimeDiff
infTime = TimeDiff {tdYear = maxBound, tdMonth = maxBound, tdDay = maxBound
                   ,tdHour = maxBound, tdMin = maxBound, tdSec = maxBound
                   ,tdPicosec = 0}

{- 
   Converts TimeDiff to an Int up to the hours
-}
timeToInt :: TimeDiff -> Int
timeToInt time = (secToMicro $ minToSec $ hourToMin $ tdHour time) + 
                 (secToMicro $ minToSec $ tdMin time) + 
                 (secToMicro $ tdSec time) + 
                 (picoToMicro $ tdPicosec time)

{------ Functions to convert to Microseconds ------}
secToMicro :: Int -> Int
secToMicro sec = sec * 1000

picoToMicro :: Integer -> Int
picoToMicro pico = fromInteger $ quot pico 1000000

minToSec :: Int -> Int
minToSec minute = minute * 60

hourToMin :: Int -> Int
hourToMin hour = hour * 60

{- 
   Timeout the fitness function after "time" microseconds
   Return: infTime if the computation fails or times out
           run time if the executable otherwise
-}
fitnessWithTimeout :: (Gene d) => Int -> d -> IO TimeDiff
fitnessWithTimeout time gene = do
                                 result <- timeout time $ fitness gene
                                 case result of
                                     Nothing -> return infTime
                                     Just t  -> return t

{-
   Takes an action with an exit code and times it
   Return: infTime if the computation fails
           run time of the action otherwise
-}
timeIO :: IO ExitCode -> IO TimeDiff
timeIO action = do
                  start <- getClockTime
                  exit <- action
                  end <- getClockTime
                  case exit of
                      ExitSuccess   -> return $ diffClockTimes end start
                      ExitFailure _ -> return infTime

{- 
   Number to add to the base time for timeout (for compilation)
-}
epsilon :: Int
epsilon = secToMicro 5

{------ Main algorithm ------}

{-
   Take a list of genes and create a new list of genes
   Return: List of genes that include the following
           - all genes given as input
           - if more than one gene is given, a set of children born at random
           - mutaions of all genes given and children (if applicable)
-}
buildGeneration :: Gene d => [d] -> [d]
buildGeneration dnas = gen'
                       where
                           gen = if (length dnas) > 1
                                 then mergeRand dnas 5
                                 else dnas
                           gen' = concat $ map ((flip massMutate) 5) gen

{-
   Randomly choose 2 genes and merge them into a new gene and do so n times
   Return: List of genes that are the given genes and all new children made
-}
mergeRand :: Gene d => [d] -> Int -> [d]
mergeRand dnas 0 = dnas
mergeRand dnas n = [merge (dnas !! i) (dnas !! j)] ++ mergeRand dnas (n-1)
                   where
                       range = (0, (length dnas) - 1)
                       i = unsafePerformIO $ getStdRandom $ randomR range
                       j = unsafePerformIO $ getStdRandom $ randomR range

{-
   Run fitness on an entire generation of DNA
   Return: list of most fit DNARecords wrapped in the IO monad
-}
runGeneration :: [DNA] -> Int -> Int -> IO [DNARecord]
runGeneration dnas time poolSize = do
                                     sequence $ map (compile . path) dnas -- Compile all the programs
                                     times <- sequence $ map (fitnessWithTimeout $ time + epsilon) dnas
                                     records <- return $ sort $ map (uncurry createDNARecords) $ zip dnas times
                                     return $ take poolSize records

{-
   Runs the genetic algorithm and returns a set of best DNA created
   Return: list of most fit DNA after n generations wrapped in the IO Monad
-}
geneticAlg :: [DNA] -> Int -> Int -> Int -> IO [DNA]
geneticAlg dnas 0 _    _        = return dnas
geneticAlg dnas n time poolSize = do
                                     records <- runGeneration (buildGeneration dnas) time poolSize
                                     geneticAlg (map dna records) (n-1) time poolSize

main :: IO ()
main = do 
          program <- readFile filePath
          def <- return $ createDNA moduleName program
          start <- getClockTime
          fitness $ def
          end <- getClockTime
          time <- return $ diffClockTimes end start
          ---dnas <- return $ buildGeneration [def]
          --dnas <- return $ massMutate def numMutations
          --sequence $ map (compile . path) dnas -- Compile all the programs
          --times <- sequence $ map (fitnessWithTimeout $ (timeToInt time) + epsilon ) dnas
          --records <- return $ sort $ map (uncurry createDNARecords) $ zip dnas times
          ---records <- runGeneration dnas (timeToInt time) 6
          dnas <- geneticAlg [def] 2 (timeToInt time) 6
          print $ head dnas
        where
           bits = 33 :: Integer
           moduleName = "Main"
           filePath = "Main" ++ ".hs"
           numMutations = 5
