{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | This program trains a parameter database for RNAwolf. The user has to take
-- care to only give appropriate training data to the optimizer. The most
-- important rule is to not give any pseudoknotted data. The small helper
-- program "MkTrainingData" should be able to take care of this.
-- "MkTrainingData" is part of BiobaseTrainingData.
--
-- We currently train using an optimization scheme described in:
--
-- Zakov, Shay and Goldberg, Yoav and Elhaded, Michael and Ziv-Ukelson, Michal
-- "Rich Parameterization Improves RNA Structure Prediction"
-- RECOMB 2011
--
-- NOTE It is likely that this we extended with other methods in the (near)
-- future, again. Especially the convex-optimization-based (even though the
-- Zakov et al. scheme is derived from cvx-methods) system seems promising.
-- Right now, this version simply is faster...
--
-- TODO update the DB within IO to save creation / destruction of Params in
-- each iteration
--
-- TODO re-allow co-folding

module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Parallel (pseq)
import Control.Parallel.Strategies
import Data.Function (on)
import Data.List
import Data.List.Split (splitEvery)
import Data.Ord
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as VU
import System.Console.CmdArgs
import System.Random
import Text.Printf
import System.IO (hFlush,stdout)

import Biobase.Primary
import Biobase.Secondary.Constraint
import Biobase.Secondary.Diagrams
import Biobase.TrainingData
import Biobase.TrainingData.Import
import Statistics.ConfusionMatrix
import Statistics.PerformanceMetrics

import BioInf.Keys
import BioInf.Params as P
import BioInf.Params.Export as P
import BioInf.Params.Import as P
import BioInf.PassiveAggressive
import BioInf.RNAwolf



-- | Entry function

main :: IO ()
main = do
  o@Options{..} <- cmdArgs options
  when (null outDB) $
    error "please set --outdb"
  when (null trainingData) $
    error "please give at least one training data file with --trainingdata"
  -- read training data
  xs <- id
      . fmap (filter (\TrainingData{..} ->
                       True
--                       length primary > 20 &&
                       && all (/='&') primary -- no co-folding right now
--                       length secondary > 5 -- at least 5 basepairs
                     )
             )
      . fmap (filter (lengthFlt maxLength))
      . fmap concat
      $ mapM fromFile trainingData
  -- read database or use zero-based parameters
  dbIn <- maybe (return . P.fromList . map (+0.01) . P.toList $ P.zeroParams) (fmap read . readFile) inDB
  -- dbOut <- foldM (foldTD $ length xs) dbIn $ zip xs [1..]
  (dbOut,_) <- foldM (doIteration o xs) (dbIn,[]) [1..numIterations]
  writeFile outDB $ show dbOut

-- | length filter for training data

lengthFlt l TrainingData{..} = maybe True (length primary <) l

-- | iterations to go

doIteration :: Options -> [TrainingData] -> (P.Params,[Double]) -> Int -> IO (P.Params,[Double])
doIteration o@Options{..} xs' (!p,rhos) !k = do
  xs <- fmap (splitEvery parallelism) $ shuffle xs'
  when (Iteration `elem` verbose) $ do
    putStrLn "\n======================================"
    printf "# INFO iteration: %4d / %4d starting\n"
            k
            numIterations
    printf "# INFO folding %d elements, maximal length: %d\n"
            (length xs')
            (maximum $ map (length . primary) xs')
    putStrLn "======================================\n"
    hFlush stdout
  let indices = mapAccumL (\acc x -> (acc+x,(acc+1,acc+x))) 0 $ map length xs
  (newp,rs) <- foldM (foldTD o $ length xs) (p,[]) . zip xs . snd $ indices
  let drctch = sum $ zipWith (\x y -> abs $ x-y) (P.toList p) (P.toList newp)
  let rhosum = sum $ map accMeas rs
  let rho = rhosum / (sum . map genericLength $ xs)
  when (Iteration `elem` verbose) $ do
    putStrLn "\n======================================"
    printf "# INFO iteration: %4d / %4d ended, rho: %4.2f (%4.2f, %4.2f)\n"
            k
            numIterations
            rho
            (minimum $ map accMeas rs)
            (maximum $ map accMeas rs)
    putStr "# INFO rho history:"
    zipWithM_ (printf " %4d %4.2f") [1::Int ..] $ rhos++[rho]
    putStrLn ""
    putStrLn "======================================\n"
    hFlush stdout
  writeFile (printf "%04d.db" k) . show $ newp
  return (newp,rhos++[rho])

-- | Fold one 'TrainingData' element and return the suggested changes and
-- additional information.

foldOne :: Options -> P.Params -> TrainingData -> (TrainingData,PA)
foldOne o@Options{..} p td
  | null bs   = (td   , PA [] 0 0 ["no prediction for: " ++ primary td])
  | otherwise = (fst worst, ret)
  where
    pri = mkPrimary $ primary td
    cst = mkConstraint $ replicate (length $ primary td) '.'
    tables = rnaWolf p cst pri
    bs = let f x = td{predicted = x} in
         map (first f) . take (maybe 1 id maxLoss) $ rnaWolfBacktrack p cst pri 0.001 tables
    worst = minimumBy (comparing (fmeasure . mkConfusionMatrix . fst)) bs
    runPA (x,score) = defaultPA aggressiveness p
            $ x { comments =
                    [ show score
                    , simpleViewer (primary x) $ secondary x
                    , simpleViewer (primary x) $ predicted x
                    , show $ predicted x
                    ]
                }
    ret = runPA worst

-- | Folding of 'TrainingData' elements.

foldTD :: Options -> Int -> (P.Params,[PA]) -> ([TrainingData],(Int,Int)) -> IO (P.Params,[PA])
foldTD o@Options{..} total (!p,oldresults) (ts,(f,t)) = do
  -- At this point, we trade most efficient optimization with increased parallelism, if that option is >1
  let parfolds = map (foldOne o p) ts
  let !results = let xs = map snd parfolds in xs `using` (parList rdeepseq)
  let cs = concatMap changes results
  let cur = VU.fromList . P.toList $ p
  let new = P.fromList . VU.toList $ VU.accum (\v pm -> v+pm) cur cs
  let rhosum = sum $ map accMeas results
  let rho = rhosum / genericLength ts
  let rhosumR = sum . map accMeas $ oldresults ++ results
  let rhoR = rhosumR / genericLength (oldresults ++ results)
  when (Single `elem` verbose) $ do
    printf "# INFO parallel: %4d - %4d, avg.rho: %4.2f, running rho: %4.2f\n"
            f t
            rho
            rhoR
  -- detailed information on each folded structure
  mapM_ (printDetailed o . fst) parfolds
  hFlush stdout
  return $ pseq (rdeepseq results)
         ( new
         , oldresults ++ results
         )

-- | simple viewer...

simpleViewer s xs = foldl f (replicate (length s) '.') xs where
  f str ((i,j),_) = upd ')' j $ upd '(' i str
  upd c k str
    |  l=='('
    && c=='('
    = pre ++ "<" ++ post
    |  l==')'
    && c==')'
    = pre ++ ">" ++ post
    | l/='.' = pre ++ "X" ++ post
    | otherwise = pre ++ [c] ++ post
    where
      pre = take k str
      l = head $ drop k str
      post = drop (k+1) str

-- | print out detailed information on a folded candidate

printDetailed :: Options -> TrainingData -> IO ()
printDetailed Options{..} x = do
  when (Detailed `elem` verbose) $ do
    putStrLn $ take (length $ primary x) . concatMap show . concat . repeat $ [0..9]
    putStrLn $ primary x
    putStrLn $ simpleViewer (primary x) $ secondary x
    putStrLn $ simpleViewer (primary x) $ predicted x
    when (AllPairs `elem` verbose) $ do
      mapM_ print $ predicted x


-- ** program options

data Options = Options
  { inDB :: Maybe FilePath
  , outDB :: FilePath
  , trainingData :: [FilePath]
  , maxLength :: Maybe Int
  , numIterations :: Int
  , verbose :: [Verbose]
  , maxLoss :: Maybe Int
  , aggressiveness :: Double
  , errorOnError :: Bool
  , parallelism :: Int
  } deriving (Show,Data,Typeable)

data Verbose
  = Iteration
  | Single
  | Detailed
  | AllPairs
  deriving (Show,Data,Typeable,Eq)

options = Options
  { inDB  = Nothing &= help "database from which to continue optimizing; if none is given, start from scratch"
  , outDB = ""      &= help "new database to write out"
  , trainingData = [] &= help "training data elements to read"
  , maxLength = Nothing &= help "[dev] only train using elements of length or less"
  , numIterations = 50 &= help "how many optimizer iterations"
  , verbose = [] &= help "select verbosity options: single, iteration, detailed (all switch on different verbosity options)"
  , maxLoss = Nothing &= help "use maxLoss optimization instead of prediction-based, requires maximal number of instances to search for maxLoss (default: not used)"
  , aggressiveness = 1 &= help "maximal tau for each round"
  , errorOnError = False &= help "error out if an error is detected (default: false)"
  , parallelism = 1 &= help "perform more than one prediction concurrently. Will probably reduce the effectiveness of the algorithm but allow to use more than one core; call with +RTS -N -RTS"
  }



-- ** helper functions

-- | simple shuffling of a list

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  r <- getStdRandom (randomR (0,length xs -1))
  let (hs,ts) = splitAt r xs
  let y = head ts
  ys <- shuffle $ hs ++ tail ts
  return $ y : ys
