{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | RNAwolf extended secondary structure folding program. This is an extended
-- version of the algorithm first described in:

--  Hoener zu Siederdissen, Christian and Bernhart, Stephan H. and Stadler,
--  Peter F. and Hofacker, Ivo L.
--  "A Folding Algorithm for Extended RNA Secondary Structures"
--  Bioinformatics, 2011

--  http://www.tbi.univie.ac.at/software/rnawolf/

module Main where

import System.Console.CmdArgs
import Control.Monad
import Text.Printf
import Data.List.Split (splitEvery)

import Biobase.Primary
import Biobase.Secondary.Constraint

import BioInf.RNAwolf
import BioInf.RNAwolf.Types
import BioInf.Params as P

import Data.PrimitiveArray



main :: IO ()
main = do
  o@Options{..} <- cmdArgs options
  when (null inDB) $ error "you need to give a database"
  db <- fmap read $ readFile inDB
  xs <- fmap (splitEvery (if constraint then 2 else 1) . lines) $ getContents
  mapM_ (foldLine o db) xs
  return ()

foldLine :: Options -> Params -> [String] -> IO ()
foldLine Options{..} p (inp:rest) = do
  let pri = mkPrimary inp
  let cst = if null rest then (mkConstraint $ replicate (length inp) '.') else (mkConstraint $ head rest)
  let ts = rnaWolf p cst pri
  let bt = take coopt $ rnaWolfBacktrack p cst pri subopt ts
--  printX inp ts
  putStrLn inp
  forM_ bt $ \(pairs,score) -> do
    printf "%s %7.4f\n" (simpleViewer inp pairs) score
    forM_ pairs $ \((i,j),ext) -> do
      printf "  %4d %4d %s\n" i j (showX ext)
  return ()

printX inp (_,_,_,_,_,_,_,_,_,_,NExtn n) = print $ n!(0,length inp -1)

showX (ct,ei,ej) = show ct ++ show ei ++ show ej

-- * options

data Options = Options
  { inDB :: FilePath
  , subopt :: Double
  , coopt :: Int
  , constraint :: Bool
  } deriving (Show,Data,Typeable)

options = Options
  { inDB = "" &= help "specify parameter database"
  , subopt = 0.00001 &= help "calculate suboptimal in this range (returns all suboptimal results)"
  , coopt = 1 &= help "how many co-optimal structures to return"
  , constraint = False &= help "2-line input: sequence, then constraint (default: no)"
  }

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
