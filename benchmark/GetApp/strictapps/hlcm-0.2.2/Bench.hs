-- HLCM, parallel execution benchmarking version.
-- (c) Alexandre Termier, 2009-2010
-- Original LCM algorithm from Takaki Uno and Hiroki Arimura.
-- 
--
-- See the README file for installation, usage, and details.
-- See the LICENSE file for licensing.

{-|
Main program to invoke the LCM algorithm and
compute closed frequent itemsets, with benchmarking options. 
These benchmarking options allow to test various ways of using semi-implicit parallelism of Haskell.

Usage : 

@benchHLCM /input_data support_threshold strategy_id value_for_parBuffer depth/@

See the page for the main of @hlcm@ for details about what the program do.

Parameters are :

 * input_data : input file, must be in the numeric format described in hlcm main executable.

 * support_threshold : minimal frequency of a pattern. Less means more computations and longer execution times.

 * strategy_id : 1 = myParBuffer from Simon Marlow (see file HLCM.hs, no memory leak), 2 = parBuffer from Control.Parallel.Strategies (can have a memory leak), 3 = parMap

 * value_for_buffer : if strategy is myParBuffer or parBuffer, value to use for buffering (you can start with 8)

 * depth : during depth first exploration, below the depth given no more sparks are created

A typical execution:

@time benchHLCM Data\/mushroom.dat 1000 1 8 2 >\/dev\/null +RTS -N2@

This runs benchHLCM on the mushroom dataset, with a support of 1000, strategy=myParBuffer with n=8 and a spark cutoff depth of 2.
The outputs are not interesting for benchmarking, they are thus dumped in /dev/null.
Two cores are requested.

Note that there is a known bug in GHC 6.10.4/6.12.1: on a machine with N cores, it is better to use <= N-1 cores.
See <http://hackage.haskell.org/trac/ghc/ticket/3553#comment:5>.

-}
module Main(main) where

import HLCM
import System( getArgs )

import qualified Data.ByteString.Char8 as L

{-|
Main program, parses command line, calls LCM and dumps raw output.
-}
main :: IO ()
main = do
  args <- getArgs
  (dataFile, _thres, _strat, _n, _d) <- return (args !! 0, args !! 1, args !! 2, args !! 3, args !! 4)
  stringMatrix <- L.readFile dataFile
  let 
      thres = read _thres::Frequency
      strat = read _strat::Int
      n = read _n::Int
      d = read _d::Int
      cfis = runBench stringMatrix thres strat n d in
    do
      putStrLn (show cfis)
      
      
runBench :: L.ByteString -> Frequency -> Int -> Int -> Int -> [[Item]]
runBench db frq strat n d
  | strat == 1 = benchLCM_parBuffer db frq n d
  | strat == 2 = benchLCM_parMap db frq d
  | otherwise = []
                 

