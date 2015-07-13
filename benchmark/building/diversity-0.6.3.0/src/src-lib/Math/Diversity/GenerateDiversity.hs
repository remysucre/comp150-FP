-- GenerateDiversity module.
-- By G.W. Schwartz
--
{- | Collection of functions for the collection of fragments for the
diversity calculations.
-}

{-# LANGUAGE BangPatterns #-}

module Math.Diversity.GenerateDiversity ( fragmentPos
                                        , generatePositionMap ) where

-- Built in
import qualified Data.Map.Strict as Map
import Data.List
import Data.Fasta.String
import qualified Data.Sequence as Seq
import Debug.Trace

-- Cabal
import qualified Data.List.Split as Split

-- Local
import Math.Diversity.Types

-- | Get the sample ID of a sequence
getSample :: Int -> FastaSequence -> Sample
getSample x = (!! (x - 1)) . Split.splitOn "|" . fastaHeader

-- | Generates fragment list from string of "win" length. This version
-- differs from normal as it takes a tuple with the position as the first
-- entry. Is in tail recursive form
fragmentPos :: Bool -> Int -> [(Position, Char)] -> [(Position, Fragment)]
fragmentPos whole win ls = fragmentPosLoop ls []
  where
    fragmentPosLoop [] !acc = acc
    fragmentPosLoop !xs !acc
        | whole && null xs = error "Empty line in file!!"
        | whole            = [combine xs]
        | length xs < win  = acc
        | otherwise        = fragmentPosLoop
                             (tail xs)
                             (combine (take win xs) : acc)
    combine ((!x, !y):ys)  = (x, y:(map snd ys))

-- | Generate the frequency from a FastaSequence
generatePositionMap :: Bool
                    -> Int
                    -> Bool
                    -> Window
                    -> FastaSequence
                    -> PositionMap
generatePositionMap !sample !sampleField !whole !win = posSeqList
  where
    posSeqList !x       = Map.fromList
                        . map (\(!p, !f) -> (p, Map.singleton (sampleIt sample x f) 1))
                        . fragmentPos whole win
                        . filter (noGaps . snd)
                        . zip [1..]
                        . fastaSeq
                        $ x
    noGaps y            = y /= '-' && y /= '.'
    sampleIt True !s !f = (getSample sampleField s, f)
    sampleIt False _ !f = ("Sample", f)
