
module BioInf.RNAwolf.Types where

import Biobase.Primary
import Biobase.Secondary
import Data.PrimitiveArray

import BioInf.Params



newD d here next = d - (next - here)
testD d = d>=0

-- | Should really go into BiobaseXNA

wsh = [wc,sugar,hoogsteen]

-- | Should really go into BiobaseXNA

citr = [cis,trans]




type ExtBT = Int -> Int -> CTisomerism -> Edge -> Edge -> Double -> BTAnswer
type NBT = Int -> Int -> Double -> BTAnswer

type BTAnswer = [([ExtPairIdx],Double)]

type Table    = PrimArray PairIdx    Double
type ExtTable = PrimArray ExtPairIdx Double

type BaseF a = Params -> Primary -> a
type ExtFeatures a = Int -> Int -> CTisomerism -> Edge -> Edge -> a
type Features a = Int -> Int -> a

type Tables = ( EStem
              , NStem
              , NInte
              , NInteLoop
              , NBulg
              , NBulgLoop
              , NMult
              , NMbr
              , NMbr1
              , NMultLoop
              , NExtn
              )

-- ** Newtype wrappers for all tables.
--
-- NOTE Don't ever not newtype-wrap or you will hurt your brain.

newtype EStem = EStem {unEStem :: ExtTable}
newtype NStem = NStem {unNStem :: Table}
newtype NInte = NInte {unNInte :: Table}
newtype NInteLoop = NInteLoop {unNInteLoop :: Table}
newtype NMult = NMult {unNMult :: Table}
newtype NBulg = NBulg {unNBulg :: Table}
newtype NBulgLoop = NBulgLoop {unBulgLoop :: Table}
newtype NMbr  = NMbr  {unNMbr :: Table}
newtype NMbr1 = NMbr1 {unNMbr1 :: Table}
newtype NExtn = NExtn {unNExtn :: Table}
newtype NMultLoop = NMultLoop {unMultLoop :: Table}
