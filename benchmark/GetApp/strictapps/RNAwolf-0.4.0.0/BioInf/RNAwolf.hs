{-# LANGUAGE BangPatterns #-}

-- | The RNAwolf folding algorithm, version 1.9. We now have full stacking and
-- rich parameters everywhere. In general, most parameters closely follow what
-- we have for ViennaRNA 1.8 but with extended RNA secondary structures,
-- instead of canonicals only. Further refinements of the parameter system will
-- follow.
--
-- TODO right now, 1-diagrams only, 2-diagrams come back in a few days. I want
-- to be sure that the full stacking approach does not introduce subtle bugs.
--
-- TODO recast all fZZZ functions for folding to actually fuse on minimum/fZZZ.
--
-- TODO VU.! -> VU.unsafeIndex
--
-- TODO possibly very big TODO: is this being optimized? : fold $ g z where g z
-- = if z==True then [1..10] else []. If this is not optimized, we should
-- change all functions below in a way that allows optimization. (I dont think
-- fusion can fire on these objects...)
--
--   TODO rewrite minimumVU to accept "Either" ctors and specialize on them.
--   "Left" to be used for strange errors, "Right" for correct streams



module BioInf.RNAwolf
  ( rnaWolf
  , rnaWolfBacktrack
  , rnaWolfOptimal
  ) where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as VU
import Control.Arrow
import Data.Fixed (mod')

import Data.PrimitiveArray
import Data.PrimitiveArray.Ix
import Biobase.Primary
import Biobase.Secondary
import Biobase.Secondary.Constraint (Constraint, bonusTable)

import BioInf.Params
import BioInf.RNAwolf.Types
import qualified BioInf.RNAwolf.Bulge as Bul
import qualified BioInf.RNAwolf.Constraint as Constraint
import qualified BioInf.RNAwolf.Extern as Ext
import qualified BioInf.RNAwolf.Hairpin as Hp
import qualified BioInf.RNAwolf.Interior as Int
import qualified BioInf.RNAwolf.Multibranched as Mul
import qualified BioInf.RNAwolf.Stem as Stem
import qualified BioInf.RNAwolf.TripletBulge as TrB
import qualified BioInf.RNAwolf.TripletStem as TrS

import Debug.Trace



-- * Folding

-- | Wrapper around the state monad.

rnaWolf :: Params -> Constraint -> Primary -> Tables
rnaWolf ps cst inp = {-# SCC "rnaWolf" #-} runST $ foldST ps cst inp

-- | Folding magic. In principle, this is not more complicated than
-- Nussinov-style folding.

foldST :: Params -> Constraint -> Primary -> ST s Tables
foldST ps cst inp = do
  let n = VU.length inp -1
  let imi = map fst . filter ((==nIMI).snd) $ zip [0..] (VU.toList inp)
  let constraintTable = bonusTable (-10000) 1000000 cst
  (eStemM,eStem) <- second EStem `fmap` mkExtTable n
  (nStemM,nStem) <- second NStem `fmap` mkTable n
  (nInteM,nInte) <- second NInte `fmap` mkTable n -- interior loop helper table
  (nMultM,nMult) <- second NMult `fmap` mkTable n -- multibranched loop helper table
  (nBulgM,nBulg) <- second NBulg `fmap` mkTable n -- bulge loop helper table
  (nMbrM ,nMbr ) <- second NMbr  `fmap` mkTable n
  (nMbr1M,nMbr1) <- second NMbr1 `fmap` mkTable n
  (nExtnM,nExtn) <- second NExtn `fmap` mkTableWith 0 n
  (nBulgLoopM,nBulgLoop) <- second NBulgLoop `fmap` mkTable n
  (nInteLoopM,nInteLoop) <- second NInteLoop `fmap` mkTable n -- interior loop helper table
  (nMultLoopM,nMultLoop) <- second NMultLoop `fmap` mkTable n -- multibranched loop helper table
  -- This version of the (i,j) pair generation walks along the diagonals. It is
  -- required to calculate this way, as otherwise the shared nucleotide
  -- variants will fail.
  forM_ (mkIJ n) $ \(i,j) -> do
    -- Fill helper tables that need to be accessed to calculate "weak": interior, bulged
    -- FIXME
    -- fill the interior LOOP table (includes everything except the closing pair)
    writeM nInteLoopM (i,j) . minimumVU $ Int.fInteriorLoop ps inp nInte i j
    -- fill bulge LOOP table
    writeM nBulgLoopM (i,j) . minimumVU $ Bul.fBulgeLoop ps inp nBulg i j
    -- multibranched close helper table (should improve speed for MLs by 2x3x3)
    writeM nMultLoopM (i,j) . minimumVU $ Mul.fMlLoop ps inp nMbr nMbr1 i j
    -- and now, fill the weak table
    forM_ citr $ \ct -> forM_ wsh $ \eI -> forM_ wsh $ \eJ -> do
      -- weak table (everything is weak)
      let vHairpin  = minimumVU $ Hp.fHairpin imi    ps inp           i j ct eI eJ
      let vStem     = minimumVU $ Stem.fStem         ps inp eStem     i j ct eI eJ
      let vInterior = minimumVU $ Int.fInteriorOuter ps inp nInteLoop i j ct eI eJ
      let vMlClose  = minimumVU $ Mul.fMlClose       ps inp nMultLoop i j ct eI eJ
      let vBulge    = minimumVU $ Bul.fBulgeOuter    ps inp nBulgLoop i j ct eI eJ
      writeM eStemM ((i,j),(ct,eI,eJ)) . Constraint.applyConstraint (i,j) constraintTable $ minimum [vHairpin,vStem,vInterior,vMlClose,vBulge] -- FIXME vTStem
    -- fill stem table that ignores extended annotations
    writeM nStemM (i,j) . minimumVU $ Stem.fNstem ps inp eStem i j
    -- fill the inner interior table
    writeM nInteM (i,j) . minimumVU $ Int.fInteriorInner ps inp eStem i j
    -- fill multibranch helper table
    writeM nMultM (i,j) . minimumVU $ Mul.fMlHelix ps inp eStem i j
    -- fill bulge close helper table
    writeM nBulgM (i,j) . minimumVU $ Bul.fBulgeInner ps inp eStem i j
    -- one or more multibranched stems
    let vUnpaired = minimumVU $ Mul.fUnpairedRight ps inp nMbr i j
    let vStem = minimumVU $ Mul.fMlStem ps inp nMult i j
    let vStems = minimumVU $ Mul.fMlStems ps inp nMbr nMult i j
    writeM nMbrM (i,j) $ minimum [vUnpaired, vStem, vStems]
    -- exactly one multibranched stem
    let vUnpaired = minimumVU $ Mul.fUnpairedRight1 ps inp nMbr1 i j
    let vStem = minimumVU $ Mul.fMl1Stem ps inp nMult i j
    writeM nMbr1M (i,j) $ minimum [vUnpaired,vStem]
  let j=n
  forM_ [n-2,n-3..0] $ \i -> do
    let unp = minimumVU $ Ext.fLeftUnpaired ps inp nExtn i j
    let es  = minimumVU $ Ext.fStem ps inp nStem i j
    let esl = minimumVU $ Ext.fStems ps inp nStem nExtn i j
    let one = minimumVU $ Ext.fOne ps inp i j
    writeM nExtnM (i,j) $ minimum [unp,esl,es,one]
  return  ( eStem
          , nStem
          , nInte
          , nInteLoop
          , nBulg
          , nBulgLoop
          , nMult
          , nMbr
          , nMbr1
          , nMultLoop
          , nExtn)



-- * Backtracking

-- | Given parameters, input, score band, and filled tables we can backtrack.
--
-- NOTE the order in which backtracking for individual functions is performed,
-- is important. In case of ties in energy, the first result is taken. This
-- should be considered!
--
-- [1] We consider unpaired stretches always first. This is kind of arbitrary.
--
-- [2] extended stems always come last. This is because they can potentially
-- introduce many co-optimal structures before they are all discarded.
--
-- TODO all the crap in comments are bug-fix backtracking options.

rnaWolfBacktrack :: Params -> Constraint -> Primary -> Double -> Tables -> [([ExtPairIdx],Double)]
rnaWolfBacktrack ps cst inp delta ( estem@(EStem eStem)
                                  , nstem@(NStem nStem)
                                  , ninte@(NInte nInte)
                                  , ninteloop@(NInteLoop nInteLoop)
                                  , nbulg@(NBulg nBulg)
                                  , nbulgloop@(NBulgLoop nBulgLoop)
                                  , nmult@(NMult nMult)
                                  , nmbr@(NMbr nMbr)
                                  , nmbr1@(NMbr1 nMbr1)
                                  , nmultloop@(NMultLoop nMultLoop)
                                  , nextn@(NExtn nExtn)
                                  )
  | null res = [([],0)]
  | otherwise = let finalScore = nExtn ! (0,n)
                in filter ((<=0).snd) . map (second (\z -> constraintMangling $ finalScore + delta -z)) $ res
  where
    res = btE 0 n delta
    btE i j d = -- trace (show ("btE",i,j,d)) $
      Ext.btOne ps inp nextn i j d ++ -- [1]
      Ext.btLeftUnpaired ps inp nextn btE i j d ++
      Ext.btStem ps inp nextn nstem btNS i j d ++
      Ext.btStems ps inp nstem nextn btNS btE i j d
    btNS i j d = -- trace (show ("btNS",i,j,d)) $
      Stem.btNstem ps inp nstem estem btES i j d
    btES :: Int -> Int -> CTisomerism -> Edge -> Edge -> Double -> [([ExtPairIdx],Double)]
    btES i j ct eI eJ d' = let d = d' - constraintTable!(i,j) in -- trace (show ("btES",i,j,ct,eI,eJ,d)) $
      Hp.btHairpin ps inp estem i j ct eI eJ d ++
      Int.btInteriorOuter ps inp estem ninteloop btILoop i j ct eI eJ d ++
      Bul.btBulgeOuter ps inp estem nbulgloop btBULoop i j ct eI eJ d ++
      Mul.btMlClose ps inp estem nmultloop btMultLoop i j ct eI eJ d ++
      Stem.btStem ps inp estem btES i j ct eI eJ d -- [2]
    btILoop i j d = -- trace (show ("btILoop",i,j,d)) $
      Int.btInteriorLoop ps inp ninteloop ninte btIL i j d
    btIL i j d = -- trace (show ("btIL",i,j,d)) $
      Int.btInteriorInner ps inp ninte estem btES i j d
    btBULoop i j d = -- trace (show ("btBULoop",i,j,d)) $
      Bul.btBulgeLoop ps inp nbulgloop nbulg btBU i j d
    btBU i j d = -- trace (show ("btBU",i,j,d)) $
      Bul.btBulgeInner ps inp nbulg estem btES i j d
    btMH i j d = -- trace (show ("btMH",i,j,d)) $
      Mul.btMlHelix ps inp nmult estem btES i j d
    btMultLoop i j d =
      Mul.btMlLoop ps inp nmultloop nmbr nmbr1 btM btM1 i j d
    btM i j d = {- trace (show ("btM",i,j,d)) $ -}
      Mul.btUnpairedRight ps inp nmbr btM i j d ++
      Mul.btMlStem ps inp nmbr nmult btMH i j d ++
      Mul.btMlStems ps inp nmbr nmult btM btMH i j d
    btM1 i j d = let ehere = nMbr1!(i,j) in
      Mul.btUnpairedRight1 ps inp nmbr1 btM1 i j d ++
      Mul.btMl1Stem ps inp nmbr1 nmult btMH i j d

    newD d here next = d - (next - here)
    testD d = d>=0
    n = VU.length inp -1
    epsilon = 0.001
    imi = map fst . filter ((==nIMI).snd) $ zip [0..] (VU.toList inp)
    constraintTable = bonusTable (-10000) 1000000 cst
    constraintMangling s = s - 10000 * c where
      c = fromIntegral $ round $ s / 10000

-- | Return the optimal energy.

rnaWolfOptimal :: Tables -> Double
rnaWolfOptimal ( estem@(EStem eStem)
               , nstem@(NStem nStem)
               , ninte@(NInte nInte)
               , ninteloop@(NInteLoop nInteLoop)
               , nbulg@(NBulg nBulg)
               , nbulgloop@(NBulgLoop nBulgLoop)
               , nmult@(NMult nMult)
               , nmbr@(NMbr nMbr)
               , nmbr1@(NMbr1 nMbr1)
               , nmultloop@(NMultLoop nMultLoop)
               , nextn@(NExtn nExtn)
               ) = nExtn ! (0,n) where n = snd . snd $ bounds nExtn

-- * Helper functions

-- | Given an unboxed vector with (index,value) elements, return the minimum
-- over the values.
--
-- TODO with vector-0.7.2 / vector-0.8, rewrite using "snd . unzip" (or not,
-- see next todo)
--
-- TODO http://trac.haskell.org/vector/ticket/51

minimumVU xs = VU.foldl' (\(!acc) (!k,!v) -> min acc v) 999999 xs
{-# INLINE minimumVU #-}

-- | Create 2d-tables, initialized with "infinity"
--
-- TODO use (infinity :: Energy)

mkTable n = mkTableWith 9999999 n

-- | Create 2d-tables, initialized with 'z'

mkTableWith z n = do
  tM <- fromAssocsM (0,0) (n,n) z []
  t  <- unsafeFreezeM tM
  return (tM,t)

-- | 2d-tables with extended information.

mkExtTable n = mkExtTableWith 9999999 n

-- | 2d-tables with extended information.

mkExtTableWith z n = do
  tM <- fromAssocsM ((0,0),(cis,wc,wc)) ((n,n),(trans,hoogsteen,hoogsteen)) z []
  t  <- unsafeFreezeM tM
  return (tM,t)

-- | Produces indices in correct diagonal order.
--
-- TODO this is a stupid way to create the indices...

mkIJ n = [ (i,j) | d <- [0..n], j<-[n,n-1..0], let i=j-d, j>=0, i>=0 ]



-- * Types




-- * debugging

trc k x = trace (show (k,x)) x
trc' k x = trace (show k) x
trci' c k x = if c then trace (show k) x else x

