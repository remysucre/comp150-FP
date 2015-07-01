
{-# LANGUAGE RecordWildCards #-}

module BioInf.RNAwolf.TripletStem where

import qualified Data.Vector.Unboxed as VU

import Biobase.Primary
import Biobase.Secondary
import Data.PrimitiveArray
import Data.PrimitiveArray.Ix

import BioInf.Params
import BioInf.RNAwolf.Types


{-
      let vTStem    = 999999 -- minimumVU $ fTripletStem ps inp eStem      i j ct eI eJ
-}

{-
-- | Triplet stems have a shared nucleotide.
--   _C
-- A-_B where A is paired with both B and C.
--
-- If A,B is paired with cWW, then A,C can only use the S or H edge.

fTripletStem :: BaseF (ExtTable -> ExtFeatures (VU.Vector (ExtPairIdx,Double)))
fTripletStem Params{..} inp eStem i j ct eI eJ
  | j-i<2     = VU.empty
  | otherwise = {- VU.map fI iShared VU.++ -} VU.map fJ jShared
  where
    fI klExt@((k,l),(ctKL,eK,eL))
      = ( klExt
        , eStem ! klExt
--        + stemTriplet ! ( ((nI,nL),(ctKL,eK,eL)), ((nI,nJ),(ct,eI,eJ)) )
--        + if j-i-1<=maxDistance then pairDistance ! (j-i-1) else 0
        ) where nL = inp VU.! l
    fJ klExt@((k,l),(ctKL,eK,eL))
      = ( klExt
        , eStem ! klExt
        + stemTriplet ! ( ((nI,nJ),((ct,eI,eJ))), ((nJ,nK),(ctKL,eJ,eK)) ) --l==j
--        + if j-i-1<=maxDistance then pairDistance ! (j-i-1) else 0
        ) where nK = inp VU.! k
    nI = inp VU.! i
    nJ = inp VU.! j
    iShared = VU.fromList [ ((k,l),(du,eK,eL))
                          | let k=i
                          , let l=j-1
                          , eK<-[wc,sugar,hoogsteen]
                          , eK/=eI
                          , eL<-[wc,sugar,hoogsteen]
                          , du<-[cis,trans]
                          ]
    jShared = VU.fromList [ ((k,l),(du,eK,eL))
                          | let k=i+1
                          , let l=j
                          , eK<-[wc,sugar,hoogsteen]
                          , eL<-[wc,sugar,hoogsteen]
                          , du<-[cis,trans]
                          , eL/=eJ
                          ]
{- INLINE fTripletStem #-}
-}
