
{-# LANGUAGE RecordWildCards #-}

module BioInf.RNAwolf.Interior where

import qualified Data.Vector.Unboxed as VU

import Biobase.Primary
import Biobase.Secondary
import Data.PrimitiveArray
import Data.PrimitiveArray.Ix

import BioInf.Params
import BioInf.RNAwolf.Types

import Debug.Trace



-- * Outer part

-- | The outer part of an interior loop. Given a certain basepair type, add the
-- cost from the unpaired part.

fInteriorOuter :: BaseF (NInteLoop -> ExtFeatures (VU.Vector (PairIdx,Double)))
fInteriorOuter Params{..} inp (NInteLoop nInteLoop) i j ct eI eJ
  | j-i<4     = VU.empty
  | otherwise = VU.map f $ VU.singleton (i,j)
  where
    f (k,l) = ( (i,j)
              , nInteLoop ! (i,j)
              + ijSc
--              + if j-i-1<=maxDistance then pairDistance ! (j-i-1) else 0
              )
    ijSc = interiorClose ! (((nI,nJ),(ct,eI,eJ)),nIp1,nJm1)
--    ijSc = interiorClose ! (((nN,nN),(cis,wc,wc)),nN,nN)
    nI   = inp VU.! i
    nJ   = inp VU.! j
    nIp1 = inp VU.! (i+1)
    nJm1 = inp VU.! (j-1)
{-# INLINE fInteriorOuter #-}

-- |

btInteriorOuter
  :: Params
  -> Primary
  -> EStem
  -> NInteLoop
  -> NBT -- recursive backtracking function for loops
  -> ExtBT
btInteriorOuter ps inp (EStem eStem) nInteLoop btILoop i j ct eI eJ d = -- iltrc ("ilOuter",i,j,lol) $
  [ (ij:x,z) -- interior loop
  | i>=0,i<j,j<=n
  , ((k,l),enext) <- VU.toList $ fInteriorOuter ps inp nInteLoop i j ct eI eJ
  , let d' = newD d ehere enext
  , testD d'
  , (x,z) <- btILoop k l d'
--  , testD z
  ] where
      ij = ((i,j),(ct,eI,eJ))
      ehere = eStem!ij
      n = VU.length inp -1
      lol = VU.toList $ fInteriorOuter ps inp nInteLoop i j ct eI eJ

iltrc k x = trace (show (k,x)) x


-- * Loop part

-- | Performs the interior loop calculations between (i,j) "outer" and (k,l)
-- "inner" part. The score based on the unpaired nucleotides is independent of
-- both, the outer and the inner basepair type.
--
-- NOTE / TODO -- fusion enabled for this function (due to it taking 50% of the
-- time), full fusion is still dependent on other factors and needs to be
-- checked (in particular, we still have allocation events)

fInteriorLoop :: BaseF (NInte -> Features (VU.Vector (PairIdx,Double)))
fInteriorLoop Params{..} inp (NInte nInte) i j
  | j-i<4 = VU.empty
  | otherwise = VU.map f kls
  where
    f (k,l) = ( (k,l)
              , nInte ! (k,l)
              + interiorLength ! (lenI+lenJ)
              + interiorAsym ! (abs $ lenI - lenJ)
              ) where lenI = k-i-1; lenJ = j-l-1
    kls  = VU.map (\(dI,dJ) -> (i+dI,j-dJ)) $ fInteriorKLs i j
{-# INLINE fInteriorLoop #-}

-- | Backtrack the unpaired loop region

btInteriorLoop
  :: Params
  -> Primary
  -> NInteLoop
  -> NInte
  -> NBT
  -> NBT
btInteriorLoop ps inp (NInteLoop nInteLoop) nInte btIL i j d = -- iltrc ("ilLoop",i,j) $
  [ (x,z)
  | i>=0,i<j,j<=n
  , ((k,l),enext) <- VU.toList $ fInteriorLoop ps inp nInte i j
  , let d' = newD d ehere enext
  , testD d'
  , (x,z) <- btIL k l d'
--  , testD z
  ] where
      ehere = nInteLoop!(i,j)
      n = VU.length inp -1



-- * Inner part

-- | This opens up an interior loop. For each index (i,j) we minimize over all
-- possible basepair types.

fInteriorInner :: BaseF (EStem -> Features (VU.Vector (ExtPairIdx,Double)))
fInteriorInner Params{..} inp (EStem eStem) i j
  | j-i<2 = VU.empty
  | i==0 || j+1==VU.length inp = VU.empty
  | otherwise = VU.map f kls
  where
    f ijExt@((i,j),(ctIJ,eI,eJ)) =
      ( ijExt
      , eStem ! ijExt
      + interiorClose ! (((nJ,nI),(ctIJ,eJ,eI)),nJp1,nIm1)
      ) where nI = inp VU.! i
              nJ = inp VU.! j
              nIm1 = inp VU.! (i-1)
              nJp1 = inp VU.! (j+1)
    kls = VU.fromList [ ((i,j),(ctIJ,eI,eJ))
                      | eI<-wsh, eJ<-wsh, ctIJ<-citr
                      ]
{-# INLINE fInteriorInner #-}

-- | Backtrack from an NInte result to the corresponding EStem parts

btInteriorInner
  :: Params
  -> Primary
  -> NInte
  -> EStem
  -> ExtBT
  -> NBT
btInteriorInner ps inp (NInte nInte) eStem btES i j d = -- iltrc ("ilInner",i,j) $
  [ (x,z)
  | i>=0,i<j,j<=n
  , ((_,(eI,eJ,ct)),enext) <- VU.toList $ fInteriorInner ps inp eStem i j
  , let d' = newD d ehere enext
  , testD d'
  , (x,z) <- btES i j eI eJ ct d'
--  , testD z
  ] where
      n = VU.length inp -1
      ehere = nInte!(i,j)



-- * Helper functions

-- | Since backtracking interior loops is mostly selfcontained, we encapsulate
-- the above three functions -- which we can't do easily with the forward
-- calculations as they actually have to save on runtime.

{-
btInteriorComplete
  :: Params
  -> Primary
  -> EStem
  -> NInteLoop
  -> NInte
  -> ExtBT
  -> ExtBT
btInteriorComplete ps pri eStem nInteLoop nInte btExtStem i j ct eI eJ d =
  btInteriorOuter ps pri eStem btiloop i j ct eI eJ d
  where btiloop i j d = btInteriorLoop  ps pri nInteLoop nInte btinner i j d
        btinner = btInteriorInner ps pri nInte     eStem btExtStem
-}

-- | Given the outer indices (i,j), produces delta_i and delta_j so that
-- i+delta_i and j-delta_j are the inner indices. 'fInteriorKLs' should fuse
-- and should make sure that l-k>=4 is always true (maxd). Furthermore the
-- maximal unpaired length of both sides combined is determined by 'maxLength'.
--
-- TODO better name than 'maxLength'

fInteriorKLs :: Int -> Int -> VU.Vector (Int,Int)
fInteriorKLs i j = didjs where
  didjs = VU.unfoldr mkDs (4,2)
  mkDs (d,s)
    | d>maxd = Nothing
    | s>=d-2 = Just ((d-s,s),(d+1,2))
    | otherwise = Just ((d-s,s),(d,s+1))
  {-# INLINE mkDs #-}
  maxd = min maxLength (j-i-4)
{-# INLINE fInteriorKLs #-}

