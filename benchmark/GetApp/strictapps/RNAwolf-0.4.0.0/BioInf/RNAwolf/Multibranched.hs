{-# LANGUAGE RecordWildCards #-}

-- | Functions for handling non-triplet multibranched loops.
--
-- TODO We can do the loop-splitting thing again to speed up multibranched
-- closing by x10.

module BioInf.RNAwolf.Multibranched where

import qualified Data.Vector.Unboxed as VU

import Biobase.Primary
import Biobase.Secondary
import Data.PrimitiveArray
import Data.PrimitiveArray.Ix

import BioInf.Params
import BioInf.RNAwolf.Types



-- * An unpaired nucleotide to the right of an NMbr structure.

-- | Energy for having the rightmost nucleotide (at j) unpaired in NMBr.

fUnpairedRight :: BaseF (NMbr -> Features (VU.Vector (PairIdx,Double)))
fUnpairedRight Params{..} inp (NMbr nMbr) i j
  | i<0 || j>n = error $ "Multibranched.fUnpairedRight: " ++ show (i,j)
  | i==j       = VU.empty
  | otherwise  = VU.singleton s
  where
    s = ( (i,j-1)
        , nMbr !(i,j-1)
        )
    n = VU.length inp -1

-- | Backtrack in NMbr if the nucleotide at j is unpaired.

btUnpairedRight
  :: Params
  -> Primary
  -> NMbr
  -> NBT
  -> NBT
btUnpairedRight ps inp nMbr btM i j d =
  [ (x,z)
  | i>=0,i<j,j<=n
  , ((_,k),enext) <- VU.toList $ fUnpairedRight ps inp nMbr i j
  , let d' = newD d ehere enext
  , testD d'
  , (x,z) <- btM i k d'
  , testD z
  ] where
      ehere = unNMbr nMbr !(i,j)
      n = VU.length inp -1


-- * An unpaired nucleotide to the right of an NMbr1 structure.

-- | Energy for having the rightmost nucleotide (at j) unpaired in NMBr1.

fUnpairedRight1 :: BaseF (NMbr1 -> Features (VU.Vector (PairIdx,Double)))
fUnpairedRight1 Params{..} inp (NMbr1 nMbr1) i j
  | i<0 || j>n = error $ "Multibranched.fUnpairedRight: " ++ show (i,j)
  | i==j       = VU.empty
  | otherwise  = VU.singleton s
  where
    s = ( (i,j-1)
        , nMbr1 !(i,j-1)
        )
    n = VU.length inp -1

-- | Backtrack NMbr1 if the nucleotide at j is unpaired.

btUnpairedRight1
  :: Params
  -> Primary
  -> NMbr1
  -> NBT
  -> NBT
btUnpairedRight1 ps inp nMbr1 btM1 i j d =
  [ (x,z)
  | i>=0,i<j,j<=n
  , ((_,k),enext) <- VU.toList $ fUnpairedRight1 ps inp nMbr1 i j
  , let d' = newD d ehere enext
  , testD d'
  , (x,z) <- btM1 i k d'
  , testD z
  ] where
      ehere = unNMbr1 nMbr1 !(i,j)
      n = VU.length inp -1



-- * EStem to Helix start

-- | A multibranched helix (except the closing one). (i,j) are closed by a
-- basepair. Backtracking into the EStem reveals the type of pairing.

fMlHelix :: BaseF (EStem -> Features (VU.Vector (ExtPairIdx,Double)))
fMlHelix Params{..} inp (EStem eStem) i j
  | i==0 || j==VU.length inp -1 = VU.empty -- TODO not required ?!
  | otherwise = VU.map f exts
  where
    f ext@(ct,eI,eJ) =
      ( ijExt
      , eStem ! ijExt
      + mbClose ! (((nJ,nI),(ct,eJ,eI)),inp VU.! (j+1) ,inp VU.! (i-1))
      + multiHelix
      ) where
          ijExt = ((i,j),(ct,eI,eJ))
          nI = inp VU.! i
          nJ = inp VU.! j
    exts = VU.fromList [ (ct,eI,eJ)
                       | j-i>=2, i>0, j+1<VU.length inp
                       , eI<-wsh, eJ<-wsh, ct<-citr
                       ]
{-# INLINE fMlHelix #-}

-- | Backtracks from (i,j) in NMult into the extended-pairing EStem.

btMlHelix
  :: Params
  -> Primary
  -> NMult
  -> EStem
  -> ExtBT
  -> NBT
btMlHelix ps inp (NMult nMult) eStem btES i j d =
  [ (x,z)
  | i>0,i<j,j<n -- correct boundaries since we access elements at (i-1) and (j+1)
  , ((_,(eI,eJ,ct)),enext) <- VU.toList $ fMlHelix ps inp eStem i j
  , let d' = newD d ehere enext
  , testD d'
  , (x,z) <- btES i j eI eJ ct d'
  , testD z
  ] where
      ehere = nMult !(i,j)
      n = VU.length inp -1



-- * Closes a multibranched loop and produces an extended structure at
-- ((i,j),ext)

-- | Closes a multibranch loop.
--
-- TODO make completely triplet compliant

fMlClose :: BaseF (NMultLoop -> ExtFeatures (VU.Vector (PairIdx,Double)))
fMlClose Params{..} inp (NMultLoop nMultLoop) i j ct eI eJ
  | i>=j      = VU.empty
  | otherwise = VU.singleton s
  where
    s = ( (i,j)
        , nMultLoop !(i,j)
        + mlc
        )
    mlc = 0
        + multiBranched
        + multiHelix
        + mbClose ! ( ((inp VU.! i, inp VU.! j),(ct,eI,eJ))
                    , inp VU.! (i+1)
                    , inp VU.! (j-1)
                    )
  --      + if j-i-1<=maxDistance then pairDistance ! (j-i-1) else 0
{-# INLINE fMlClose #-}

-- | Backtrack from and extended annotation (ij,ext) into the helper table
-- NMultLoop.

btMlClose
  :: Params
  -> Primary
  -> EStem
  -> NMultLoop
  -> NBT
  -> ExtBT
btMlClose ps inp (EStem eStem) nMultLoop btMultLoop i j ct eI eJ d =
  [ (ij:x,z)
  | i>=0,i<j,j<=n
  , (_,enext) <- VU.toList $ fMlClose ps inp nMultLoop i j ct eI eJ
  , let d' = newD d ehere enext
  , testD d'
  , (x,z) <- btMultLoop i j d'
  , testD z
  ] where
      ij = ((i,j),(ct,eI,eJ))
      ehere = eStem!ij
      n = VU.length inp -1



-- * Multibranched loop helper table

-- | Multibranched loop helper function that combines "at least one stem" with
-- "exactly one stem" but does not add the closing energy from (i,j).

fMlLoop :: BaseF (NMbr -> NMbr1 -> Features (VU.Vector (Int,Double)))
fMlLoop Params{..} inp (NMbr nMbr) (NMbr1 nMbr1) i j = VU.map f ks where
  f k = ( k
        , nMbr!(i+1,k)
        + nMbr1!(k+1,j-1)
        )
  ks  = VU.enumFromN (i+2) (j-i-3) -- == [i+2 .. j-2]
{-# INLINE fMlLoop #-}

-- | Backtracking the multibranched loop.

btMlLoop
  :: Params
  -> Primary
  -> NMultLoop
  -> NMbr
  -> NMbr1
  -> NBT
  -> NBT
  -> NBT
btMlLoop ps inp (NMultLoop nMultLoop) nMbr nMbr1 btM btM1 i j d =
  [ (x++y,z)
  | i>=0,i<j,j<=n
  , (k,enext) <- VU.toList $ fMlLoop ps inp nMbr nMbr1 i j
  , let d' = newD d ehere enext
  , testD d'
  , i+1<k
  , (x,z') <- btM (i+1) k d'
  , testD z'
  , k+1<j-1
  , (y,z)  <- btM1 (k+1) (j-1) z'
  , testD z
  ] where
      ehere = nMultLoop !(i,j)
      n = VU.length inp -1



-- * Backtracking of a multibranched stem with unpaired nucleotides to the
-- left.

-- | Backtrack a single stem in NMbr, where the stem has zero or more unpaired
-- nucleotides to the left.

fMlStem :: BaseF (NMult -> Features (VU.Vector (Int,Double)))
fMlStem Params{..} inp (NMult nMult) i j = VU.map f ks where
  f k = ( k
        , nMult!(k,j)
        )
  ks  = VU.enumFromN i (j-i-1) -- == [i..j-2]
{-# INLINE fMlStem #-}

-- | Backtrack by trying to find a multilooped helix.

btMlStem
  :: Params
  -> Primary
  -> NMbr
  -> NMult
  -> NBT
  -> NBT
btMlStem ps inp (NMbr nMbr) nMult btMH i j d =
  [ (x,z) -- stem at (k,j)
  | i>=0,i<j,j<=n
  , (k,enext) <- VU.toList $ fMlStem ps inp nMult i j
  , let d' = newD d ehere enext
  , testD d'
  , (x,z) <- btMH k j d'
  , testD z
  ] where
      ehere = nMbr!(i,j)
      n = VU.length inp -1



-- * Backtracking of at least two stems by finding one or more stems to the
-- left and exactly one stem to the right.

-- | Add a stem to a multibranch table containing already at least one stem.

fMlStems :: BaseF (NMbr -> NMult -> Features (VU.Vector (Int,Double)))
fMlStems Params{..} inp (NMbr nMbr) (NMult nMult) i j = VU.map f ks where
  f k = ( k
        , nMbr!(i,k)
        + nMult!(k+1,j)
        )
  ks  = VU.enumFromN (i+2) (j-i-4) -- == [i+2..j-3]
{-# INLINE fMlStems #-}

-- | Backtrack by finding the splitting index between an NMbr composite
-- structure and a single multibranched stem NMult (which can contain unpaired
-- nucleotides to the left).

btMlStems
  :: Params
  -> Primary
  -> NMbr
  -> NMult
  -> NBT
  -> NBT
  -> NBT
btMlStems ps inp nMbr nMult btM btMH i j d =
  [ (x++y,z) -- nMbr ++ nStem
  | i>=0,i<j,j<=n
  , (k,enext) <- VU.toList $ fMlStems ps inp nMbr nMult i j
  , let d' = newD d ehere enext
  , testD d'
  , (x,z') <- btM i k d'
  , testD z'
  , (y,z) <- btMH (k+1) j z'
  , testD z
  ] where
      ehere = unNMbr nMbr !(i,j)
      n = VU.length inp -1



-- * Backtrack a single stem in NMbr1. This stem is closed at (i,j).

-- | Add a single stem to a multibranch table containing zero stems already.
--
-- TODO this would be equal to mlHelix, unify!

fMl1Stem :: BaseF (NMult -> Features (VU.Vector ((Int,Int),Double)))
fMl1Stem Params{..} inp (NMult nMult) i j = VU.singleton s where
  s = ( (i,j)
      , nMult!(i,j)
      )
{-# INLINE fMl1Stem #-}

-- | Backtrack a single stem closed at (i,j) for NMbr1. Takes the route through
-- NMult which solves for the exact pairtype.

btMl1Stem
  :: Params
  -> Primary
  -> NMbr1
  -> NMult
  -> NBT
  -> NBT
btMl1Stem ps inp (NMbr1 nMbr1) nMult btMH i j d =
  [ (x,z)
  | i>=0,i<j,j<=n
  , ((k,l),enext) <- VU.toList $ fMl1Stem ps inp nMult i j
  , i==k && j==l
  , let d' = newD d ehere enext
  , testD d'
  , (x,z) <- btMH i j d'
  , testD z
  ] where
      ehere = nMbr1!(i,j)
      n = VU.length inp -1
