{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}

-- | Transformation of predictions and known structures into keys. Keys are
-- used for linearization.
--
-- NOTE READ THE BIG FAT KEYS WARNING
--
-- TODO Generalize and move into its own library

module BioInf.Keys where

import Data.Vector.Unboxed as VU hiding ((++),concatMap,length,concat,null)
import qualified Data.Vector.Unboxed as VU
import Data.List as L
import qualified Data.Map as M

import Biobase.Primary
import Biobase.Secondary
import Biobase.Secondary.Diagrams
import Data.PrimitiveArray
import Data.PrimitiveArray.Ix

import BioInf.Params as P
import BioInf.Params.Import as P
import BioInf.Params.Export as P



-- | A list of "named" parameters.

paramsKeys = concat
  [ L.map (HairpinLength  . fst) . assocs . hairpinLength   $ zeroParams
  , L.map (HairpinClose   . fst) . assocs . hairpinClose    $ zeroParams
  , L.map (Stem           . fst) . assocs . stem            $ zeroParams
  , L.map (StemTriplet    . fst) . assocs . stemTriplet     $ zeroParams
  , L.map (InteriorLength . fst) . assocs . interiorLength  $ zeroParams
  , L.map (InteriorAsym   . fst) . assocs . interiorAsym    $ zeroParams
  , L.map (InteriorClose  . fst) . assocs . interiorClose   $ zeroParams
  , L.map (BulgeLength    . fst) . assocs . bulgeLength     $ zeroParams
  , L.map (BulgeTriplet   . fst) . assocs . bulgeTriplet    $ zeroParams
  , L.map (BulgeClose     . fst) . assocs . bulgeClose      $ zeroParams
  , L.map (MbClose        . fst) . assocs . mbClose         $ zeroParams
  -- scalar values for multiloops
  , [ MultiBranched, MultiHelix, MultiUnpaired ]
  -- distance between pairs
  , L.map (PairDistance   . fst) . assocs . pairDistance    $ zeroParams
  , [ InterMolInit ]
  ]

-- | Uniquely tag each key
--
-- NOTE BIG FAT WARNING: BE ABSOLUTELY SURE THAT ALL IMPORTS AND EXPORTS FOLLOW
-- THIS ORDERING EXACTLY, OTHERWISE KEYS WILL BE MAPPED TO WRONG POSITIONS
-- DURING LOOKUP AND VALUES END UP SOMEWHERE ELSE.

data Keys
  = HairpinLength   Int
  | HairpinClose    (ExtPair,Nuc,Nuc)
  | Stem            (ExtPair,ExtPair)
  | StemTriplet     (ExtPair,ExtPair)
  | InteriorLength  Int
  | InteriorAsym    Int
  | InteriorClose   (ExtPair,Nuc,Nuc)
  | BulgeLength     Int
  | BulgeTriplet    (ExtPair,ExtPair)
  | BulgeClose      ExtPair
  | MbClose         (ExtPair,Nuc,Nuc)
  | MultiBranched
  | MultiHelix
  | MultiUnpaired
  | PairDistance    Int
  | InterMolInit
  deriving (Read,Show,Eq,Ord)

-- | Training data to feature vector

featureVector :: String -> [ExtPairIdx] -> [Int]
featureVector inp xs = ys where
  ys = L.map lookupFeatureIndex tr
  tr = treeToFeatures inp $ ssTree (length inp) xs

-- | transform feature to 0-based index

lookupFeatureIndex :: Keys -> Int
lookupFeatureIndex k
  | Just v <- k `M.lookup` kvs = v
  | otherwise = error $ show ("key unknown: ", k)
  where

-- | Map param keys to thei Int-indices.

kvs = M.fromList $ L.zip paramsKeys [0..]
{-# NOINLINE kvs #-}

-- | And back from Int-indices to the keys.

vks = M.fromList $ L.zip [0 ::Int ..] paramsKeys
{-# NOINLINE vks #-}

-- | Takes a primary structure and secondary structure tree and produces a list
-- of keys.
--
-- TODO Data.Traversable ?!
--
-- TODO better handling of unknown features: we can have genuine errors
-- (pseudoknots) and uncoded features (e.g. hairpins of size > 30)

treeToFeatures :: (MkPrimary a, Show a) => a -> SSTree ExtPairIdx  t -> [Keys]
treeToFeatures inp = f where
  pri = mkPrimary inp
  swap23 (a,b,c) = (a,c,b)
  vuIndex xs k = if k<0 || k>= VU.length xs then error (show (inp,k)) else xs VU.! k
  n = VU.length pri -1
  -- Features for external loop
  f (SSExt n _ xs) = concatMap f xs
  -- Features for anything else
  f (SSTree ((i,j),ijExt) _ xs)

    -- intermolecular init
    | null xs
    , let is = VU.length . VU.filter (==nIMI) . VU.take (j-i) . VU.drop i $ pri
    , is > 0
    = L.replicate is InterMolInit

    -- hairpin
    -- TODO relax the minima?
    | null xs
    , j-i-1<=P.maxLength
    , j-i>=3
    = [ HairpinLength (j-i-1)
      , HairpinClose (((nI,nJ),ijExt),nIp1,nJm1)
--      , PairDistance  (j-i-1)
      ]

    -- normal stem
    | [SSTree ((k,l),klExt) _ _] <- xs
    , let nK = pri `vuIndex` k; nL = pri `vuIndex` l
    , i+1==k && j-1==l
    = [ Stem (((nI,nJ),ijExt),((nL,nK),swap23 klExt))
--      , PairDistance (j-i-1)
      ] ++ concatMap f xs
    -- interior loops
    | [SSTree ((k,l),klExt) _ _] <- xs
    , let lenI = k-i-1; lenJ = j-l-1; len = lenI+lenJ
    , lenI>=1 && lenJ>=1 && len<=P.maxLength
    , let nL = pri `vuIndex` l; nK = pri `vuIndex` k; lkExt = swap23 klExt
    , let nLm1 = pri `vuIndex` (l-1); nKp1 = pri `vuIndex` (k+1)
    , let nLp1 = pri `vuIndex` (l+1); nKm1 = pri `vuIndex` (k-1)
--    = [ InteriorClose (((nN,nN),(cis,wc,wc)),nN,nN) -- the closing pair (don't block this)
    = [ InteriorClose (((nI,nJ),ijExt),nIp1,nJm1) -- the closing pair (don't block this)
      , InteriorLength len
      , InteriorAsym $ abs (lenI-lenJ)
      , InteriorClose (((nL,nK),lkExt),nLp1,nKm1)
--      , PairDistance (j-i-1)
      ] ++ concatMap f xs
    -- normal bulge
    | [SSTree ((k,l),klExt) _ _] <- xs
    , let lenI = k-i-1; lenJ = j-l-1; len = max lenI lenJ
    , lenI==0 && lenJ>0 || lenJ==0 && lenI>0
    , len<=P.maxLength
    , let nK = pri `vuIndex` k; nL = pri `vuIndex` l; lkExt = swap23 klExt
    = [ BulgeLength len
      , BulgeClose ((nI,nJ),ijExt)
--      , BulgeClose ((nL,nK),lkExt)
--      , PairDistance (j-i-1)
      ] ++ concatMap f xs
    -- close a multibranched loop
    --
    -- TODO what about shared multibranched loops? (see sequence GCGGCACCGUCCGCUCAAACAAACGG in fr3d DB)
    | length xs > 1
--    = concatMap f xs
    = [ MbClose (((nI,nJ),ijExt),nIp1,nJm1)
--      , PairDistance (j-i-1)
      , MultiBranched
      , MultiHelix
      ] ++ concat
      -- each inner part
      [ [ MbClose (((nL,nK),lkExt),nLp1,nKm1)
        , MultiHelix ]
      | SSTree ((k,l),klExt) _ _ <- xs
      , k>0 && l<n
      , let nK = pri `vuIndex` k; nL = pri `vuIndex` l
      , let nKm1 = pri `vuIndex` (k-1); nLp1 = pri `vuIndex` (l+1)
      , let lkExt = swap23 klExt
      ] ++ concatMap f xs

    | otherwise = concatMap f xs
    where
      nI    = pri `vuIndex` i
      nJ    = pri `vuIndex` j
      nIp1  = pri `vuIndex` (i+1)
      nJm1  = pri `vuIndex` (j-1)
      jiExt = swap23 ijExt

-- | Create the secondary structure tree
--
-- FIXME okPairs is ad-hoc, we should allow for other kinds of pairs!

ssTree :: Int -> [ExtPairIdx] -> SSTree ExtPairIdx ()
ssTree n xs = d2sTree . mkD2S . (n,) . L.filter okPairs $ xs where
  okPairs ((i,j),_) = j-i>2 -- we only keep pairs which have at least to free nucleotides between them
