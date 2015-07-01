{-# OPTIONS_GHC -O2 -fglasgow-exts #-}
{-# LANGUAGE BangPatterns, TemplateHaskell #-}

module Language.Haskell.Derive.Gadt.Class.Ord where

import Language.Haskell.Derive.Gadt.Common
import Data.List
import Control.Monad
import Control.Applicative
import Language.Haskell.Meta
import Language.Haskell.Meta.Utils
import Language.Haskell.Exts.Pretty
import qualified Language.Haskell.Exts.Syntax as Hs
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Text.PrettyPrint
import Data.Function



deriveOrdGadts :: String -> Q [Dec]
deriveOrdGadts s = do
  case parseModuleGadts s of
    Left e -> fail e
    Right is -> concat <$> mapM deriveOrdGadtInfo is

deriveOrdGadtInfo :: GadtInfo -> Q [Dec]
deriveOrdGadtInfo info = do
  let grps = instanceGroups info
      go (t,xs) = let ys = fmap (\(n,ary)->(prettyPrint n, ary)) xs
                  in deriveOrdConsQ t ys
  concat <$> mapM go (nubBy ((==) `on` fst) grps)

deriveOrdConsQ :: Hs.Type -> [(String, Int)] -> Q [Dec]
deriveOrdConsQ ty cons = do
  let t = toType ty
  [x,y] <- replicateM 2 (newName "x")
  e <- mkCompareE t cons x y
  let decs = [return (mkFunD 'compare
                      [VarP x, VarP y] e)]
      inst = instanceD
              (return [])
              (conT ''Ord `appT` return t)
              decs
  sequence [inst]

mkCompareE :: Type -> [(String, Int)] -> Name -> Name -> ExpQ
mkCompareE t cons a b = do
  goDecs <- fmap return <$> mkGo cons
  tagDecs <- fmap return <$> mkTagMap (fmap fst cons)
  let goSig = sigD (mkName "go") (return (t .->. (t .->. ConT ''Ordering)))
      tagSig = sigD (mkName "tag") (return (t .->. ConT ''Int))
  letE ([goSig] ++ goDecs ++ [tagSig] ++ tagDecs)
        [|$(varE (mkName "go")) $(varE a) $(varE b)|]

mkGo :: [(String, Int)] -> Q [Dec]
mkGo cons = do
  let go = mkName "go"
      clauses = fmap (uncurry mkCompareSame) cons
                          ++ [mkCompareDiff]
  sequence [funD go clauses]

mkTagMap :: [String] -> Q [Dec]
mkTagMap cons = do
  let go con i = mkClauseQ
                  [recP (mkName con) []]
                  [|$(lift (i::Int))::Int|]
      clauses = zipWith go cons [0..]
  sequence [funD (mkName "tag") clauses]

mkCompareSame :: String -> Int -> ClauseQ
mkCompareSame con ary = do
  let name = mkName con
  xs <- replicateM ary (newName "x")
  ys <- replicateM ary (newName "y")
  let ps = [conP name (fmap varP xs)
           ,conP name (fmap varP ys)]
  mkClauseQ ps [|compare $(tupE (fmap varE xs))
                         $(tupE (fmap varE ys))|]

mkCompareDiff :: ClauseQ
mkCompareDiff = do
  let tag = varE (mkName "tag")
  [x,y] <- replicateM 2 (newName "x")
  mkClauseQ (fmap varP [x,y])
    [|compare $(tag `appE` varE x)
              $(tag `appE` varE x)|]




