{-# OPTIONS_GHC -O2 -fglasgow-exts #-}
{-# LANGUAGE BangPatterns, TemplateHaskell #-}

module Language.Haskell.Derive.Gadt.Class.Eq where

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


deriveEqGadts :: String -> Q [Dec]
deriveEqGadts s = do
  case parseModuleGadts s of
    Left e -> fail e
    Right is -> concat <$> mapM deriveEqGadtInfo is

deriveEqGadtInfo :: GadtInfo -> Q [Dec]
deriveEqGadtInfo info = do
  let grps = instanceGroups info
      go (t,xs) = let ys = fmap (\(n,ary)->(prettyPrint n, ary)) xs
                  in deriveEqConsQ t ys
  concat <$> mapM go (nubBy ((==) `on` fst) grps)

deriveEqConsQ :: Hs.Type -> [(String, Int)] -> Q [Dec]
deriveEqConsQ ty cons = do
  let decs = fmap (uncurry mkEqFunD) cons ++ dflt
      dflt = [return (mkFunD (mkName "(==)")
                              [WildP,WildP]
                              (ConE 'False))]
      inst = instanceD
              (return [])
              (conT ''Eq `appT` return (toType ty))
              decs
  sequence [inst]

mkEqFunD :: String -> Int -> Q Dec
mkEqFunD con ary = do
  let name = mkName con
  xs <- replicateM ary (newName "x")
  ys <- replicateM ary (newName "y")
  let ps = [ConP name (fmap VarP xs)
           ,ConP name (fmap VarP ys)]
      a |==| b = [|$(varE a) == $(varE b)|]
      andQ [] = [|True|]
      andQ (e:es)= foldr (\a b -> [|$a && $b|]) e es
  mkFunD (mkName "(==)") ps <$>
    (andQ (zipWith (|==|) xs ys))



