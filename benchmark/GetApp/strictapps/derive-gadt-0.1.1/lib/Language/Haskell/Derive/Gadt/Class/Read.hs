{-# OPTIONS_GHC -O2 -fglasgow-exts #-}
{-# LANGUAGE BangPatterns, TemplateHaskell #-}

module Language.Haskell.Derive.Gadt.Class.Read where

import Language.Haskell.Derive.Gadt.Common
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set(Set)
import qualified Data.Set as S
import Data.Monoid(Monoid(..))
import Language.Haskell.Meta hiding (parseExp,parseType)
import Language.Haskell.Meta.Utils
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Fixity
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Lib as TH
import Control.Applicative
import Control.Monad
import Text.PrettyPrint
import Data.Function
import Data.List

{-
TODO:
 (1) Determine the context (if any) needed in the instance decl
-}

-- ghci> ppDoc <$> test1
test1 = deriveReadGadts =<< TH.runIO (readFile "GADTTest.hs")

deriveReadGadts :: String -> TH.Q [TH.Dec]
deriveReadGadts s = do
  case parseModuleGadts s of
    Left e -> fail e
    Right is -> concat <$> mapM deriveReadGadtInfo is

deriveReadGadtInfo :: GadtInfo -> TH.Q [TH.Dec]
deriveReadGadtInfo info = do
  let grps = instanceGroups info
      go (t,xs) = let ys = fmap (\(n,ary)->(prettyPrint n, ary)) xs
                  in deriveReadConsQ t ys
  concat <$> mapM go (nubBy ((==) `on` fst) grps)

-- omg hax
deriveReadConsQ :: Type -> [(String, Int)] -> TH.Q [TH.Dec]
deriveReadConsQ ty cons = do
  let ary = maximum (fmap snd cons)
  p <- TH.newName "p"
  xs <- replicateM ary (TH.newName "x")
  s0:s1:ss <- replicateM (max 2 (ary+2)) (TH.newName "s")
  let ps = fmap TH.VarP [p,s0]
      doOne con xs s0 ss =
        let go  _  [s] = [TH.noBindS
              (TH.tupE [foldl TH.appE (TH.conE (TH.mkName con))
                                      (fmap TH.varE xs)
                      ,TH.varE s])]
            go (x:xs) (s0:s1:ss) = TH.bindS
              (TH.tupP [TH.varP x, TH.varP s1])
              [|readsPrec 11 $(TH.varE s0)|] : go xs (s1:ss)
            e0 = TH.bindS
                  (TH.tupP [TH.litP (TH.stringL con), TH.varP s1])
                  [|lex $(TH.varE s0)|]
            es = go xs ss
        in TH.compE (e0:es)
      es = flip fmap cons (\(con,ary) ->
              let ys = take ary xs
                  zs = s1 : take ary ss
              in doOne con ys s0 zs)
  e <- [|concat $(TH.listE (fmap (\x -> [|readParen ($(TH.varE p) > 10) $(TH.lamE [TH.varP s0] x) $(TH.varE s0)|]) es))|]

  let dec = mkFunD (TH.mkName "readsPrec") ps e
      inst = TH.instanceD
              (return [])
              (TH.conT ''Read `TH.appT` return (toType ty))
              [return dec]
  sequence [inst]











