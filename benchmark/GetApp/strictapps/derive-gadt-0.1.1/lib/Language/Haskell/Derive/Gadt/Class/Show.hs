{-# OPTIONS_GHC -O2 -fglasgow-exts #-}
{-# LANGUAGE BangPatterns, TemplateHaskell #-}

module Language.Haskell.Derive.Gadt.Class.Show where

import Language.Haskell.Derive.Gadt.Common
import Data.List
import Control.Monad
import Control.Applicative
import Language.Haskell.Meta.Utils
import Language.Haskell.Exts.Pretty
import qualified Language.Haskell.Exts.Syntax as Hs
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Text.PrettyPrint


deriveShowGadts :: String -> Q [Dec]
deriveShowGadts s = do
  case parseModuleGadts s of
    Left e -> fail e
    Right is -> concat <$> mapM deriveShowGadtInfo is


deriveShowGadtInfo :: GadtInfo -> Q [Dec]
deriveShowGadtInfo = deriveShowGadtShowInfo . gadtShowInfo


data GadtShowInfo = GadtShowInfo
  {gadtShowName  :: Name
  ,gadtShowArity :: Int
  ,gadtShowCxt   :: [Int]
  ,gadtShowCons  :: [(Name, Int)]}
  deriving(Show)

gadtShowInfo :: GadtInfo -> GadtShowInfo
gadtShowInfo info = GadtShowInfo
  {gadtShowName = (mkName . prettyPrint . gadtName) info
  ,gadtShowArity = gadtArity info
  ,gadtShowCxt = nub (concatMap collectShowCxt (gadtCons info))
  ,gadtShowCons = fmap (\c -> ((mkName . prettyPrint . gadtConName) c
                              ,(length . gadtConArgs) c))
                      (gadtCons info)}

-- XXX: need to deal with existentials
--  somehow (i.e. error out in that case?)
collectShowCxt :: GadtConInfo -> [Int]
collectShowCxt info =
  let cvs = fmap (mkName . prettyPrint) (gadtConBound info
                                          `intersect`
                                            gadtConFree info)
      ixs = (fmap (\(n,i) -> ((mkName . prettyPrint) n, i))
              . getTopTyVars . snd . splitTypeApps . gadtConType) info
  in concatMap (maybe [] (:[]) . flip lookup ixs) cvs

deriveShowGadtShowInfo :: GadtShowInfo -> Q [Dec]
deriveShowGadtShowInfo info = do
  do  vars <- fmap VarT <$> replicateM (gadtShowArity info) (newName "a")
      let cxt = fmap (ConT ''Show `AppT`) (fmap (vars!!) (gadtShowCxt info))
          tyCon = conT (gadtShowName info)
          decsQ = mkShowFunDs (gadtShowCons info)
      decs <- fmap return <$> decsQ
      sequence [instanceD (return cxt)
                (conT ''Show `appT` foldl appT tyCon
                              (fmap return vars)) decs]

mkShowFunDs :: [(Name, Int)] -> Q [Dec]
mkShowFunDs xs = do
  p <- newName "p"
  forM xs (\(n,i)-> do
    ns <- replicateM i (newName "x")
    ps <- sequence [varP p, conP n (fmap varP ns)]
    mkFunD 'showsPrec ps <$>
      (let es = intersperse [|showChar ' '|]
                  ([|showString $((litE . stringL . render . ppDoc) n)|]
                      : fmap (\x -> [|showsPrec 11 $(varE x)|]) ns)
        in  [|showParen ($(varE p) > 10) $(foldr (|.|) [|id|] es)|]))

