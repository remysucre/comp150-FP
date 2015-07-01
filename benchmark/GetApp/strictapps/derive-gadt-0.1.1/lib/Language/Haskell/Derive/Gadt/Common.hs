{-# OPTIONS_GHC -O2 -fglasgow-exts #-}
{-# LANGUAGE BangPatterns, TemplateHaskell #-}

module Language.Haskell.Derive.Gadt.Common where
import qualified Language.Haskell.Derive.Gadt.Unify as U
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
import Data.List
ppHs a = (text . pretty) a

data GadtInfo = GadtInfo
  {gadtName   :: Name
  ,gadtArity  :: Int -- XXX: take into account higher kinds
  ,gadtCons   :: [GadtConInfo]}
  deriving(Show)

data GadtConInfo = GadtConInfo
  {gadtConName  :: Name
  ,gadtConType  :: Type
  ,gadtConArgs  :: [Type]
  ,gadtConBound :: [Name]
  ,gadtConFree  :: [Name]}
  deriving(Show)

-- test0 = do
--   Right a <- parseModuleGadts `fmap` readFile "GADTTest.hs"
--   return a

instanceGroups :: GadtInfo -> [(Type, [(Name, Int)])]
instanceGroups info = grps1
  where cmap = M.fromList (fmap (\c -> (gadtConName c, c)) (gadtCons info))
        grps0 = groupCons info
        grps1 = fmap (\(n,ns) ->
                        let c = cmap M.! n
                            t = gadtConType c
                            cs = fmap (cmap M.!) ns
                        in (t, fmap onlyImportant (c:cs))) grps0
        onlyImportant c = (gadtConName c, length (gadtConArgs c))

groupCons :: GadtInfo -> [(Name, [Name])]
groupCons = fmap (\(x,xs)->(x,fmap fst xs))
                  . unifiedGroups . gadtCons

unifiedGroups :: [GadtConInfo] -> [(Name, [(Name, (U.Substs,U.Substs))])]
unifiedGroups cons = fmap go monoCons
  where nonExCons = filter (not . isExistential) cons
        monoCons = filter (isMono . gadtConType) nonExCons
        ts = fmap (\c -> (gadtConName c
                    ,(srcExtsTypeToUnifyType . gadtConType) c)) cons
        go con = let n = gadtConName con
                     t = srcExtsTypeToUnifyType (gadtConType con)
                     xs = filter ((/=n) . fst) ts
                     doIt (n',t') = do
                      o <- U.unify t t'
                      case o of Left{} -> return []
                                Right subs -> return [(n',subs)]
                  in (n, U.unQ (concat <$> mapM doIt xs))

isExistential :: GadtConInfo -> Bool
isExistential = not . null . existentials

existentials :: GadtConInfo -> [Name]
existentials con = gadtConFree con \\ gadtConBound con

gadtInfo :: Decl -> [GadtInfo]
gadtInfo (GDataDecl _ _ cxt name tvbs kM gdecls hehe)
  = [GadtInfo {gadtName = name
              ,gadtArity = arityGadt tvbs kM
              ,gadtCons = fmap gadtConInfo gdecls}]
gadtInfo _ = []

gadtConInfo :: GadtDecl -> GadtConInfo
gadtConInfo (GadtDecl _ con t)
  = let (ty,args) = unwindType t
        bnd = ftvs ty
        fvs = S.unions (fmap ftvs args)
    in GadtConInfo
        {gadtConName = con
        ,gadtConType = ty
        ,gadtConArgs = args
        ,gadtConBound = S.toList bnd
        ,gadtConFree = S.toList fvs}

-- XXX: what about higher-kinded tyvars?
arityGadt :: [TyVarBind] -> Maybe Kind -> Int
arityGadt tvbs Nothing = length tvbs
arityGadt tvbs (Just k) = length tvbs + kindArity k

parseModuleGadts :: String -> Either String [GadtInfo]
parseModuleGadts s =
  case myParseModule s of
    Left e -> Left e
    Right m -> let is = concatMap gadtInfo
                        [d | d@(GDataDecl{}) <- moduleDecls m]
                in Right is

kindArity :: Kind -> Int
kindArity = go 0
  where go !n (KindFn _ b) = go (n+1) b
        go n    _          = n

tvbName :: TyVarBind -> Name
tvbName (KindedVar n _) = n
tvbName (UnkindedVar n) = n

unwindType :: Type -> (Type, [Type])
unwindType = go []
  where go acc (TyFun a b) = go (a:acc) b
        go acc  b          = (b, reverse acc)

splitTypeApps :: Type -> (Type, [Type])
splitTypeApps = go []
  where go acc (a `TyApp` b) = go (b:acc) a
        go acc      a       = (a, acc)

isTyVar :: Type -> Bool
isTyVar (TyVar{}) = True
isTyVar _         = False

getTopTyVars :: [Type] -> [(Name,Int)]
getTopTyVars = fmap (\(TyVar n,i)->(n,i)) . filter (isTyVar . fst) . flip zip [0..]

ftvs :: Type -> Set Name
ftvs (TyForall tvbsM cxt t)
  = let bnd = maybe mempty (S.fromList . fmap tvbName) tvbsM
    in ftvs t `S.difference` bnd
ftvs (TyFun a b) = ftvs a `S.union` ftvs b
ftvs (TyTuple boxed ts) = S.unions (fmap ftvs ts)
ftvs (TyList t) = ftvs t
ftvs (TyApp a b) = ftvs a `S.union` ftvs b
ftvs (TyVar n) = S.singleton n
ftvs (TyCon qn) = mempty
ftvs (TyParen t) = ftvs t
ftvs (TyInfix a qn b) = ftvs a `S.union` ftvs b
ftvs (TyKind t k) = ftvs t

isMono :: Type -> Bool
isMono (TyForall{}) = False
isMono (TyFun a b) = isMono a && isMono b
isMono (TyTuple boxed ts) = and (fmap isMono ts)
isMono (TyList t) = isMono t
isMono (TyApp a b) = isMono a && isMono b
isMono (TyVar{}) = False
isMono (TyCon{}) = True
isMono (TyParen t) = isMono t
isMono (TyInfix a qn b) = isMono a && isMono b
isMono (TyKind t k) = isMono t

srcExtsTypeToUnifyType :: Type -> U.Type
srcExtsTypeToUnifyType = go
  where go (TyForall tvbsM cxt t)
          = let bnd = maybe [] (fmap (nameToUName . tvbName)) tvbsM
            in U.ForallT bnd (go t)
        go (TyFun a b) = go a U..->. go b
          -- XXX: need to reject unboxed tuples
        go (TyTuple boxed ts) = U.tupT (fmap go ts)
        go (TyList t) = U.listT (go t)
        go (TyApp a b) = go a `U.AppT` go b
        go (TyVar n) = U.VarT (nameToUName n)
        go (TyCon qn) = U.ConT (qnameToUName qn)
        go (TyParen t) = go t
        go (TyInfix a qn b) =
          (U.ConT (qnameToUName qn) `U.AppT` go a) `U.AppT` go b
          -- XXX: don't ignore kinds
        go (TyKind t k) = go t

myParseType :: String -> Type
myParseType s = either error id (parseResultToEither (parseType s))

nameToUName :: Name -> U.Name
nameToUName (Ident s) = U.mkNameL s
nameToUName (Symbol s) = U.mkNameL s

qnameToUName :: QName -> U.Name
qnameToUName = U.mkNameG . prettyPrint

myParseModule :: String -> Either String Module
myParseModule = parseResultToEither . parseModuleWithMode myParseMode

myParseMode :: ParseMode
myParseMode = ParseMode
  {parseFilename = []
  ,extensions = myExtensions
  ,ignoreLanguagePragmas = False
  ,fixities = baseFixities}

myExtensions :: [Extension]
myExtensions =
  [ OverlappingInstances
  , UndecidableInstances
  , IncoherentInstances
  , RecursiveDo
  , ParallelListComp
  , MultiParamTypeClasses
  , NoMonomorphismRestriction
  , FunctionalDependencies
  , RankNTypes
  , PolymorphicComponents
  , ExistentialQuantification
  , ScopedTypeVariables
  , ImplicitParams
  , FlexibleContexts
  , FlexibleInstances
  , EmptyDataDecls
  , CPP
  , KindSignatures
  , BangPatterns
  , TypeSynonymInstances
  , TemplateHaskell
  , ForeignFunctionInterface
  , NamedFieldPuns
  , PatternGuards
  , GeneralizedNewtypeDeriving
  , ExtensibleRecords
  , HereDocuments
  , MagicHash
  , TypeFamilies
  , StandaloneDeriving
  , UnicodeSyntax
  , PatternSignatures
  , UnliftedFFITypes
  , LiberalTypeSynonyms
  , TypeOperators
  , RecordWildCards
  , RecordPuns
  , DisambiguateRecordFields
  , OverloadedStrings
  , GADTs
  , RelaxedPolyRec
  , ExtendedDefaultRules
  , UnboxedTuples
  , DeriveDataTypeable
  , ConstrainedClassMethods
  , PackageImports
  , ImpredicativeTypes
  , NewQualifiedOperators
  , PostfixOperators
  , QuasiQuotes
  , TransformListComp
  , ViewPatterns ]

mkFunD :: TH.Name -> [TH.Pat] -> TH.Exp -> TH.Dec
mkFunD f xs e = TH.FunD f [TH.Clause xs (TH.NormalB e) []]

mkClauseQ :: [TH.PatQ] -> TH.ExpQ -> TH.ClauseQ
mkClauseQ ps e = TH.clause ps (TH.normalB e) []

