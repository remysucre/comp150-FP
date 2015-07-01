

-- UUAGC 0.9.40.1 (src-ag/Transform.ag)
module Transform where
{-# LINE 8 "src-ag/Transform.ag" #-}

import Control.Monad(mplus,mzero)
import Data.List (partition, elem, nub,intersperse, union)
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set as Set (Set, member, union, toList, fromList, empty, singleton, member, unions, size, fold, intersection, difference, insert, elems)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq, empty, (><),fromList)
import Data.Foldable(toList)
import UU.Scanner.Position(noPos)

import ConcreteSyntax
import AbstractSyntax
import ErrorMessages
import Patterns (Patterns(..),Pattern(..))
import Expression (Expression(..))
import HsToken

import Options
import CommonTypes
import RhsCheck
import Debug.Trace
{-# LINE 30 "dist/build/Transform.hs" #-}

{-# LINE 2 "src-ag/ConcreteSyntax.ag" #-}

import UU.Scanner.Position (Pos)
import Patterns   (Pattern)
import Expression (Expression)
import CommonTypes
import Macro --marcos
{-# LINE 39 "dist/build/Transform.hs" #-}

{-# LINE 2 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 46 "dist/build/Transform.hs" #-}
{-# LINE 106 "src-ag/Transform.ag" #-}
type DefinedSets = Map Identifier (Set NontermIdent) 
{-# LINE 49 "dist/build/Transform.hs" #-}

{-# LINE 126 "src-ag/Transform.ag" #-}
type FieldMap  = [(Identifier, Type)] 
{-# LINE 53 "dist/build/Transform.hs" #-}

{-# LINE 127 "src-ag/Transform.ag" #-}
type DataTypes = Map.Map NontermIdent (Map.Map ConstructorIdent FieldMap) 
{-# LINE 57 "dist/build/Transform.hs" #-}

{-# LINE 150 "src-ag/Transform.ag" #-}
type AttrName   = (Identifier,Identifier) 
{-# LINE 61 "dist/build/Transform.hs" #-}

{-# LINE 151 "src-ag/Transform.ag" #-}
type RuleInfo   = (Maybe Identifier, [AttrName]->Pattern, Expression, [AttrName], Bool, String, Bool, Bool) 
{-# LINE 65 "dist/build/Transform.hs" #-}

{-# LINE 152 "src-ag/Transform.ag" #-}
type SigInfo    = (Identifier,Type) 
{-# LINE 69 "dist/build/Transform.hs" #-}

{-# LINE 153 "src-ag/Transform.ag" #-}
type UniqueInfo = (Identifier,Identifier) 
{-# LINE 73 "dist/build/Transform.hs" #-}

{-# LINE 154 "src-ag/Transform.ag" #-}
type AugmentInfo = (Identifier,Expression)
{-# LINE 77 "dist/build/Transform.hs" #-}

{-# LINE 155 "src-ag/Transform.ag" #-}
type AroundInfo  = (Identifier,Expression)
{-# LINE 81 "dist/build/Transform.hs" #-}

{-# LINE 156 "src-ag/Transform.ag" #-}
type MergeInfo   = (Identifier, Identifier, [Identifier], Expression)
{-# LINE 85 "dist/build/Transform.hs" #-}

{-# LINE 205 "src-ag/Transform.ag" #-}


checkDuplicate :: (Identifier -> Identifier -> Error)
               -> Identifier -> val -> Map Identifier val -> (Map Identifier val,Seq Error)
checkDuplicate dupError key val m
  = case Map.lookupIndex key m of
     Just ix -> let (key',_) = Map.elemAt ix m
                in  (m,Seq.singleton (dupError key key'))
     Nothing -> (Map.insert key val m,Seq.empty)

checkDuplicates :: (Identifier -> Identifier -> Error)
                -> [(Identifier, val)] -> Map Identifier val -> (Map Identifier val,Seq Error)
checkDuplicates dupError new m = foldErrors check m new
 where  check = uncurry (checkDuplicate dupError)

foldErrors f e xs = foldl g (e,Seq.empty) xs
  where g ~(e,es) x = let (e',es') = f x e
                      in (e', es >< es')


checkForDuplicates :: (Identifier -> Identifier -> Error)  ->  [Identifier]  ->  [Error]
checkForDuplicates err [] = []
checkForDuplicates err (x:xs) = let (same,other) = partition (equalId x) xs
                                in  map (err x) same ++ checkForDuplicates err other

equalId :: Identifier -> Identifier -> Bool
equalId x y = getName x == getName y

{-# LINE 116 "dist/build/Transform.hs" #-}

{-# LINE 351 "src-ag/Transform.ag" #-}

type RulesAndErrors = ([Rule], Seq Error)
type SigsAndErrors  = ([TypeSig], Seq Error)
type InstsAndErrors = ([(Identifier, Type)], Seq Error)
type UniquesAndErrors = (Map Identifier Identifier, Seq Error)
type AugmentsAndErrors = (Map Identifier [Expression], Seq Error)
type AroundsAndErrors = (Map Identifier [Expression], Seq Error)
type MergesAndErrors  = (Map Identifier (Identifier, [Identifier], Expression), Seq Error)
type AttrOverwrite  = Map AttrName Bool
type AccumRuleCheck = (RulesAndErrors, AttrOverwrite)
type AccumDefiCheck = (Seq Error, AttrOverwrite, [AttrName], [AttrName])

checkRules :: Map NontermIdent (Attributes, Attributes) -> Map NontermIdent (Map ConstructorIdent FieldMap) ->
              Map NontermIdent (Map ConstructorIdent [Identifier]) -> Map NontermIdent (Map ConstructorIdent [SigInfo]) ->
              Map NontermIdent (Map ConstructorIdent [MergeInfo]) ->
              NontermIdent -> ConstructorIdent -> [RuleInfo] -> RulesAndErrors
checkRules attributes fields allinsts allsigs allmerges nt con rs
  = let fieldmap :: FieldMap
        fieldmap = (_LHS, NT nt [] False) : (_LOC, NT nullIdent [] False) : (_INST, NT nullIdent [] False) : (_FIRST, NT nullIdent [] False) : (_LAST, NT nullIdent [] False)
                 : Map.findWithDefault [] con (Map.findWithDefault Map.empty nt fields)
                 ++ mapMaybe (\instNm -> lookup instNm sigs >>= \tp -> return (instNm, tp)) (Map.findWithDefault [] con (Map.findWithDefault Map.empty nt allinsts))
                 --   merged children are not allowed to have any inherited attrs defined: do not include

        sigs = Map.findWithDefault [] con (Map.findWithDefault Map.empty nt allsigs)

        hasAttrib f tp attr  = Map.member attr (f (Map.findWithDefault (Map.empty,Map.empty) tp attributes))

        checkRule :: RuleInfo -> AccumRuleCheck -> AccumRuleCheck
        checkRule (mbNm, pat,exp,as,owrt,str, pur, eager) ((r1,e1),m1)
          = let (e2,m2,u2,b2) = foldr (checkDefi owrt) (e1,m1,[],[]) as
            in  ( (Rule mbNm (pat u2) exp owrt str True pur False Nothing eager : r1, e2), m2)

        checkDefi :: Bool -> AttrName -> AccumDefiCheck -> AccumDefiCheck
        checkDefi owrt fa@(field,attr) (e,m,u,bs)
         = case lookup field fieldmap
            of  Just (NT tp _ _) ->
                  let tp' = maybe tp id (deforestedNt tp)
                  in              if field == _LOC || field == _INST || field == _FIRST || field == _LAST
                                     || hasAttrib (if getName field==getName _LHS then snd else fst) tp' attr
                                  then case Map.lookupIndex fa m of
                                           Just ix -> let ((_,attr2),b) = Map.elemAt ix m
                                                       in  if b && not (fa `elem` bs)
                                                           then (                                             e, Map.insert fa owrt m, fa:u, fa:bs)
                                                           else (((Seq.<|)) (DupRule nt con field attr2 attr)   e,                    m, fa:u,    bs)
                                           Nothing ->           (                                             e, Map.insert fa owrt m,    u, fa:bs)
                                  else                          (((Seq.<|)) (SuperfluousRule nt con field attr) e,                    m, fa:u,    bs)
                _              ->                               (((Seq.<|)) (UndefChild nt con field)           e,                    m, fa:u,    bs )

    in  fst (foldr checkRule (([],Seq.empty),Map.empty) rs)

checkRuleNames :: NontermIdent -> ConstructorIdent -> [RuleInfo] -> Seq Error
checkRuleNames nt con
  = fst . foldr checkRule (Seq.empty, Set.empty)
  where
    checkRule (Just nm,_,_,_,_,_,_,_) (errs, nms)
      | nm `Set.member` nms = (DupRuleName nt con nm Seq.<| errs, nms)
      | otherwise           = (errs, Set.insert nm nms)
    checkRule (Nothing,_,_,_,_,_,_,_) inp = inp

checkSigs :: NontermIdent -> ConstructorIdent -> [SigInfo] -> SigsAndErrors
checkSigs nt con sis
  = let checkSig (ide,typ) (sigs,errs)
         = if   ide `elem` map (\(TypeSig n t)-> n) sigs
           then (sigs, ((Seq.<|)) (DupSig nt con ide) errs)
           -- else if not (ide `elem` locattrdefs)
           -- then (sigs, ((Seq.<|)) (SupSig nt con ide) errs)
           else (TypeSig ide typ:sigs, errs)
    in  foldr checkSig ([],Seq.empty) sis

checkInsts :: Set NontermIdent -> Map NontermIdent (Map ConstructorIdent [SigInfo]) -> Map NontermIdent (Map ConstructorIdent [(Identifier, Type)]) -> NontermIdent -> ConstructorIdent -> [Identifier] -> InstsAndErrors
checkInsts allNts sigMap fieldMap nt con
  = foldr (\inst (insts, errs) ->
              maybe (insts, Seq.singleton (MissingInstSig nt con inst) >< errs)
                    (\info@(k, NT nm args _) ->
                      case findInst k insts of
                        Just k' -> (insts, Seq.singleton (DupChild nt con k k') >< errs)
                        Nothing -> case nm `Set.member` allNts of
                                             True  -> (info : insts, errs)
                                             False | take 2 (getName nm) == "T_" -> let nm'   = Ident (drop 2 (getName nm)) (getPos nm)
                                                                                        info' = (k, NT nm' args True)   -- this should be the only place at which 'for' with value True can be generated
                                                                                    in case nm' `Set.member` allNts of
                                                                                         True  -> (info' : insts, errs)
                                                                                         False -> (insts, Seq.singleton (UndefNont nm') >< errs)
                                                   | otherwise                   -> (insts, Seq.singleton (UndefNont nm) >< errs)
                    )
                  $ findSig inst
          ) ([], Seq.empty)
  where
    sigs = Map.findWithDefault [] con (Map.findWithDefault Map.empty nt sigMap)

    findSig name
      = do tp@(NT _ _ _) <- lookup name sigs
           return (name, tp)

    findInst _ [] = Nothing
    findInst k ((k', _): r)
      | k == k'   = Just k'
      | otherwise = findInst k r

checkUniques :: Map NontermIdent (Attributes, Attributes) -> NontermIdent -> ConstructorIdent -> [UniqueInfo] -> UniquesAndErrors
checkUniques allAttrs nt con uniques
  = let checkUnique (ident,ref) (us,errs)
          = if ident `Map.member` us
            then (us, ((Seq.<|)) (DupUnique nt con ident) errs)
            else if Map.member ref inhs && Map.member ref syns
                 then (Map.insert ident ref us, errs)
                 else (us, ((Seq.<|)) (MissingUnique nt ref) errs)

        (inhs,syns) = Map.findWithDefault (Map.empty,Map.empty) nt allAttrs
    in foldr checkUnique (Map.empty, Seq.empty) uniques

checkAugments :: Map NontermIdent (Attributes, Attributes) -> NontermIdent -> ConstructorIdent -> [AugmentInfo] -> AugmentsAndErrors
checkAugments allAttrs nt con augments
  = let checkAugment (ident,expr) (as,errs)
          = if ident `Map.member` as
            then (Map.update (\vs -> Just (vs ++ [expr])) ident as, errs)
            else if Map.member ident syns
                 then (Map.insert ident [expr] as, errs)
                 else (as, ((Seq.<|)) (MissingSyn nt ident) errs)

        (inhs,syns) = Map.findWithDefault (Map.empty,Map.empty) nt allAttrs
    in foldr checkAugment (Map.empty, Seq.empty) augments

checkArounds :: Map NontermIdent (Map ConstructorIdent [(Identifier, Type)]) -> NontermIdent -> ConstructorIdent -> [AroundInfo] -> AroundsAndErrors
checkArounds fieldMap nt con arounds
  = let checkAround (ident,expr) (as,errs)
          = if ident `Map.member` as
            then (Map.update (\vs -> Just (vs ++ [expr])) ident as, errs)
            else case lookup ident fields of
                   Just (NT _ _ _) -> (Map.insert ident [expr] as, errs)
                   _               -> (as, ((Seq.<|)) (UndefChild nt con ident) errs)
        fields = Map.findWithDefault [] con (Map.findWithDefault Map.empty nt fieldMap)
    in foldr checkAround (Map.empty, Seq.empty) arounds

checkMerges :: Set NontermIdent -> Map NontermIdent (Map ConstructorIdent [Identifier]) -> Map NontermIdent (Map ConstructorIdent [(Identifier, Type)]) -> NontermIdent -> ConstructorIdent -> [MergeInfo] -> MergesAndErrors
checkMerges allNts allInsts fieldMap nt con merges
  = let checkMerge (target,nt,sources,expr) (m,errs)
          = let fields = Map.findWithDefault [] con (Map.findWithDefault Map.empty nt fieldMap)
                insts  = Map.findWithDefault [] con (Map.findWithDefault Map.empty nt allInsts)
                allFields = insts ++ map fst fields   -- note: sources of merge may not contain a target (for simplicity)
            in if target `Map.member` m   -- check for duplicate with self
               then (m, DupChild nt con target (fst $ Map.elemAt (Map.findIndex target m) m) Seq.<| errs)
               else if target `elem` allFields
                     then (m, DupChild nt con target (head $ filter (== target) allFields) Seq.<| errs)
                     else let missing = filter (\s -> not (s `elem` allFields)) sources
                          in if null missing
                             then if nt `Set.member` allNts   -- check if the nonterm is defined
                                  then (Map.insert target (nt, sources, expr) m, errs) -- all ok..
                                  else (m, UndefNont nt Seq.<| errs)
                             else (m, (Seq.fromList $ map (UndefChild nt con) missing) Seq.>< errs)
    in foldr checkMerge (Map.empty, Seq.empty) merges

unionunionplusplus = Map.unionWith (Map.unionWith (++))
{-# LINE 272 "dist/build/Transform.hs" #-}

{-# LINE 507 "src-ag/Transform.ag" #-}

mkUniqueRules :: Options -> Map NontermIdent (Map ConstructorIdent [(Identifier, Type)]) -> Map NontermIdent (Map ConstructorIdent [(Identifier, Type)]) -> Map NontermIdent (Attributes,Attributes) -> NontermIdent -> ConstructorIdent -> Map Identifier Identifier -> [Rule]
mkUniqueRules opts allFields allInsts allAttrDecls nt con usMap
  = map apply groups
  where
    fields = Map.findWithDefault [] con (Map.findWithDefault Map.empty nt allFields)
             ++ Map.findWithDefault [] con (Map.findWithDefault Map.empty nt allInsts)
             -- may have duplicates

    groups = Map.assocs $ Map.foldrWithKey (\i r m -> Map.insertWith (++) r [i] m) Map.empty usMap
    apply (ref,us) = mkRule ref (findOutField ref) us
    findOutField ref = case [ chld | (chld, NT tp _ _) <- fields, tp `hasSyn` ref] of
                         []    -> _LHS
                         (x:_) -> x
    hasSyn tp ref = Map.member ref $ snd $ Map.findWithDefault (Map.empty,Map.empty) tp allAttrDecls
    mkRule ref outFld locAttrs
      = let pat = Product noPos (attr outFld ref : [attr _LOC u | u <- locAttrs ])
            rhs = Expression noPos $ wrap ref $ foldr gencase (finalout locAttrs) locAttrs
                     -- [HsToken ("mkUniques" ++ show (length locAttrs) ++ " ") noPos, AGField _LHS ref noPos Nothing]
        in Rule Nothing pat rhs False "-- generated by the unique rule mechanism." False True False Nothing False
    attr fld a = Alias fld a (Underscore noPos)
    gencase nm outp
      = h ("case " ++ uniqueDispenser opts ++ " __cont of { (__cont, " ++ getName nm ++ ") -> ") ++ outp ++ h "}"
    h s = [HsToken s noPos]
    finalout us = h ("(__cont, " ++ concat (intersperse "," (map getName us)) ++ ")")
    wrap ref inp = h "let __cont = " ++ [AGField _LHS ref noPos Nothing] ++ h " in seq __cont ( " ++ inp ++ h " )"
{-# LINE 301 "dist/build/Transform.hs" #-}

{-# LINE 732 "src-ag/Transform.ag" #-}

flattenDatas :: DataTypes -> Map NontermIdent (Set NontermIdent)
flattenDatas ds = Map.map flatten ds
  where flatten cs =  Set.fromList [ nt | (_, NT nt _ _) <- concatMap snd (Map.toList cs)]

reachableFrom :: Map NontermIdent (Set NontermIdent) -> Set NontermIdent -> Set NontermIdent
reachableFrom table nts = reach nts
  where reach nts = let nts' = Set.unions (nts : [ ns  | nt <- Set.toList nts
                                                 , let ns = Map.findWithDefault Set.empty nt table ])
                    in if Set.size nts' > Set.size nts
                          then reach nts'
                          else nts
invert :: Map NontermIdent (Set NontermIdent) -> Map NontermIdent (Set NontermIdent)
invert m = foldr inv Map.empty (Map.toList m)
  where inv (x,ns) m = fold (\n m -> Map.insertWith Set.union n (Set.singleton x) m) m ns

path :: Map NontermIdent (Set NontermIdent) -> NontermIdent -> NontermIdent -> Set NontermIdent
path table from to = let children = Map.findWithDefault Set.empty from table
                         forward  = reachableFrom table children
                         backward = reachableFrom (invert table)
                                                  (Set.singleton to)
                     in  Set.intersection forward backward
{-# LINE 326 "dist/build/Transform.hs" #-}

{-# LINE 857 "src-ag/Transform.ag" #-}

extract s = case dropWhile isSeparator s of
                                "" -> []
                                s' -> w : extract s''
                                      where (w, s'') = break isSeparator  s'
isSeparator x = x == '_'
{-# LINE 335 "dist/build/Transform.hs" #-}

{-# LINE 881 "src-ag/Transform.ag" #-}

pragmaMapUnion :: PragmaMap -> PragmaMap -> PragmaMap
pragmaMapUnion = Map.unionWith (Map.unionWith Set.union)

pragmaMapSingle :: NontermIdent -> ConstructorIdent -> Set Identifier -> PragmaMap
pragmaMapSingle nt con nms = Map.singleton nt (Map.singleton con nms)
{-# LINE 344 "dist/build/Transform.hs" #-}

{-# LINE 913 "src-ag/Transform.ag" #-}

orderMapUnion :: AttrOrderMap -> AttrOrderMap -> AttrOrderMap
orderMapUnion = Map.unionWith (Map.unionWith Set.union)

orderMapSingle :: NontermIdent -> ConstructorIdent -> Set Dependency -> AttrOrderMap
orderMapSingle nt con deps = Map.singleton nt (Map.singleton con deps)
{-# LINE 353 "dist/build/Transform.hs" #-}

{-# LINE 939 "src-ag/Transform.ag" #-}

mergeParams :: ParamMap -> ParamMap -> ParamMap
mergeParams = Map.unionWith (++)
{-# LINE 359 "dist/build/Transform.hs" #-}

{-# LINE 962 "src-ag/Transform.ag" #-}

mergeCtx :: ContextMap -> ContextMap -> ContextMap
mergeCtx
  = Map.unionWith nubconcat
  where nubconcat a b = nub (a ++ b)
{-# LINE 367 "dist/build/Transform.hs" #-}

{-# LINE 981 "src-ag/Transform.ag" #-}

mergeQuant :: QuantMap -> QuantMap -> QuantMap
mergeQuant = Map.unionWith (++)
{-# LINE 373 "dist/build/Transform.hs" #-}

{-# LINE 992 "src-ag/Transform.ag" #-}

mergeDerivings m1 m2 = foldr (\(n,cs) m -> Map.insertWith Set.union n cs m) m2 (Map.toList m1)
{-# LINE 378 "dist/build/Transform.hs" #-}

{-# LINE 1003 "src-ag/Transform.ag" #-}

merge x y = foldr f y (Map.toList x)
 where f ~(k,v) m = Map.insertWith (Map.union) k v m
{-# LINE 384 "dist/build/Transform.hs" #-}

{-# LINE 1045 "src-ag/Transform.ag" #-}

checkAttrs allFields nts inherited synthesized decls = foldErrors check decls nts where
  check nt decls | not (nt `Map.member` allFields) = (decls,Seq.singleton(UndefNont nt))
                 | otherwise = let (inh,syn) = Map.findWithDefault (Map.empty,Map.empty) nt decls
                                   (inh',einh) = checkDuplicates (DupInhAttr nt) inherited   inh
                                   (syn',esyn) = checkDuplicates (DupSynAttr nt) synthesized syn
                               in (Map.insert nt (inh',syn') decls,einh >< esyn)
{-# LINE 394 "dist/build/Transform.hs" #-}

{-# LINE 1056 "src-ag/Transform.ag" #-}

addSelf name atMap = let (eInh,eSyn) = Map.findWithDefault(Map.empty,Map.empty) name atMap
                     in  Map.insert name (eInh, Map.insert (Ident "self" noPos) Self eSyn)atMap
{-# LINE 400 "dist/build/Transform.hs" #-}

{-# LINE 1197 "src-ag/Transform.ag" #-}

makeType :: Set NontermIdent -> Type -> Type
makeType nts tp@(NT x _ _)   | Set.member x nts = tp
                             | otherwise        = Haskell (typeToHaskellString Nothing [] tp)
makeType _   tp                                 = tp
{-# LINE 408 "dist/build/Transform.hs" #-}

{-# LINE 1203 "src-ag/Transform.ag" #-}

constructGrammar ::    Set NontermIdent
                    -> ParamMap
                    -> Map NontermIdent (Map ConstructorIdent (Set Identifier))
                    -> DataTypes
                    -> Map NontermIdent (Map ConstructorIdent [Type])
                    -> Map NontermIdent (Attributes, Attributes)
                    -> Map NontermIdent (Map Identifier (String, String, String))
                    -> Derivings
                    -> Set NontermIdent
                    -> Map NontermIdent (Map ConstructorIdent [Rule])
                    -> Map NontermIdent (Map ConstructorIdent [TypeSig])
                    -> Map NontermIdent (Map ConstructorIdent [(Identifier, Type)])
                    -> TypeSyns
                    -> PragmaMap
                    -> AttrOrderMap
                    -> ContextMap
                    -> QuantMap
                    -> UniqueMap
                    -> Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
                    -> Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
                    -> Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))
                    -> Map NontermIdent (Map ConstructorIdent MaybeMacro)
                    -> Grammar

constructGrammar nts ntParams prodParams gram constraints attrs uses derivings wrappers allrules tsigs allinsts tsyns pragmaMap orderMap contextMap quantMap uniqueMap augmentsMap aroundsMap mergeMap macros =
   let gr = [ (nt,Map.toList alts) | (nt,alts) <- Map.toList gram]
       nonts = map nont gr
       nont (nt,alts) =  let (inh,syn) = Map.findWithDefault (Map.empty,Map.empty) nt attrs
                             rmap      = Map.findWithDefault Map.empty             nt allrules
                             tsmap     = Map.findWithDefault Map.empty             nt tsigs
                             instsmap  = Map.findWithDefault Map.empty             nt allinsts
                             params    = Map.findWithDefault []                    nt ntParams
                             mergemap  = Map.findWithDefault Map.empty             nt mergeMap
                             macromap  = Map.findWithDefault Map.empty             nt macros
                             csmap     = Map.findWithDefault Map.empty             nt constraints
                             psmap     = Map.findWithDefault Map.empty             nt prodParams
                             alt (con,flds) =
                                   let rules   = Map.findWithDefault [] con rmap
                                       tsigs   = Map.findWithDefault [] con tsmap
                                       insts   = Map.findWithDefault [] con instsmap
                                       merges  = [ (n, NT t [] False) | (n, (t, _, _)) <- Map.assocs $ maybe Map.empty id (Map.lookup con mergemap) ]
                                       cs      = Map.findWithDefault [] con csmap
                                       ps      = Set.elems $ Map.findWithDefault Set.empty con psmap
                                       mbMacro = Map.findWithDefault Nothing con macromap

                                       -- important: keep order of children
                                       cldrn = map child (flds ++ filter (not . existsAsField) insts ++ merges)
                                       child (nm, tp) =
                                          let tpI = if existsAsInst nm
                                                    then fromJust $ lookup nm insts
                                                    else tp
                                              virt = if existsAsInst nm
                                                     then case lookup nm flds of
                                                            Just tp' -> ChildReplace tp'
                                                            Nothing  -> ChildAttr
                                                     else if existsAsMerge nm
                                                          then ChildAttr
                                                          else ChildSyntax
                                          in Child nm tpI virt
                                       existsAsInst nm = maybe False (const True) (lookup nm insts)
                                       existsAsField (nm,_) = maybe False (const True) (lookup nm flds)
                                       existsAsMerge nm = maybe False (const True) (lookup nm merges)
                                   in Production con ps cs cldrn rules tsigs mbMacro
                            in Nonterminal nt params inh syn (map alt alts)
   in Grammar tsyns uses derivings wrappers nonts pragmaMap orderMap ntParams contextMap quantMap uniqueMap augmentsMap aroundsMap mergeMap
{-# LINE 477 "dist/build/Transform.hs" #-}

{-# LINE 1271 "src-ag/Transform.ag" #-}

mapUnionWithSetUnion = Map.unionWith Set.union
mapUnionWithPlusPlus = Map.unionWith (++)
{-# LINE 483 "dist/build/Transform.hs" #-}
-- AG ----------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attributes:
         agi                  : (Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes))
         blocks               : Blocks
         errors               : Seq Error
         moduleDecl           : Maybe (String,String,String)
         output               : Grammar
         pragmas              : Options -> Options
   alternatives:
      alternative AG:
         child elems          : Elems 
         visit 0:
            local allFields   : _
            local allConstraints : _
            local allConParams : _
            local allConstrs  : _
            local allRules    : _
            local allSigs     : _
            local allInsts    : _
            local allUniques  : _
            local allAugments : _
            local allArounds  : _
            local allMerges   : _
            local augmentSigs : _
            local allRulesErrs : _
            local allNamesErrs : _
            local allSigsErrs : _
            local allInstsErrs : _
            local allUniquesErrs : _
            local allAugmentErrs : _
            local allAroundsErrs : _
            local allMergesErrs : _
            local checkedRulesPre : _
            local checkedSigs : _
            local checkedInsts : _
            local checkedUniques : _
            local checkedAugments : _
            local checkedArounds : _
            local checkedRules : _
            local checkedMerges : _
            local errs1       : _
            local errs2       : _
            local errs3       : _
            local errs4       : _
            local errs5       : _
            local errs6       : _
            local errs7       : _
            local errs8       : _
            local errs9       : _
            local errs10      : _
            local errs11      : _
            local allNonterminals : _
            local allAttrDecls : _
            local allMacros   : _
            local allAttrs    : _
-}
-- cata
sem_AG :: AG ->
          T_AG
sem_AG (AG _elems) =
    (sem_AG_AG (sem_Elems _elems))
-- semantic domain
newtype T_AG = T_AG (Options ->
                     ( ((Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes))),Blocks,(Seq Error),(Maybe (String,String,String)),Grammar,(Options -> Options)))
data Inh_AG = Inh_AG {options_Inh_AG :: !(Options)}
data Syn_AG = Syn_AG {agi_Syn_AG :: !(((Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes)))),blocks_Syn_AG :: !(Blocks),errors_Syn_AG :: !((Seq Error)),moduleDecl_Syn_AG :: !((Maybe (String,String,String))),output_Syn_AG :: !(Grammar),pragmas_Syn_AG :: !((Options -> Options))}
wrap_AG :: T_AG ->
           Inh_AG ->
           Syn_AG
wrap_AG (T_AG sem) (Inh_AG _lhsIoptions) =
    (let ( _lhsOagi,_lhsOblocks,_lhsOerrors,_lhsOmoduleDecl,_lhsOoutput,_lhsOpragmas) = sem _lhsIoptions
     in  (Syn_AG _lhsOagi _lhsOblocks _lhsOerrors _lhsOmoduleDecl _lhsOoutput _lhsOpragmas))
sem_AG_AG :: T_Elems ->
             T_AG
sem_AG_AG (T_Elems elems_) =
    (T_AG (\ _lhsIoptions ->
               (let _lhsOoutput :: Grammar
                    _lhsOerrors :: (Seq Error)
                    _elemsOallConstructors :: (Map NontermIdent (Set ConstructorIdent))
                    _elemsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                    _elemsOdefinedSets :: DefinedSets
                    _elemsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                    _lhsOagi :: ((Set NontermIdent, DataTypes, Map NontermIdent (Attributes, Attributes)))
                    _elemsOattrs :: (Map NontermIdent (Attributes, Attributes))
                    _lhsOblocks :: Blocks
                    _lhsOmoduleDecl :: (Maybe (String,String,String))
                    _lhsOpragmas :: (Options -> Options)
                    _elemsOallAttrDecls :: (Map NontermIdent (Attributes, Attributes))
                    _elemsOallAttrs :: (Map NontermIdent (Attributes, Attributes))
                    _elemsOallFields :: DataTypes
                    _elemsOallNonterminals :: (Set NontermIdent)
                    _elemsOoptions :: Options
                    _elemsIattrDecls :: (Map NontermIdent (Attributes, Attributes))
                    _elemsIattrOrderCollect :: AttrOrderMap
                    _elemsIattrs :: (Map NontermIdent (Attributes, Attributes))
                    _elemsIblocks :: Blocks
                    _elemsIcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                    _elemsIcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                    _elemsIcollectedConParams :: ([(NontermIdent, ConstructorIdent, Set Identifier)])
                    _elemsIcollectedConstraints :: ([(NontermIdent, ConstructorIdent, [Type])])
                    _elemsIcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                    _elemsIcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                    _elemsIcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                    _elemsIcollectedMacros :: ([(NontermIdent, ConstructorIdent, MaybeMacro)])
                    _elemsIcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                    _elemsIcollectedNames :: (Set Identifier)
                    _elemsIcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                    _elemsIcollectedSetNames :: (Set Identifier)
                    _elemsIcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                    _elemsIcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                    _elemsIctxCollect :: ContextMap
                    _elemsIdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                    _elemsIderivings :: Derivings
                    _elemsIerrors :: (Seq Error)
                    _elemsImoduleDecl :: (Maybe (String,String,String))
                    _elemsIparamsCollect :: ParamMap
                    _elemsIpragmas :: (Options -> Options)
                    _elemsIquantCollect :: QuantMap
                    _elemsIsemPragmasCollect :: PragmaMap
                    _elemsItypeSyns :: TypeSyns
                    _elemsIuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                    _elemsIwrappers :: (Set NontermIdent)
                    -- "src-ag/Transform.ag"(line 53, column 8)
                    _lhsOoutput =
                        ({-# LINE 53 "src-ag/Transform.ag" #-}
                         constructGrammar _allNonterminals
                                          _elemsIparamsCollect
                                          _allConParams
                                          _allFields
                                          _allConstraints
                                          _allAttrDecls
                                          _elemsIuseMap
                                          _elemsIderivings
                                          (if wrappers _lhsIoptions then _allNonterminals     else _elemsIwrappers)
                                          _checkedRules
                                          _checkedSigs
                                          _checkedInsts
                                          _elemsItypeSyns
                                          _elemsIsemPragmasCollect
                                          _elemsIattrOrderCollect
                                          _elemsIctxCollect
                                          _elemsIquantCollect
                                          _checkedUniques
                                          _checkedAugments
                                          _checkedArounds
                                          _checkedMerges
                                          _allMacros
                         {-# LINE 635 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 260, column 10)
                    _allFields =
                        ({-# LINE 260 "src-ag/Transform.ag" #-}
                         let f (nt,con,fm) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con fm)
                         in  foldr f (Map.empty) _elemsIcollectedFields
                         {-# LINE 642 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 263, column 10)
                    _allConstraints =
                        ({-# LINE 263 "src-ag/Transform.ag" #-}
                         let f (nt,con,fm) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con fm)
                         in  foldr f (Map.empty) _elemsIcollectedConstraints
                         {-# LINE 649 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 266, column 10)
                    _allConParams =
                        ({-# LINE 266 "src-ag/Transform.ag" #-}
                         let f (nt,con,fm) = Map.insertWith (Map.unionWith Set.union) nt (Map.singleton con fm)
                         in  foldr f (Map.empty) _elemsIcollectedConParams
                         {-# LINE 656 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 269, column 10)
                    _allConstrs =
                        ({-# LINE 269 "src-ag/Transform.ag" #-}
                         let f (nt,con,_) = Map.insertWith (++) nt [con]
                         in  foldr f (Map.empty) _elemsIcollectedFields
                         {-# LINE 663 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 272, column 10)
                    _allRules =
                        ({-# LINE 272 "src-ag/Transform.ag" #-}
                         let f (nt,con,r) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con [r])
                         in  foldr f (Map.empty) _elemsIcollectedRules
                         {-# LINE 670 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 275, column 10)
                    _allSigs =
                        ({-# LINE 275 "src-ag/Transform.ag" #-}
                         let f (nt,con,t) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con [t])
                             typeof nt r = Map.findWithDefault (Haskell "<unknown>") r $ fst $ Map.findWithDefault (Map.empty,Map.empty) nt _allAttrDecls
                         in  foldr f (Map.empty) ( _elemsIcollectedSigs
                                                 ++ [ (nt, con, (ident,typeof nt ref))  | (nt, con, us) <- _elemsIcollectedUniques, (ident,ref) <- us ]
                                                 )
                         {-# LINE 680 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 281, column 10)
                    _allInsts =
                        ({-# LINE 281 "src-ag/Transform.ag" #-}
                         let f (nt,con,is) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con is)
                         in  foldr f (Map.empty) _elemsIcollectedInsts
                         {-# LINE 687 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 284, column 10)
                    _allUniques =
                        ({-# LINE 284 "src-ag/Transform.ag" #-}
                         let f (nt,con,us) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con us)
                         in foldr f (Map.empty) _elemsIcollectedUniques
                         {-# LINE 694 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 286, column 10)
                    _allAugments =
                        ({-# LINE 286 "src-ag/Transform.ag" #-}
                         let f (nt,con,as) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con as)
                         in foldr f Map.empty _elemsIcollectedAugments
                         {-# LINE 701 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 288, column 10)
                    _allArounds =
                        ({-# LINE 288 "src-ag/Transform.ag" #-}
                         let f (nt,con,as) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con as)
                         in foldr f Map.empty _elemsIcollectedArounds
                         {-# LINE 708 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 290, column 10)
                    _allMerges =
                        ({-# LINE 290 "src-ag/Transform.ag" #-}
                         let f (nt,con,as) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con as)
                          in foldr f Map.empty _elemsIcollectedMerges
                         {-# LINE 715 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 293, column 10)
                    _augmentSigs =
                        ({-# LINE 293 "src-ag/Transform.ag" #-}
                         let gen mp = []
                         in Map.map (Map.map gen) _allAugments
                         {-# LINE 722 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 296, column 10)
                    _allRulesErrs =
                        ({-# LINE 296 "src-ag/Transform.ag" #-}
                         Map.mapWithKey (Map.mapWithKey . (checkRules _allAttrDecls _allFields _allInsts _allSigs     _allMerges    )) _allRules
                         {-# LINE 728 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 297, column 10)
                    _allNamesErrs =
                        ({-# LINE 297 "src-ag/Transform.ag" #-}
                         Map.mapWithKey (Map.mapWithKey . checkRuleNames) _allRules
                         {-# LINE 734 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 298, column 10)
                    _allSigsErrs =
                        ({-# LINE 298 "src-ag/Transform.ag" #-}
                         Map.mapWithKey (Map.mapWithKey . (checkSigs                                                 )) _allSigs
                         {-# LINE 740 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 299, column 10)
                    _allInstsErrs =
                        ({-# LINE 299 "src-ag/Transform.ag" #-}
                         Map.mapWithKey (Map.mapWithKey . (checkInsts _allNonterminals     _allSigs     _allFields   )) _allInsts
                         {-# LINE 746 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 300, column 10)
                    _allUniquesErrs =
                        ({-# LINE 300 "src-ag/Transform.ag" #-}
                         Map.mapWithKey (Map.mapWithKey . (checkUniques _allAttrDecls                                )) _allUniques
                         {-# LINE 752 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 301, column 10)
                    _allAugmentErrs =
                        ({-# LINE 301 "src-ag/Transform.ag" #-}
                         Map.mapWithKey (Map.mapWithKey . (checkAugments _allAttrDecls                               )) _allAugments
                         {-# LINE 758 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 302, column 10)
                    _allAroundsErrs =
                        ({-# LINE 302 "src-ag/Transform.ag" #-}
                         Map.mapWithKey (Map.mapWithKey . (checkArounds _allFields    )) _allArounds
                         {-# LINE 764 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 303, column 10)
                    _allMergesErrs =
                        ({-# LINE 303 "src-ag/Transform.ag" #-}
                         Map.mapWithKey (Map.mapWithKey . (checkMerges _allNonterminals     _allInsts     _allFields    )) _allMerges
                         {-# LINE 770 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 305, column 10)
                    _checkedRulesPre =
                        ({-# LINE 305 "src-ag/Transform.ag" #-}
                         Map.map (Map.map fst) _allRulesErrs
                         {-# LINE 776 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 306, column 10)
                    _checkedSigs =
                        ({-# LINE 306 "src-ag/Transform.ag" #-}
                         Map.map (Map.map fst) _allSigsErrs     `unionunionplusplus` _augmentSigs
                         {-# LINE 782 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 307, column 10)
                    _checkedInsts =
                        ({-# LINE 307 "src-ag/Transform.ag" #-}
                         Map.map (Map.map fst) _allInstsErrs
                         {-# LINE 788 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 308, column 10)
                    _checkedUniques =
                        ({-# LINE 308 "src-ag/Transform.ag" #-}
                         Map.map (Map.map fst) _allUniquesErrs
                         {-# LINE 794 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 309, column 10)
                    _checkedAugments =
                        ({-# LINE 309 "src-ag/Transform.ag" #-}
                         Map.map (Map.map fst) _allAugmentErrs
                         {-# LINE 800 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 310, column 10)
                    _checkedArounds =
                        ({-# LINE 310 "src-ag/Transform.ag" #-}
                         Map.map (Map.map fst) _allAroundsErrs
                         {-# LINE 806 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 311, column 10)
                    _checkedRules =
                        ({-# LINE 311 "src-ag/Transform.ag" #-}
                         Map.unionWith (Map.unionWith (++)) _checkedRulesPre     (Map.mapWithKey (Map.mapWithKey . (mkUniqueRules _lhsIoptions _allFields     _checkedInsts     _allAttrDecls    )) _checkedUniques    )
                         {-# LINE 812 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 312, column 10)
                    _checkedMerges =
                        ({-# LINE 312 "src-ag/Transform.ag" #-}
                         Map.map (Map.map fst) _allMergesErrs
                         {-# LINE 818 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 314, column 10)
                    _errs1 =
                        ({-# LINE 314 "src-ag/Transform.ag" #-}
                         let f = checkForDuplicates (DupSynonym)
                         in  Seq.fromList . f . map fst $ _elemsItypeSyns
                         {-# LINE 825 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 317, column 10)
                    _errs2 =
                        ({-# LINE 317 "src-ag/Transform.ag" #-}
                         let g nt (con,fm) = checkForDuplicates (DupChild nt con) (map fst fm)
                             f (nt,cfm)    = concat . map (g nt) . Map.toList $ cfm
                         in  Seq.fromList . concat . map f . Map.toList $ _allFields
                         {-# LINE 833 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 321, column 10)
                    _errs3 =
                        ({-# LINE 321 "src-ag/Transform.ag" #-}
                         let f (nt,cons) = checkForDuplicates (DupAlt nt) cons
                         in   Seq.empty
                         {-# LINE 840 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 325, column 10)
                    _errs4 =
                        ({-# LINE 325 "src-ag/Transform.ag" #-}
                         let  f m s = Map.fold ((><) . snd) s m
                         in Map.fold f Seq.empty _allRulesErrs
                         {-# LINE 847 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 328, column 10)
                    _errs5 =
                        ({-# LINE 328 "src-ag/Transform.ag" #-}
                         let  f m s = Map.fold ((><) . snd) s m
                         in Map.fold f Seq.empty _allSigsErrs
                         {-# LINE 854 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 331, column 10)
                    _errs6 =
                        ({-# LINE 331 "src-ag/Transform.ag" #-}
                         let  f m s = Map.fold ((><) . snd) s m
                         in Map.fold f Seq.empty _allInstsErrs
                         {-# LINE 861 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 334, column 10)
                    _errs7 =
                        ({-# LINE 334 "src-ag/Transform.ag" #-}
                         let  f m s = Map.fold ((><) . snd) s m
                         in Map.fold f Seq.empty _allUniquesErrs
                         {-# LINE 868 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 337, column 10)
                    _errs8 =
                        ({-# LINE 337 "src-ag/Transform.ag" #-}
                         let  f m s = Map.fold ((><) . snd) s m
                         in Map.fold f Seq.empty _allAugmentErrs
                         {-# LINE 875 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 340, column 10)
                    _errs9 =
                        ({-# LINE 340 "src-ag/Transform.ag" #-}
                         let  f m s = Map.fold ((><) . snd) s m
                         in Map.fold f Seq.empty _allAroundsErrs
                         {-# LINE 882 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 343, column 10)
                    _errs10 =
                        ({-# LINE 343 "src-ag/Transform.ag" #-}
                         let  f m s = Map.fold ((><)) s m
                         in Map.fold f Seq.empty _allNamesErrs
                         {-# LINE 889 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 346, column 10)
                    _errs11 =
                        ({-# LINE 346 "src-ag/Transform.ag" #-}
                         let f m s = Map.fold ((><) . snd) s m
                         in Map.fold f Seq.empty _allMergesErrs
                         {-# LINE 896 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 349, column 10)
                    _lhsOerrors =
                        ({-# LINE 349 "src-ag/Transform.ag" #-}
                         _elemsIerrors >< _errs1 >< _errs2 >< _errs3 >< _errs4 >< _errs5 >< _errs6 >< _errs7 >< _errs8 >< _errs9 >< _errs10 >< _errs11
                         {-# LINE 902 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 591, column 10)
                    _allNonterminals =
                        ({-# LINE 591 "src-ag/Transform.ag" #-}
                         _elemsIcollectedNames `Set.difference` _elemsIcollectedSetNames
                         {-# LINE 908 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 611, column 8)
                    _elemsOallConstructors =
                        ({-# LINE 611 "src-ag/Transform.ag" #-}
                         _elemsIcollectedConstructorsMap
                         {-# LINE 914 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 694, column 8)
                    _elemsOdefSets =
                        ({-# LINE 694 "src-ag/Transform.ag" #-}
                         Map.fromList (map (\x->(x,(Set.singleton x, Set.empty))) (Set.toList _allNonterminals    ))
                         {-# LINE 920 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 695, column 8)
                    _elemsOdefinedSets =
                        ({-# LINE 695 "src-ag/Transform.ag" #-}
                         Map.map fst _elemsIdefSets
                         {-# LINE 926 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 1009, column 8)
                    _elemsOattrDecls =
                        ({-# LINE 1009 "src-ag/Transform.ag" #-}
                         Map.empty
                         {-# LINE 932 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 1063, column 9)
                    _allAttrDecls =
                        ({-# LINE 1063 "src-ag/Transform.ag" #-}
                         if withSelf _lhsIoptions
                          then foldr addSelf _elemsIattrDecls (Set.toList _allNonterminals    )
                          else               _elemsIattrDecls
                         {-# LINE 940 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 1296, column 10)
                    _allMacros =
                        ({-# LINE 1296 "src-ag/Transform.ag" #-}
                         let f (nt,con,m) = Map.insertWith (Map.union) nt (Map.singleton con m)
                         in  foldr f (Map.empty) _elemsIcollectedMacros
                         {-# LINE 947 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 1309, column 8)
                    _lhsOagi =
                        ({-# LINE 1309 "src-ag/Transform.ag" #-}
                         (_allNonterminals    ,_allFields    ,_allAttrs    )
                         {-# LINE 953 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 1311, column 8)
                    _allAttrs =
                        ({-# LINE 1311 "src-ag/Transform.ag" #-}
                         if withSelf _lhsIoptions
                              then foldr addSelf _elemsIattrs (Set.toList _allNonterminals    )
                              else               _elemsIattrs
                         {-# LINE 961 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 1319, column 9)
                    _elemsOattrs =
                        ({-# LINE 1319 "src-ag/Transform.ag" #-}
                         Map.empty
                         {-# LINE 967 "dist/build/Transform" #-}
                         )
                    -- use rule "src-ag/Transform.ag"(line 46, column 19)
                    _lhsOblocks =
                        ({-# LINE 46 "src-ag/Transform.ag" #-}
                         _elemsIblocks
                         {-# LINE 973 "dist/build/Transform" #-}
                         )
                    -- use rule "src-ag/Transform.ag"(line 1187, column 37)
                    _lhsOmoduleDecl =
                        ({-# LINE 1187 "src-ag/Transform.ag" #-}
                         _elemsImoduleDecl
                         {-# LINE 979 "dist/build/Transform" #-}
                         )
                    -- use rule "src-ag/Transform.ag"(line 787, column 34)
                    _lhsOpragmas =
                        ({-# LINE 787 "src-ag/Transform.ag" #-}
                         _elemsIpragmas
                         {-# LINE 985 "dist/build/Transform" #-}
                         )
                    -- copy rule (from local)
                    _elemsOallAttrDecls =
                        ({-# LINE 894 "src-ag/Transform.ag" #-}
                         _allAttrDecls
                         {-# LINE 991 "dist/build/Transform" #-}
                         )
                    -- copy rule (from local)
                    _elemsOallAttrs =
                        ({-# LINE 1306 "src-ag/Transform.ag" #-}
                         _allAttrs
                         {-# LINE 997 "dist/build/Transform" #-}
                         )
                    -- copy rule (from local)
                    _elemsOallFields =
                        ({-# LINE 136 "src-ag/Transform.ag" #-}
                         _allFields
                         {-# LINE 1003 "dist/build/Transform" #-}
                         )
                    -- copy rule (from local)
                    _elemsOallNonterminals =
                        ({-# LINE 93 "src-ag/Transform.ag" #-}
                         _allNonterminals
                         {-# LINE 1009 "dist/build/Transform" #-}
                         )
                    -- copy rule (down)
                    _elemsOoptions =
                        ({-# LINE 40 "src-ag/Transform.ag" #-}
                         _lhsIoptions
                         {-# LINE 1015 "dist/build/Transform" #-}
                         )
                    ( _elemsIattrDecls,_elemsIattrOrderCollect,_elemsIattrs,_elemsIblocks,_elemsIcollectedArounds,_elemsIcollectedAugments,_elemsIcollectedConParams,_elemsIcollectedConstraints,_elemsIcollectedConstructorsMap,_elemsIcollectedFields,_elemsIcollectedInsts,_elemsIcollectedMacros,_elemsIcollectedMerges,_elemsIcollectedNames,_elemsIcollectedRules,_elemsIcollectedSetNames,_elemsIcollectedSigs,_elemsIcollectedUniques,_elemsIctxCollect,_elemsIdefSets,_elemsIderivings,_elemsIerrors,_elemsImoduleDecl,_elemsIparamsCollect,_elemsIpragmas,_elemsIquantCollect,_elemsIsemPragmasCollect,_elemsItypeSyns,_elemsIuseMap,_elemsIwrappers) =
                        elems_ _elemsOallAttrDecls _elemsOallAttrs _elemsOallConstructors _elemsOallFields _elemsOallNonterminals _elemsOattrDecls _elemsOattrs _elemsOdefSets _elemsOdefinedSets _elemsOoptions
                    ___node =
                        (Syn_AG _lhsOagi _lhsOblocks _lhsOerrors _lhsOmoduleDecl _lhsOoutput _lhsOpragmas)
                in  ( _lhsOagi,_lhsOblocks,_lhsOerrors,_lhsOmoduleDecl,_lhsOoutput,_lhsOpragmas))))
-- Alt ---------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allConstructors      : Map NontermIdent (Set ConstructorIdent)
         allNonterminals      : Set NontermIdent
         nts                  : Set NontermIdent
      synthesized attributes:
         collectedConParams   : [(NontermIdent, ConstructorIdent, Set Identifier)]
         collectedConstraints : [(NontermIdent, ConstructorIdent, [Type])]
         collectedConstructorNames : Set ConstructorIdent
         collectedFields      : [(NontermIdent, ConstructorIdent, FieldMap)]
         collectedMacros      : [(NontermIdent, ConstructorIdent, MaybeMacro)]
   alternatives:
      alternative Alt:
         child pos            : {Pos}
         child names          : ConstructorSet 
         child tyvars         : {[Identifier]}
         child fields         : Fields 
         child macro          : {MaybeMacro}
-}
-- cata
sem_Alt :: Alt ->
           T_Alt
sem_Alt (Alt _pos _names _tyvars _fields _macro) =
    (sem_Alt_Alt _pos (sem_ConstructorSet _names) _tyvars (sem_Fields _fields) _macro)
-- semantic domain
newtype T_Alt = T_Alt ((Map NontermIdent (Set ConstructorIdent)) ->
                       (Set NontermIdent) ->
                       (Set NontermIdent) ->
                       ( ([(NontermIdent, ConstructorIdent, Set Identifier)]),([(NontermIdent, ConstructorIdent, [Type])]),(Set ConstructorIdent),([(NontermIdent, ConstructorIdent, FieldMap)]),([(NontermIdent, ConstructorIdent, MaybeMacro)])))
data Inh_Alt = Inh_Alt {allConstructors_Inh_Alt :: !((Map NontermIdent (Set ConstructorIdent))),allNonterminals_Inh_Alt :: !((Set NontermIdent)),nts_Inh_Alt :: !((Set NontermIdent))}
data Syn_Alt = Syn_Alt {collectedConParams_Syn_Alt :: !(([(NontermIdent, ConstructorIdent, Set Identifier)])),collectedConstraints_Syn_Alt :: !(([(NontermIdent, ConstructorIdent, [Type])])),collectedConstructorNames_Syn_Alt :: !((Set ConstructorIdent)),collectedFields_Syn_Alt :: !(([(NontermIdent, ConstructorIdent, FieldMap)])),collectedMacros_Syn_Alt :: !(([(NontermIdent, ConstructorIdent, MaybeMacro)]))}
wrap_Alt :: T_Alt ->
            Inh_Alt ->
            Syn_Alt
wrap_Alt (T_Alt sem) (Inh_Alt _lhsIallConstructors _lhsIallNonterminals _lhsInts) =
    (let ( _lhsOcollectedConParams,_lhsOcollectedConstraints,_lhsOcollectedConstructorNames,_lhsOcollectedFields,_lhsOcollectedMacros) = sem _lhsIallConstructors _lhsIallNonterminals _lhsInts
     in  (Syn_Alt _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorNames _lhsOcollectedFields _lhsOcollectedMacros))
sem_Alt_Alt :: Pos ->
               T_ConstructorSet ->
               ([Identifier]) ->
               T_Fields ->
               MaybeMacro ->
               T_Alt
sem_Alt_Alt pos_ (T_ConstructorSet names_) tyvars_ (T_Fields fields_) macro_ =
    (T_Alt (\ _lhsIallConstructors
              _lhsIallNonterminals
              _lhsInts ->
                (let _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                     _lhsOcollectedConstraints :: ([(NontermIdent, ConstructorIdent, [Type])])
                     _lhsOcollectedConParams :: ([(NontermIdent, ConstructorIdent, Set Identifier)])
                     _lhsOcollectedMacros :: ([(NontermIdent, ConstructorIdent, MaybeMacro)])
                     _lhsOcollectedConstructorNames :: (Set ConstructorIdent)
                     _fieldsOallNonterminals :: (Set NontermIdent)
                     _namesIcollectedConstructorNames :: (Set ConstructorIdent)
                     _namesIconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                     _namesIerrors :: (Seq Error)
                     _fieldsIcollectedConstraints :: ([Type])
                     _fieldsIcollectedFields :: ([(Identifier, Type)])
                     -- "src-ag/Transform.ag"(line 241, column 10)
                     _lhsOcollectedFields =
                         ({-# LINE 241 "src-ag/Transform.ag" #-}
                          [ (nt, con, _fieldsIcollectedFields)
                          | nt  <- Set.toList _lhsInts
                          , con <- Set.toList (_namesIconstructors (Map.findWithDefault Set.empty nt _lhsIallConstructors))
                          ]
                          {-# LINE 1089 "dist/build/Transform" #-}
                          )
                     -- "src-ag/Transform.ag"(line 245, column 10)
                     _lhsOcollectedConstraints =
                         ({-# LINE 245 "src-ag/Transform.ag" #-}
                          [ (nt, con, _fieldsIcollectedConstraints)
                          | nt  <- Set.toList _lhsInts
                          , con <- Set.toList (_namesIconstructors (Map.findWithDefault Set.empty nt _lhsIallConstructors))
                          ]
                          {-# LINE 1098 "dist/build/Transform" #-}
                          )
                     -- "src-ag/Transform.ag"(line 249, column 10)
                     _lhsOcollectedConParams =
                         ({-# LINE 249 "src-ag/Transform.ag" #-}
                          [ (nt, con, Set.fromList tyvars_)
                          | nt  <- Set.toList _lhsInts
                          , con <- Set.toList (_namesIconstructors (Map.findWithDefault Set.empty nt _lhsIallConstructors))
                          ]
                          {-# LINE 1107 "dist/build/Transform" #-}
                          )
                     -- "src-ag/Transform.ag"(line 1287, column 10)
                     _lhsOcollectedMacros =
                         ({-# LINE 1287 "src-ag/Transform.ag" #-}
                          [ (nt, con, macro_)
                          | nt  <- Set.toList _lhsInts
                          , con <- Set.toList (_namesIconstructors (Map.findWithDefault Set.empty nt _lhsIallConstructors))
                          ]
                          {-# LINE 1116 "dist/build/Transform" #-}
                          )
                     -- use rule "src-ag/Transform.ag"(line 98, column 62)
                     _lhsOcollectedConstructorNames =
                         ({-# LINE 98 "src-ag/Transform.ag" #-}
                          _namesIcollectedConstructorNames
                          {-# LINE 1122 "dist/build/Transform" #-}
                          )
                     -- copy rule (down)
                     _fieldsOallNonterminals =
                         ({-# LINE 93 "src-ag/Transform.ag" #-}
                          _lhsIallNonterminals
                          {-# LINE 1128 "dist/build/Transform" #-}
                          )
                     ( _namesIcollectedConstructorNames,_namesIconstructors,_namesIerrors) =
                         names_
                     ( _fieldsIcollectedConstraints,_fieldsIcollectedFields) =
                         fields_ _fieldsOallNonterminals
                     ___node =
                         (Syn_Alt _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorNames _lhsOcollectedFields _lhsOcollectedMacros)
                 in  ( _lhsOcollectedConParams,_lhsOcollectedConstraints,_lhsOcollectedConstructorNames,_lhsOcollectedFields,_lhsOcollectedMacros))))
-- Alts --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allConstructors      : Map NontermIdent (Set ConstructorIdent)
         allNonterminals      : Set NontermIdent
         nts                  : Set NontermIdent
      synthesized attributes:
         collectedConParams   : [(NontermIdent, ConstructorIdent, Set Identifier)]
         collectedConstraints : [(NontermIdent, ConstructorIdent, [Type])]
         collectedConstructorNames : Set ConstructorIdent
         collectedFields      : [(NontermIdent, ConstructorIdent, FieldMap)]
         collectedMacros      : [(NontermIdent, ConstructorIdent, MaybeMacro)]
   alternatives:
      alternative Cons:
         child hd             : Alt 
         child tl             : Alts 
      alternative Nil:
-}
-- cata
sem_Alts :: Alts ->
            T_Alts
sem_Alts list =
    (Prelude.foldr sem_Alts_Cons sem_Alts_Nil (Prelude.map sem_Alt list))
-- semantic domain
newtype T_Alts = T_Alts ((Map NontermIdent (Set ConstructorIdent)) ->
                         (Set NontermIdent) ->
                         (Set NontermIdent) ->
                         ( ([(NontermIdent, ConstructorIdent, Set Identifier)]),([(NontermIdent, ConstructorIdent, [Type])]),(Set ConstructorIdent),([(NontermIdent, ConstructorIdent, FieldMap)]),([(NontermIdent, ConstructorIdent, MaybeMacro)])))
data Inh_Alts = Inh_Alts {allConstructors_Inh_Alts :: !((Map NontermIdent (Set ConstructorIdent))),allNonterminals_Inh_Alts :: !((Set NontermIdent)),nts_Inh_Alts :: !((Set NontermIdent))}
data Syn_Alts = Syn_Alts {collectedConParams_Syn_Alts :: !(([(NontermIdent, ConstructorIdent, Set Identifier)])),collectedConstraints_Syn_Alts :: !(([(NontermIdent, ConstructorIdent, [Type])])),collectedConstructorNames_Syn_Alts :: !((Set ConstructorIdent)),collectedFields_Syn_Alts :: !(([(NontermIdent, ConstructorIdent, FieldMap)])),collectedMacros_Syn_Alts :: !(([(NontermIdent, ConstructorIdent, MaybeMacro)]))}
wrap_Alts :: T_Alts ->
             Inh_Alts ->
             Syn_Alts
wrap_Alts (T_Alts sem) (Inh_Alts _lhsIallConstructors _lhsIallNonterminals _lhsInts) =
    (let ( _lhsOcollectedConParams,_lhsOcollectedConstraints,_lhsOcollectedConstructorNames,_lhsOcollectedFields,_lhsOcollectedMacros) = sem _lhsIallConstructors _lhsIallNonterminals _lhsInts
     in  (Syn_Alts _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorNames _lhsOcollectedFields _lhsOcollectedMacros))
sem_Alts_Cons :: T_Alt ->
                 T_Alts ->
                 T_Alts
sem_Alts_Cons (T_Alt hd_) (T_Alts tl_) =
    (T_Alts (\ _lhsIallConstructors
               _lhsIallNonterminals
               _lhsInts ->
                 (let _lhsOcollectedConParams :: ([(NontermIdent, ConstructorIdent, Set Identifier)])
                      _lhsOcollectedConstraints :: ([(NontermIdent, ConstructorIdent, [Type])])
                      _lhsOcollectedConstructorNames :: (Set ConstructorIdent)
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedMacros :: ([(NontermIdent, ConstructorIdent, MaybeMacro)])
                      _hdOallConstructors :: (Map NontermIdent (Set ConstructorIdent))
                      _hdOallNonterminals :: (Set NontermIdent)
                      _hdOnts :: (Set NontermIdent)
                      _tlOallConstructors :: (Map NontermIdent (Set ConstructorIdent))
                      _tlOallNonterminals :: (Set NontermIdent)
                      _tlOnts :: (Set NontermIdent)
                      _hdIcollectedConParams :: ([(NontermIdent, ConstructorIdent, Set Identifier)])
                      _hdIcollectedConstraints :: ([(NontermIdent, ConstructorIdent, [Type])])
                      _hdIcollectedConstructorNames :: (Set ConstructorIdent)
                      _hdIcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _hdIcollectedMacros :: ([(NontermIdent, ConstructorIdent, MaybeMacro)])
                      _tlIcollectedConParams :: ([(NontermIdent, ConstructorIdent, Set Identifier)])
                      _tlIcollectedConstraints :: ([(NontermIdent, ConstructorIdent, [Type])])
                      _tlIcollectedConstructorNames :: (Set ConstructorIdent)
                      _tlIcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _tlIcollectedMacros :: ([(NontermIdent, ConstructorIdent, MaybeMacro)])
                      -- use rule "src-ag/Transform.ag"(line 132, column 31)
                      _lhsOcollectedConParams =
                          ({-# LINE 132 "src-ag/Transform.ag" #-}
                           _hdIcollectedConParams ++ _tlIcollectedConParams
                           {-# LINE 1206 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 131, column 33)
                      _lhsOcollectedConstraints =
                          ({-# LINE 131 "src-ag/Transform.ag" #-}
                           _hdIcollectedConstraints ++ _tlIcollectedConstraints
                           {-# LINE 1212 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 98, column 62)
                      _lhsOcollectedConstructorNames =
                          ({-# LINE 98 "src-ag/Transform.ag" #-}
                           _hdIcollectedConstructorNames `Set.union` _tlIcollectedConstructorNames
                           {-# LINE 1218 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 130, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 130 "src-ag/Transform.ag" #-}
                           _hdIcollectedFields ++ _tlIcollectedFields
                           {-# LINE 1224 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1283, column 28)
                      _lhsOcollectedMacros =
                          ({-# LINE 1283 "src-ag/Transform.ag" #-}
                           _hdIcollectedMacros ++ _tlIcollectedMacros
                           {-# LINE 1230 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _hdOallConstructors =
                          ({-# LINE 101 "src-ag/Transform.ag" #-}
                           _lhsIallConstructors
                           {-# LINE 1236 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _hdOallNonterminals =
                          ({-# LINE 93 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 1242 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _hdOnts =
                          ({-# LINE 175 "src-ag/Transform.ag" #-}
                           _lhsInts
                           {-# LINE 1248 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _tlOallConstructors =
                          ({-# LINE 101 "src-ag/Transform.ag" #-}
                           _lhsIallConstructors
                           {-# LINE 1254 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _tlOallNonterminals =
                          ({-# LINE 93 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 1260 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _tlOnts =
                          ({-# LINE 175 "src-ag/Transform.ag" #-}
                           _lhsInts
                           {-# LINE 1266 "dist/build/Transform" #-}
                           )
                      ( _hdIcollectedConParams,_hdIcollectedConstraints,_hdIcollectedConstructorNames,_hdIcollectedFields,_hdIcollectedMacros) =
                          hd_ _hdOallConstructors _hdOallNonterminals _hdOnts
                      ( _tlIcollectedConParams,_tlIcollectedConstraints,_tlIcollectedConstructorNames,_tlIcollectedFields,_tlIcollectedMacros) =
                          tl_ _tlOallConstructors _tlOallNonterminals _tlOnts
                      ___node =
                          (Syn_Alts _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorNames _lhsOcollectedFields _lhsOcollectedMacros)
                  in  ( _lhsOcollectedConParams,_lhsOcollectedConstraints,_lhsOcollectedConstructorNames,_lhsOcollectedFields,_lhsOcollectedMacros))))
sem_Alts_Nil :: T_Alts
sem_Alts_Nil =
    (T_Alts (\ _lhsIallConstructors
               _lhsIallNonterminals
               _lhsInts ->
                 (let _lhsOcollectedConParams :: ([(NontermIdent, ConstructorIdent, Set Identifier)])
                      _lhsOcollectedConstraints :: ([(NontermIdent, ConstructorIdent, [Type])])
                      _lhsOcollectedConstructorNames :: (Set ConstructorIdent)
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedMacros :: ([(NontermIdent, ConstructorIdent, MaybeMacro)])
                      -- use rule "src-ag/Transform.ag"(line 132, column 31)
                      _lhsOcollectedConParams =
                          ({-# LINE 132 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1289 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 131, column 33)
                      _lhsOcollectedConstraints =
                          ({-# LINE 131 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1295 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 98, column 62)
                      _lhsOcollectedConstructorNames =
                          ({-# LINE 98 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 1301 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 130, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 130 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1307 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1283, column 28)
                      _lhsOcollectedMacros =
                          ({-# LINE 1283 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1313 "dist/build/Transform" #-}
                           )
                      ___node =
                          (Syn_Alts _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorNames _lhsOcollectedFields _lhsOcollectedMacros)
                  in  ( _lhsOcollectedConParams,_lhsOcollectedConstraints,_lhsOcollectedConstructorNames,_lhsOcollectedFields,_lhsOcollectedMacros))))
-- Attrs -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allFields            : DataTypes
         allNonterminals      : Set NontermIdent
         nts                  : Set NontermIdent
         options              : Options
      chained attributes:
         attrDecls            : Map NontermIdent (Attributes, Attributes)
         attrs                : Map NontermIdent (Attributes, Attributes)
      synthesized attributes:
         errors               : Seq Error
         useMap               : Map NontermIdent (Map Identifier (String,String,String))
   alternatives:
      alternative Attrs:
         child pos            : {Pos}
         child inh            : {AttrNames}
         child chn            : {AttrNames}
         child syn            : {AttrNames}
         visit 0:
            local attrDecls   : _
            local errors      : _
            local inherited   : _
            local synthesized : _
            local useMap      : _
            local errors1     : _
-}
-- cata
sem_Attrs :: Attrs ->
             T_Attrs
sem_Attrs (Attrs _pos _inh _chn _syn) =
    (sem_Attrs_Attrs _pos _inh _chn _syn)
-- semantic domain
newtype T_Attrs = T_Attrs (DataTypes ->
                           (Set NontermIdent) ->
                           (Map NontermIdent (Attributes, Attributes)) ->
                           (Map NontermIdent (Attributes, Attributes)) ->
                           (Set NontermIdent) ->
                           Options ->
                           ( (Map NontermIdent (Attributes, Attributes)),(Map NontermIdent (Attributes, Attributes)),(Seq Error),(Map NontermIdent (Map Identifier (String,String,String)))))
data Inh_Attrs = Inh_Attrs {allFields_Inh_Attrs :: !(DataTypes),allNonterminals_Inh_Attrs :: !((Set NontermIdent)),attrDecls_Inh_Attrs :: !((Map NontermIdent (Attributes, Attributes))),attrs_Inh_Attrs :: !((Map NontermIdent (Attributes, Attributes))),nts_Inh_Attrs :: !((Set NontermIdent)),options_Inh_Attrs :: !(Options)}
data Syn_Attrs = Syn_Attrs {attrDecls_Syn_Attrs :: !((Map NontermIdent (Attributes, Attributes))),attrs_Syn_Attrs :: !((Map NontermIdent (Attributes, Attributes))),errors_Syn_Attrs :: !((Seq Error)),useMap_Syn_Attrs :: !((Map NontermIdent (Map Identifier (String,String,String))))}
wrap_Attrs :: T_Attrs ->
              Inh_Attrs ->
              Syn_Attrs
wrap_Attrs (T_Attrs sem) (Inh_Attrs _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsInts _lhsIoptions) =
    (let ( _lhsOattrDecls,_lhsOattrs,_lhsOerrors,_lhsOuseMap) = sem _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsInts _lhsIoptions
     in  (Syn_Attrs _lhsOattrDecls _lhsOattrs _lhsOerrors _lhsOuseMap))
sem_Attrs_Attrs :: Pos ->
                   AttrNames ->
                   AttrNames ->
                   AttrNames ->
                   T_Attrs
sem_Attrs_Attrs pos_ inh_ chn_ syn_ =
    (T_Attrs (\ _lhsIallFields
                _lhsIallNonterminals
                _lhsIattrDecls
                _lhsIattrs
                _lhsInts
                _lhsIoptions ->
                  (let _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                       _lhsOerrors :: (Seq Error)
                       _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                       _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                       -- "src-ag/Transform.ag"(line 1018, column 15)
                       (_attrDecls,_errors) =
                           ({-# LINE 1018 "src-ag/Transform.ag" #-}
                            checkAttrs _lhsIallFields (Set.toList _lhsInts) _inherited _synthesized _lhsIattrDecls
                            {-# LINE 1387 "dist/build/Transform" #-}
                            )
                       -- "src-ag/Transform.ag"(line 1020, column 15)
                       (_inherited,_synthesized,_useMap) =
                           ({-# LINE 1020 "src-ag/Transform.ag" #-}
                            let splitAttrs xs = unzip [ ((n,makeType _lhsIallNonterminals t),(n,ud))
                                                      | (n,t,ud) <- xs
                                                      ]
                                (inh,_)     = splitAttrs inh_
                                (chn,uses1) = splitAttrs chn_
                                (syn,uses2) = splitAttrs syn_
                                isUse (n,(e1,e2,_)) = not (null e1 || null e2)
                            in (inh++chn,chn++syn, Map.fromList (Prelude.filter isUse (uses1++uses2)))
                            {-# LINE 1400 "dist/build/Transform" #-}
                            )
                       -- "src-ag/Transform.ag"(line 1028, column 11)
                       _lhsOuseMap =
                           ({-# LINE 1028 "src-ag/Transform.ag" #-}
                            Map.fromList (zip (Set.toList _lhsInts) (repeat _useMap))
                            {-# LINE 1406 "dist/build/Transform" #-}
                            )
                       -- "src-ag/Transform.ag"(line 1030, column 11)
                       _errors1 =
                           ({-# LINE 1030 "src-ag/Transform.ag" #-}
                            if checkParseTy _lhsIoptions
                            then let attrs  = inh_ ++ syn_ ++ chn_
                                     items = map (\(ident,tp,_) -> (getPos ident, tp)) attrs
                                     errs  = map check items
                                     check (pos,Haskell s) =
                                       let exp = Expression pos tks
                                           tks = [tk]
                                           tk  = HsToken s pos
                                       in Seq.fromList $ checkTy exp
                                     check _ = Seq.empty
                                 in foldr (Seq.><) Seq.empty errs
                            else Seq.empty
                            {-# LINE 1423 "dist/build/Transform" #-}
                            )
                       -- "src-ag/Transform.ag"(line 1042, column 11)
                       _lhsOerrors =
                           ({-# LINE 1042 "src-ag/Transform.ag" #-}
                            _errors     Seq.>< _errors1
                            {-# LINE 1429 "dist/build/Transform" #-}
                            )
                       -- "src-ag/Transform.ag"(line 1323, column 11)
                       _lhsOattrs =
                           ({-# LINE 1323 "src-ag/Transform.ag" #-}
                            let insert decls nt = if Map.member nt decls
                                                    then  Map.update (\(inh,syn) -> Just ( Map.union inh $ Map.fromList _inherited
                                                                                         , Map.union syn $ Map.fromList _synthesized)) nt decls
                                                    else  Map.insert nt (Map.fromList _inherited, Map.fromList _synthesized) decls
                            in  foldl insert _lhsIattrs (Set.toList _lhsInts)
                            {-# LINE 1439 "dist/build/Transform" #-}
                            )
                       -- copy rule (from local)
                       _lhsOattrDecls =
                           ({-# LINE 144 "src-ag/Transform.ag" #-}
                            _attrDecls
                            {-# LINE 1445 "dist/build/Transform" #-}
                            )
                       ___node =
                           (Syn_Attrs _lhsOattrDecls _lhsOattrs _lhsOerrors _lhsOuseMap)
                   in  ( _lhsOattrDecls,_lhsOattrs,_lhsOerrors,_lhsOuseMap))))
-- ConstructorSet ----------------------------------------------
{-
   visit 0:
      synthesized attributes:
         collectedConstructorNames : Set ConstructorIdent
         constructors         : (Set ConstructorIdent->Set ConstructorIdent)
         errors               : Seq Error
   alternatives:
      alternative CAll:
      alternative CDifference:
         child set1           : ConstructorSet 
         child set2           : ConstructorSet 
      alternative CName:
         child name           : {ConstructorIdent}
      alternative CUnion:
         child set1           : ConstructorSet 
         child set2           : ConstructorSet 
-}
-- cata
sem_ConstructorSet :: ConstructorSet ->
                      T_ConstructorSet
sem_ConstructorSet (CAll) =
    (sem_ConstructorSet_CAll)
sem_ConstructorSet (CDifference _set1 _set2) =
    (sem_ConstructorSet_CDifference (sem_ConstructorSet _set1) (sem_ConstructorSet _set2))
sem_ConstructorSet (CName _name) =
    (sem_ConstructorSet_CName _name)
sem_ConstructorSet (CUnion _set1 _set2) =
    (sem_ConstructorSet_CUnion (sem_ConstructorSet _set1) (sem_ConstructorSet _set2))
-- semantic domain
newtype T_ConstructorSet = T_ConstructorSet (( (Set ConstructorIdent),((Set ConstructorIdent->Set ConstructorIdent)),(Seq Error)))
data Inh_ConstructorSet = Inh_ConstructorSet {}
data Syn_ConstructorSet = Syn_ConstructorSet {collectedConstructorNames_Syn_ConstructorSet :: !((Set ConstructorIdent)),constructors_Syn_ConstructorSet :: !(((Set ConstructorIdent->Set ConstructorIdent))),errors_Syn_ConstructorSet :: !((Seq Error))}
wrap_ConstructorSet :: T_ConstructorSet ->
                       Inh_ConstructorSet ->
                       Syn_ConstructorSet
wrap_ConstructorSet (T_ConstructorSet sem) (Inh_ConstructorSet) =
    (let ( _lhsOcollectedConstructorNames,_lhsOconstructors,_lhsOerrors) = sem
     in  (Syn_ConstructorSet _lhsOcollectedConstructorNames _lhsOconstructors _lhsOerrors))
sem_ConstructorSet_CAll :: T_ConstructorSet
sem_ConstructorSet_CAll =
    (T_ConstructorSet (let _lhsOconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _lhsOcollectedConstructorNames :: (Set ConstructorIdent)
                           _lhsOerrors :: (Seq Error)
                           -- "src-ag/Transform.ag"(line 765, column 17)
                           _lhsOconstructors =
                               ({-# LINE 765 "src-ag/Transform.ag" #-}
                                \ds -> ds
                                {-# LINE 1498 "dist/build/Transform" #-}
                                )
                           -- use rule "src-ag/Transform.ag"(line 98, column 62)
                           _lhsOcollectedConstructorNames =
                               ({-# LINE 98 "src-ag/Transform.ag" #-}
                                Set.empty
                                {-# LINE 1504 "dist/build/Transform" #-}
                                )
                           -- use rule "src-ag/Transform.ag"(line 44, column 19)
                           _lhsOerrors =
                               ({-# LINE 44 "src-ag/Transform.ag" #-}
                                Seq.empty
                                {-# LINE 1510 "dist/build/Transform" #-}
                                )
                           ___node =
                               (Syn_ConstructorSet _lhsOcollectedConstructorNames _lhsOconstructors _lhsOerrors)
                       in  ( _lhsOcollectedConstructorNames,_lhsOconstructors,_lhsOerrors)))
sem_ConstructorSet_CDifference :: T_ConstructorSet ->
                                  T_ConstructorSet ->
                                  T_ConstructorSet
sem_ConstructorSet_CDifference (T_ConstructorSet set1_) (T_ConstructorSet set2_) =
    (T_ConstructorSet (let _lhsOconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _lhsOcollectedConstructorNames :: (Set ConstructorIdent)
                           _lhsOerrors :: (Seq Error)
                           _set1IcollectedConstructorNames :: (Set ConstructorIdent)
                           _set1Iconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _set1Ierrors :: (Seq Error)
                           _set2IcollectedConstructorNames :: (Set ConstructorIdent)
                           _set2Iconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _set2Ierrors :: (Seq Error)
                           -- "src-ag/Transform.ag"(line 764, column 17)
                           _lhsOconstructors =
                               ({-# LINE 764 "src-ag/Transform.ag" #-}
                                \ds -> _set1Iconstructors ds `Set.difference` _set2Iconstructors ds
                                {-# LINE 1532 "dist/build/Transform" #-}
                                )
                           -- use rule "src-ag/Transform.ag"(line 98, column 62)
                           _lhsOcollectedConstructorNames =
                               ({-# LINE 98 "src-ag/Transform.ag" #-}
                                _set1IcollectedConstructorNames `Set.union` _set2IcollectedConstructorNames
                                {-# LINE 1538 "dist/build/Transform" #-}
                                )
                           -- use rule "src-ag/Transform.ag"(line 44, column 19)
                           _lhsOerrors =
                               ({-# LINE 44 "src-ag/Transform.ag" #-}
                                _set1Ierrors Seq.>< _set2Ierrors
                                {-# LINE 1544 "dist/build/Transform" #-}
                                )
                           ( _set1IcollectedConstructorNames,_set1Iconstructors,_set1Ierrors) =
                               set1_
                           ( _set2IcollectedConstructorNames,_set2Iconstructors,_set2Ierrors) =
                               set2_
                           ___node =
                               (Syn_ConstructorSet _lhsOcollectedConstructorNames _lhsOconstructors _lhsOerrors)
                       in  ( _lhsOcollectedConstructorNames,_lhsOconstructors,_lhsOerrors)))
sem_ConstructorSet_CName :: ConstructorIdent ->
                            T_ConstructorSet
sem_ConstructorSet_CName name_ =
    (T_ConstructorSet (let _lhsOcollectedConstructorNames :: (Set ConstructorIdent)
                           _lhsOconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _lhsOerrors :: (Seq Error)
                           -- "src-ag/Transform.ag"(line 599, column 11)
                           _lhsOcollectedConstructorNames =
                               ({-# LINE 599 "src-ag/Transform.ag" #-}
                                Set.singleton name_
                                {-# LINE 1563 "dist/build/Transform" #-}
                                )
                           -- "src-ag/Transform.ag"(line 762, column 17)
                           _lhsOconstructors =
                               ({-# LINE 762 "src-ag/Transform.ag" #-}
                                \ds -> Set.singleton name_
                                {-# LINE 1569 "dist/build/Transform" #-}
                                )
                           -- use rule "src-ag/Transform.ag"(line 44, column 19)
                           _lhsOerrors =
                               ({-# LINE 44 "src-ag/Transform.ag" #-}
                                Seq.empty
                                {-# LINE 1575 "dist/build/Transform" #-}
                                )
                           ___node =
                               (Syn_ConstructorSet _lhsOcollectedConstructorNames _lhsOconstructors _lhsOerrors)
                       in  ( _lhsOcollectedConstructorNames,_lhsOconstructors,_lhsOerrors)))
sem_ConstructorSet_CUnion :: T_ConstructorSet ->
                             T_ConstructorSet ->
                             T_ConstructorSet
sem_ConstructorSet_CUnion (T_ConstructorSet set1_) (T_ConstructorSet set2_) =
    (T_ConstructorSet (let _lhsOconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _lhsOcollectedConstructorNames :: (Set ConstructorIdent)
                           _lhsOerrors :: (Seq Error)
                           _set1IcollectedConstructorNames :: (Set ConstructorIdent)
                           _set1Iconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _set1Ierrors :: (Seq Error)
                           _set2IcollectedConstructorNames :: (Set ConstructorIdent)
                           _set2Iconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _set2Ierrors :: (Seq Error)
                           -- "src-ag/Transform.ag"(line 763, column 17)
                           _lhsOconstructors =
                               ({-# LINE 763 "src-ag/Transform.ag" #-}
                                \ds -> _set1Iconstructors ds `Set.union`      _set2Iconstructors ds
                                {-# LINE 1597 "dist/build/Transform" #-}
                                )
                           -- use rule "src-ag/Transform.ag"(line 98, column 62)
                           _lhsOcollectedConstructorNames =
                               ({-# LINE 98 "src-ag/Transform.ag" #-}
                                _set1IcollectedConstructorNames `Set.union` _set2IcollectedConstructorNames
                                {-# LINE 1603 "dist/build/Transform" #-}
                                )
                           -- use rule "src-ag/Transform.ag"(line 44, column 19)
                           _lhsOerrors =
                               ({-# LINE 44 "src-ag/Transform.ag" #-}
                                _set1Ierrors Seq.>< _set2Ierrors
                                {-# LINE 1609 "dist/build/Transform" #-}
                                )
                           ( _set1IcollectedConstructorNames,_set1Iconstructors,_set1Ierrors) =
                               set1_
                           ( _set2IcollectedConstructorNames,_set2Iconstructors,_set2Ierrors) =
                               set2_
                           ___node =
                               (Syn_ConstructorSet _lhsOcollectedConstructorNames _lhsOconstructors _lhsOerrors)
                       in  ( _lhsOcollectedConstructorNames,_lhsOconstructors,_lhsOerrors)))
-- Elem --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allAttrDecls         : Map NontermIdent (Attributes, Attributes)
         allAttrs             : Map NontermIdent (Attributes, Attributes)
         allConstructors      : Map NontermIdent (Set ConstructorIdent)
         allFields            : DataTypes
         allNonterminals      : Set NontermIdent
         definedSets          : DefinedSets
         options              : Options
      chained attributes:
         attrDecls            : Map NontermIdent (Attributes, Attributes)
         attrs                : Map NontermIdent (Attributes, Attributes)
         defSets              : Map Identifier (Set NontermIdent,Set Identifier)
      synthesized attributes:
         attrOrderCollect     : AttrOrderMap
         blocks               : Blocks
         collectedArounds     : [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         collectedAugments    : [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         collectedConParams   : [(NontermIdent, ConstructorIdent, Set Identifier)]
         collectedConstraints : [(NontermIdent, ConstructorIdent, [Type])]
         collectedConstructorsMap : Map NontermIdent (Set ConstructorIdent)
         collectedFields      : [(NontermIdent, ConstructorIdent, FieldMap)]
         collectedInsts       : [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         collectedMacros      : [(NontermIdent, ConstructorIdent, MaybeMacro)]
         collectedMerges      : [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         collectedNames       : Set Identifier
         collectedRules       : [ (NontermIdent, ConstructorIdent, RuleInfo)]
         collectedSetNames    : Set Identifier
         collectedSigs        : [ (NontermIdent, ConstructorIdent, SigInfo) ]
         collectedUniques     : [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         ctxCollect           : ContextMap
         derivings            : Derivings
         errors               : Seq Error
         moduleDecl           : Maybe (String,String,String)
         paramsCollect        : ParamMap
         pragmas              : Options -> Options
         quantCollect         : QuantMap
         semPragmasCollect    : PragmaMap
         typeSyns             : TypeSyns
         useMap               : Map NontermIdent (Map Identifier (String,String,String))
         wrappers             : Set NontermIdent
   alternatives:
      alternative Attr:
         child pos            : {Pos}
         child ctx            : {ClassContext}
         child names          : NontSet 
         child quants         : {[String]}
         child attrs          : Attrs 
      alternative Data:
         child pos            : {Pos}
         child ctx            : {ClassContext}
         child names          : NontSet 
         child params         : {[Identifier]}
         child attrs          : Attrs 
         child alts           : Alts 
         child ext            : {Bool}
      alternative Deriving:
         child pos            : {Pos}
         child set            : NontSet 
         child classes        : {[NontermIdent]}
      alternative Module:
         child pos            : {Pos}
         child name           : {String}
         child exports        : {String}
         child imports        : {String}
      alternative Nocatas:
         child pos            : {Pos}
         child set            : NontSet 
      alternative Pragma:
         child pos            : {Pos}
         child names          : {[NontermIdent]}
      alternative Sem:
         child pos            : {Pos}
         child ctx            : {ClassContext}
         child names          : NontSet 
         child attrs          : Attrs 
         child quants         : {[String]}
         child alts           : SemAlts 
      alternative Set:
         child pos            : {Pos}
         child name           : {NontermIdent}
         child merge          : {Bool}
         child set            : NontSet 
         visit 0:
            local defSets2    : _
            local errs        : _
      alternative Txt:
         child pos            : {Pos}
         child kind           : {BlockKind}
         child mbNt           : {Maybe NontermIdent}
         child lines          : {[String]}
         visit 0:
            local blockInfo   : _
            local blockValue  : _
      alternative Type:
         child pos            : {Pos}
         child ctx            : {ClassContext}
         child name           : {NontermIdent}
         child params         : {[Identifier]}
         child type           : {ComplexType}
         visit 0:
            local expanded    : _
            local argType     : _
      alternative Wrapper:
         child pos            : {Pos}
         child set            : NontSet 
-}
-- cata
sem_Elem :: Elem ->
            T_Elem
sem_Elem (Attr _pos _ctx _names _quants _attrs) =
    (sem_Elem_Attr _pos _ctx (sem_NontSet _names) _quants (sem_Attrs _attrs))
sem_Elem (Data _pos _ctx _names _params _attrs _alts _ext) =
    (sem_Elem_Data _pos _ctx (sem_NontSet _names) _params (sem_Attrs _attrs) (sem_Alts _alts) _ext)
sem_Elem (Deriving _pos _set _classes) =
    (sem_Elem_Deriving _pos (sem_NontSet _set) _classes)
sem_Elem (Module _pos _name _exports _imports) =
    (sem_Elem_Module _pos _name _exports _imports)
sem_Elem (Nocatas _pos _set) =
    (sem_Elem_Nocatas _pos (sem_NontSet _set))
sem_Elem (Pragma _pos _names) =
    (sem_Elem_Pragma _pos _names)
sem_Elem (Sem _pos _ctx _names _attrs _quants _alts) =
    (sem_Elem_Sem _pos _ctx (sem_NontSet _names) (sem_Attrs _attrs) _quants (sem_SemAlts _alts))
sem_Elem (Set _pos _name _merge _set) =
    (sem_Elem_Set _pos _name _merge (sem_NontSet _set))
sem_Elem (Txt _pos _kind _mbNt _lines) =
    (sem_Elem_Txt _pos _kind _mbNt _lines)
sem_Elem (Type _pos _ctx _name _params _type) =
    (sem_Elem_Type _pos _ctx _name _params _type)
sem_Elem (Wrapper _pos _set) =
    (sem_Elem_Wrapper _pos (sem_NontSet _set))
-- semantic domain
newtype T_Elem = T_Elem ((Map NontermIdent (Attributes, Attributes)) ->
                         (Map NontermIdent (Attributes, Attributes)) ->
                         (Map NontermIdent (Set ConstructorIdent)) ->
                         DataTypes ->
                         (Set NontermIdent) ->
                         (Map NontermIdent (Attributes, Attributes)) ->
                         (Map NontermIdent (Attributes, Attributes)) ->
                         (Map Identifier (Set NontermIdent,Set Identifier)) ->
                         DefinedSets ->
                         Options ->
                         ( (Map NontermIdent (Attributes, Attributes)),AttrOrderMap,(Map NontermIdent (Attributes, Attributes)),Blocks,([ (NontermIdent, ConstructorIdent, [AroundInfo])  ]),([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]),([(NontermIdent, ConstructorIdent, Set Identifier)]),([(NontermIdent, ConstructorIdent, [Type])]),(Map NontermIdent (Set ConstructorIdent)),([(NontermIdent, ConstructorIdent, FieldMap)]),([ (NontermIdent, ConstructorIdent, [Identifier]) ]),([(NontermIdent, ConstructorIdent, MaybeMacro)]),([ (NontermIdent, ConstructorIdent, [MergeInfo])   ]),(Set Identifier),([ (NontermIdent, ConstructorIdent, RuleInfo)]),(Set Identifier),([ (NontermIdent, ConstructorIdent, SigInfo) ]),([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]),ContextMap,(Map Identifier (Set NontermIdent,Set Identifier)),Derivings,(Seq Error),(Maybe (String,String,String)),ParamMap,(Options -> Options),QuantMap,PragmaMap,TypeSyns,(Map NontermIdent (Map Identifier (String,String,String))),(Set NontermIdent)))
data Inh_Elem = Inh_Elem {allAttrDecls_Inh_Elem :: !((Map NontermIdent (Attributes, Attributes))),allAttrs_Inh_Elem :: !((Map NontermIdent (Attributes, Attributes))),allConstructors_Inh_Elem :: !((Map NontermIdent (Set ConstructorIdent))),allFields_Inh_Elem :: !(DataTypes),allNonterminals_Inh_Elem :: !((Set NontermIdent)),attrDecls_Inh_Elem :: !((Map NontermIdent (Attributes, Attributes))),attrs_Inh_Elem :: !((Map NontermIdent (Attributes, Attributes))),defSets_Inh_Elem :: !((Map Identifier (Set NontermIdent,Set Identifier))),definedSets_Inh_Elem :: !(DefinedSets),options_Inh_Elem :: !(Options)}
data Syn_Elem = Syn_Elem {attrDecls_Syn_Elem :: !((Map NontermIdent (Attributes, Attributes))),attrOrderCollect_Syn_Elem :: !(AttrOrderMap),attrs_Syn_Elem :: !((Map NontermIdent (Attributes, Attributes))),blocks_Syn_Elem :: !(Blocks),collectedArounds_Syn_Elem :: !(([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])),collectedAugments_Syn_Elem :: !(([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])),collectedConParams_Syn_Elem :: !(([(NontermIdent, ConstructorIdent, Set Identifier)])),collectedConstraints_Syn_Elem :: !(([(NontermIdent, ConstructorIdent, [Type])])),collectedConstructorsMap_Syn_Elem :: !((Map NontermIdent (Set ConstructorIdent))),collectedFields_Syn_Elem :: !(([(NontermIdent, ConstructorIdent, FieldMap)])),collectedInsts_Syn_Elem :: !(([ (NontermIdent, ConstructorIdent, [Identifier]) ])),collectedMacros_Syn_Elem :: !(([(NontermIdent, ConstructorIdent, MaybeMacro)])),collectedMerges_Syn_Elem :: !(([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])),collectedNames_Syn_Elem :: !((Set Identifier)),collectedRules_Syn_Elem :: !(([ (NontermIdent, ConstructorIdent, RuleInfo)])),collectedSetNames_Syn_Elem :: !((Set Identifier)),collectedSigs_Syn_Elem :: !(([ (NontermIdent, ConstructorIdent, SigInfo) ])),collectedUniques_Syn_Elem :: !(([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])),ctxCollect_Syn_Elem :: !(ContextMap),defSets_Syn_Elem :: !((Map Identifier (Set NontermIdent,Set Identifier))),derivings_Syn_Elem :: !(Derivings),errors_Syn_Elem :: !((Seq Error)),moduleDecl_Syn_Elem :: !((Maybe (String,String,String))),paramsCollect_Syn_Elem :: !(ParamMap),pragmas_Syn_Elem :: !((Options -> Options)),quantCollect_Syn_Elem :: !(QuantMap),semPragmasCollect_Syn_Elem :: !(PragmaMap),typeSyns_Syn_Elem :: !(TypeSyns),useMap_Syn_Elem :: !((Map NontermIdent (Map Identifier (String,String,String)))),wrappers_Syn_Elem :: !((Set NontermIdent))}
wrap_Elem :: T_Elem ->
             Inh_Elem ->
             Syn_Elem
wrap_Elem (T_Elem sem) (Inh_Elem _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) =
    (let ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConParams,_lhsOcollectedConstraints,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMacros,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers) = sem _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions
     in  (Syn_Elem _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers))
sem_Elem_Attr :: Pos ->
                 ClassContext ->
                 T_NontSet ->
                 ([String]) ->
                 T_Attrs ->
                 T_Elem
sem_Elem_Attr pos_ ctx_ (T_NontSet names_) quants_ (T_Attrs attrs_) =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallAttrs
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIattrs
               _lhsIdefSets
               _lhsIdefinedSets
               _lhsIoptions ->
                 (let _lhsOctxCollect :: ContextMap
                      _lhsOquantCollect :: QuantMap
                      _attrsOnts :: (Set NontermIdent)
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _lhsOcollectedConParams :: ([(NontermIdent, ConstructorIdent, Set Identifier)])
                      _lhsOcollectedConstraints :: ([(NontermIdent, ConstructorIdent, [Type])])
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedMacros :: ([(NontermIdent, ConstructorIdent, MaybeMacro)])
                      _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      _namesOallFields :: DataTypes
                      _namesOallNonterminals :: (Set NontermIdent)
                      _namesOdefinedSets :: DefinedSets
                      _attrsOallFields :: DataTypes
                      _attrsOallNonterminals :: (Set NontermIdent)
                      _attrsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _attrsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _attrsOoptions :: Options
                      _namesIcollectedNames :: (Set Identifier)
                      _namesIerrors :: (Seq Error)
                      _namesInontSet :: (Set NontermIdent)
                      _attrsIattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _attrsIattrs :: (Map NontermIdent (Attributes, Attributes))
                      _attrsIerrors :: (Seq Error)
                      _attrsIuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      -- "src-ag/Transform.ag"(line 952, column 7)
                      _lhsOctxCollect =
                          ({-# LINE 952 "src-ag/Transform.ag" #-}
                           if null ctx_
                           then Map.empty
                           else Map.fromList [(nt, ctx_) | nt <- Set.toList _namesInontSet]
                           {-# LINE 1841 "dist/build/Transform" #-}
                           )
                      -- "src-ag/Transform.ag"(line 977, column 7)
                      _lhsOquantCollect =
                          ({-# LINE 977 "src-ag/Transform.ag" #-}
                           if null quants_
                           then Map.empty
                           else Map.fromList [(nt, quants_) | nt <- Set.toList _namesInontSet]
                           {-# LINE 1849 "dist/build/Transform" #-}
                           )
                      -- "src-ag/Transform.ag"(line 1013, column 10)
                      _attrsOnts =
                          ({-# LINE 1013 "src-ag/Transform.ag" #-}
                           _namesInontSet
                           {-# LINE 1855 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 893, column 55)
                      _lhsOattrOrderCollect =
                          ({-# LINE 893 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 1861 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 46, column 19)
                      _lhsOblocks =
                          ({-# LINE 46 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 1867 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 164, column 32)
                      _lhsOcollectedArounds =
                          ({-# LINE 164 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1873 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 163, column 32)
                      _lhsOcollectedAugments =
                          ({-# LINE 163 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1879 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 132, column 31)
                      _lhsOcollectedConParams =
                          ({-# LINE 132 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1885 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 131, column 33)
                      _lhsOcollectedConstraints =
                          ({-# LINE 131 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1891 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 99, column 48)
                      _lhsOcollectedConstructorsMap =
                          ({-# LINE 99 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 1897 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 130, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 130 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1903 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 161, column 32)
                      _lhsOcollectedInsts =
                          ({-# LINE 161 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1909 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1283, column 28)
                      _lhsOcollectedMacros =
                          ({-# LINE 1283 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1915 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 165, column 32)
                      _lhsOcollectedMerges =
                          ({-# LINE 165 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1921 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 91, column 50)
                      _lhsOcollectedNames =
                          ({-# LINE 91 "src-ag/Transform.ag" #-}
                           _namesIcollectedNames
                           {-# LINE 1927 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 159, column 32)
                      _lhsOcollectedRules =
                          ({-# LINE 159 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1933 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 90, column 50)
                      _lhsOcollectedSetNames =
                          ({-# LINE 90 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 1939 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 160, column 32)
                      _lhsOcollectedSigs =
                          ({-# LINE 160 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1945 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 162, column 32)
                      _lhsOcollectedUniques =
                          ({-# LINE 162 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1951 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 990, column 33)
                      _lhsOderivings =
                          ({-# LINE 990 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 1957 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 44, column 19)
                      _lhsOerrors =
                          ({-# LINE 44 "src-ag/Transform.ag" #-}
                           _namesIerrors Seq.>< _attrsIerrors
                           {-# LINE 1963 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1187, column 37)
                      _lhsOmoduleDecl =
                          ({-# LINE 1187 "src-ag/Transform.ag" #-}
                           mzero
                           {-# LINE 1969 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 925, column 37)
                      _lhsOparamsCollect =
                          ({-# LINE 925 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 1975 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 787, column 34)
                      _lhsOpragmas =
                          ({-# LINE 787 "src-ag/Transform.ag" #-}
                           id
                           {-# LINE 1981 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 865, column 56)
                      _lhsOsemPragmasCollect =
                          ({-# LINE 865 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 1987 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 626, column 32)
                      _lhsOtypeSyns =
                          ({-# LINE 626 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 1993 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 145, column 15)
                      _lhsOuseMap =
                          ({-# LINE 145 "src-ag/Transform.ag" #-}
                           _attrsIuseMap
                           {-# LINE 1999 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 771, column 32)
                      _lhsOwrappers =
                          ({-# LINE 771 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 2005 "dist/build/Transform" #-}
                           )
                      -- copy rule (up)
                      _lhsOattrDecls =
                          ({-# LINE 144 "src-ag/Transform.ag" #-}
                           _attrsIattrDecls
                           {-# LINE 2011 "dist/build/Transform" #-}
                           )
                      -- copy rule (up)
                      _lhsOattrs =
                          ({-# LINE 1316 "src-ag/Transform.ag" #-}
                           _attrsIattrs
                           {-# LINE 2017 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOdefSets =
                          ({-# LINE 109 "src-ag/Transform.ag" #-}
                           _lhsIdefSets
                           {-# LINE 2023 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _namesOallFields =
                          ({-# LINE 136 "src-ag/Transform.ag" #-}
                           _lhsIallFields
                           {-# LINE 2029 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _namesOallNonterminals =
                          ({-# LINE 93 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 2035 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _namesOdefinedSets =
                          ({-# LINE 112 "src-ag/Transform.ag" #-}
                           _lhsIdefinedSets
                           {-# LINE 2041 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _attrsOallFields =
                          ({-# LINE 136 "src-ag/Transform.ag" #-}
                           _lhsIallFields
                           {-# LINE 2047 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _attrsOallNonterminals =
                          ({-# LINE 93 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 2053 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _attrsOattrDecls =
                          ({-# LINE 144 "src-ag/Transform.ag" #-}
                           _lhsIattrDecls
                           {-# LINE 2059 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _attrsOattrs =
                          ({-# LINE 1316 "src-ag/Transform.ag" #-}
                           _lhsIattrs
                           {-# LINE 2065 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _attrsOoptions =
                          ({-# LINE 40 "src-ag/Transform.ag" #-}
                           _lhsIoptions
                           {-# LINE 2071 "dist/build/Transform" #-}
                           )
                      ( _namesIcollectedNames,_namesIerrors,_namesInontSet) =
                          names_ _namesOallFields _namesOallNonterminals _namesOdefinedSets
                      ( _attrsIattrDecls,_attrsIattrs,_attrsIerrors,_attrsIuseMap) =
                          attrs_ _attrsOallFields _attrsOallNonterminals _attrsOattrDecls _attrsOattrs _attrsOnts _attrsOoptions
                      ___node =
                          (Syn_Elem _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers)
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConParams,_lhsOcollectedConstraints,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMacros,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))))
sem_Elem_Data :: Pos ->
                 ClassContext ->
                 T_NontSet ->
                 ([Identifier]) ->
                 T_Attrs ->
                 T_Alts ->
                 Bool ->
                 T_Elem
sem_Elem_Data pos_ ctx_ (T_NontSet names_) params_ (T_Attrs attrs_) (T_Alts alts_) ext_ =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallAttrs
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIattrs
               _lhsIdefSets
               _lhsIdefinedSets
               _lhsIoptions ->
                 (let _altsOnts :: (Set NontermIdent)
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOctxCollect :: ContextMap
                      _attrsOnts :: (Set NontermIdent)
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _lhsOcollectedConParams :: ([(NontermIdent, ConstructorIdent, Set Identifier)])
                      _lhsOcollectedConstraints :: ([(NontermIdent, ConstructorIdent, [Type])])
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedMacros :: ([(NontermIdent, ConstructorIdent, MaybeMacro)])
                      _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOquantCollect :: QuantMap
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      _namesOallFields :: DataTypes
                      _namesOallNonterminals :: (Set NontermIdent)
                      _namesOdefinedSets :: DefinedSets
                      _attrsOallFields :: DataTypes
                      _attrsOallNonterminals :: (Set NontermIdent)
                      _attrsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _attrsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _attrsOoptions :: Options
                      _altsOallConstructors :: (Map NontermIdent (Set ConstructorIdent))
                      _altsOallNonterminals :: (Set NontermIdent)
                      _namesIcollectedNames :: (Set Identifier)
                      _namesIerrors :: (Seq Error)
                      _namesInontSet :: (Set NontermIdent)
                      _attrsIattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _attrsIattrs :: (Map NontermIdent (Attributes, Attributes))
                      _attrsIerrors :: (Seq Error)
                      _attrsIuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _altsIcollectedConParams :: ([(NontermIdent, ConstructorIdent, Set Identifier)])
                      _altsIcollectedConstraints :: ([(NontermIdent, ConstructorIdent, [Type])])
                      _altsIcollectedConstructorNames :: (Set ConstructorIdent)
                      _altsIcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _altsIcollectedMacros :: ([(NontermIdent, ConstructorIdent, MaybeMacro)])
                      -- "src-ag/Transform.ag"(line 178, column 10)
                      _altsOnts =
                          ({-# LINE 178 "src-ag/Transform.ag" #-}
                           _namesInontSet
                           {-# LINE 2157 "dist/build/Transform" #-}
                           )
                      -- "src-ag/Transform.ag"(line 605, column 11)
                      _lhsOcollectedConstructorsMap =
                          ({-# LINE 605 "src-ag/Transform.ag" #-}
                           Map.fromList
                           [ (n, _altsIcollectedConstructorNames)
                           | n <- Set.toList _namesInontSet
                           ]
                           {-# LINE 2166 "dist/build/Transform" #-}
                           )
                      -- "src-ag/Transform.ag"(line 929, column 7)
                      _lhsOparamsCollect =
                          ({-# LINE 929 "src-ag/Transform.ag" #-}
                           if null params_
                           then Map.empty
                           else Map.fromList [(nt, params_) | nt <- Set.toList _namesInontSet]
                           {-# LINE 2174 "dist/build/Transform" #-}
                           )
                      -- "src-ag/Transform.ag"(line 952, column 7)
                      _lhsOctxCollect =
                          ({-# LINE 952 "src-ag/Transform.ag" #-}
                           if null ctx_
                           then Map.empty
                           else Map.fromList [(nt, ctx_) | nt <- Set.toList _namesInontSet]
                           {-# LINE 2182 "dist/build/Transform" #-}
                           )
                      -- "src-ag/Transform.ag"(line 1012, column 10)
                      _attrsOnts =
                          ({-# LINE 1012 "src-ag/Transform.ag" #-}
                           _namesInontSet
                           {-# LINE 2188 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 893, column 55)
                      _lhsOattrOrderCollect =
                          ({-# LINE 893 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2194 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 46, column 19)
                      _lhsOblocks =
                          ({-# LINE 46 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2200 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 164, column 32)
                      _lhsOcollectedArounds =
                          ({-# LINE 164 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2206 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 163, column 32)
                      _lhsOcollectedAugments =
                          ({-# LINE 163 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2212 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 132, column 31)
                      _lhsOcollectedConParams =
                          ({-# LINE 132 "src-ag/Transform.ag" #-}
                           _altsIcollectedConParams
                           {-# LINE 2218 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 131, column 33)
                      _lhsOcollectedConstraints =
                          ({-# LINE 131 "src-ag/Transform.ag" #-}
                           _altsIcollectedConstraints
                           {-# LINE 2224 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 130, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 130 "src-ag/Transform.ag" #-}
                           _altsIcollectedFields
                           {-# LINE 2230 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 161, column 32)
                      _lhsOcollectedInsts =
                          ({-# LINE 161 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2236 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1283, column 28)
                      _lhsOcollectedMacros =
                          ({-# LINE 1283 "src-ag/Transform.ag" #-}
                           _altsIcollectedMacros
                           {-# LINE 2242 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 165, column 32)
                      _lhsOcollectedMerges =
                          ({-# LINE 165 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2248 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 91, column 50)
                      _lhsOcollectedNames =
                          ({-# LINE 91 "src-ag/Transform.ag" #-}
                           _namesIcollectedNames
                           {-# LINE 2254 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 159, column 32)
                      _lhsOcollectedRules =
                          ({-# LINE 159 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2260 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 90, column 50)
                      _lhsOcollectedSetNames =
                          ({-# LINE 90 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 2266 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 160, column 32)
                      _lhsOcollectedSigs =
                          ({-# LINE 160 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2272 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 162, column 32)
                      _lhsOcollectedUniques =
                          ({-# LINE 162 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2278 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 990, column 33)
                      _lhsOderivings =
                          ({-# LINE 990 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2284 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 44, column 19)
                      _lhsOerrors =
                          ({-# LINE 44 "src-ag/Transform.ag" #-}
                           _namesIerrors Seq.>< _attrsIerrors
                           {-# LINE 2290 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1187, column 37)
                      _lhsOmoduleDecl =
                          ({-# LINE 1187 "src-ag/Transform.ag" #-}
                           mzero
                           {-# LINE 2296 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 787, column 34)
                      _lhsOpragmas =
                          ({-# LINE 787 "src-ag/Transform.ag" #-}
                           id
                           {-# LINE 2302 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 973, column 36)
                      _lhsOquantCollect =
                          ({-# LINE 973 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2308 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 865, column 56)
                      _lhsOsemPragmasCollect =
                          ({-# LINE 865 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2314 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 626, column 32)
                      _lhsOtypeSyns =
                          ({-# LINE 626 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2320 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 145, column 15)
                      _lhsOuseMap =
                          ({-# LINE 145 "src-ag/Transform.ag" #-}
                           _attrsIuseMap
                           {-# LINE 2326 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 771, column 32)
                      _lhsOwrappers =
                          ({-# LINE 771 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 2332 "dist/build/Transform" #-}
                           )
                      -- copy rule (up)
                      _lhsOattrDecls =
                          ({-# LINE 144 "src-ag/Transform.ag" #-}
                           _attrsIattrDecls
                           {-# LINE 2338 "dist/build/Transform" #-}
                           )
                      -- copy rule (up)
                      _lhsOattrs =
                          ({-# LINE 1316 "src-ag/Transform.ag" #-}
                           _attrsIattrs
                           {-# LINE 2344 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOdefSets =
                          ({-# LINE 109 "src-ag/Transform.ag" #-}
                           _lhsIdefSets
                           {-# LINE 2350 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _namesOallFields =
                          ({-# LINE 136 "src-ag/Transform.ag" #-}
                           _lhsIallFields
                           {-# LINE 2356 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _namesOallNonterminals =
                          ({-# LINE 93 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 2362 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _namesOdefinedSets =
                          ({-# LINE 112 "src-ag/Transform.ag" #-}
                           _lhsIdefinedSets
                           {-# LINE 2368 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _attrsOallFields =
                          ({-# LINE 136 "src-ag/Transform.ag" #-}
                           _lhsIallFields
                           {-# LINE 2374 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _attrsOallNonterminals =
                          ({-# LINE 93 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 2380 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _attrsOattrDecls =
                          ({-# LINE 144 "src-ag/Transform.ag" #-}
                           _lhsIattrDecls
                           {-# LINE 2386 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _attrsOattrs =
                          ({-# LINE 1316 "src-ag/Transform.ag" #-}
                           _lhsIattrs
                           {-# LINE 2392 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _attrsOoptions =
                          ({-# LINE 40 "src-ag/Transform.ag" #-}
                           _lhsIoptions
                           {-# LINE 2398 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _altsOallConstructors =
                          ({-# LINE 101 "src-ag/Transform.ag" #-}
                           _lhsIallConstructors
                           {-# LINE 2404 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _altsOallNonterminals =
                          ({-# LINE 93 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 2410 "dist/build/Transform" #-}
                           )
                      ( _namesIcollectedNames,_namesIerrors,_namesInontSet) =
                          names_ _namesOallFields _namesOallNonterminals _namesOdefinedSets
                      ( _attrsIattrDecls,_attrsIattrs,_attrsIerrors,_attrsIuseMap) =
                          attrs_ _attrsOallFields _attrsOallNonterminals _attrsOattrDecls _attrsOattrs _attrsOnts _attrsOoptions
                      ( _altsIcollectedConParams,_altsIcollectedConstraints,_altsIcollectedConstructorNames,_altsIcollectedFields,_altsIcollectedMacros) =
                          alts_ _altsOallConstructors _altsOallNonterminals _altsOnts
                      ___node =
                          (Syn_Elem _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers)
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConParams,_lhsOcollectedConstraints,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMacros,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))))
sem_Elem_Deriving :: Pos ->
                     T_NontSet ->
                     ([NontermIdent]) ->
                     T_Elem
sem_Elem_Deriving pos_ (T_NontSet set_) classes_ =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallAttrs
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIattrs
               _lhsIdefSets
               _lhsIdefinedSets
               _lhsIoptions ->
                 (let _lhsOderivings :: Derivings
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _lhsOcollectedConParams :: ([(NontermIdent, ConstructorIdent, Set Identifier)])
                      _lhsOcollectedConstraints :: ([(NontermIdent, ConstructorIdent, [Type])])
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedMacros :: ([(NontermIdent, ConstructorIdent, MaybeMacro)])
                      _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOctxCollect :: ContextMap
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOquantCollect :: QuantMap
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      _setOallFields :: DataTypes
                      _setOallNonterminals :: (Set NontermIdent)
                      _setOdefinedSets :: DefinedSets
                      _setIcollectedNames :: (Set Identifier)
                      _setIerrors :: (Seq Error)
                      _setInontSet :: (Set NontermIdent)
                      -- "src-ag/Transform.ag"(line 997, column 14)
                      _lhsOderivings =
                          ({-# LINE 997 "src-ag/Transform.ag" #-}
                           Map.fromList [(nt,Set.fromList classes_) | nt <- Set.toList _setInontSet]
                           {-# LINE 2476 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 893, column 55)
                      _lhsOattrOrderCollect =
                          ({-# LINE 893 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2482 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 46, column 19)
                      _lhsOblocks =
                          ({-# LINE 46 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2488 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 164, column 32)
                      _lhsOcollectedArounds =
                          ({-# LINE 164 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2494 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 163, column 32)
                      _lhsOcollectedAugments =
                          ({-# LINE 163 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2500 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 132, column 31)
                      _lhsOcollectedConParams =
                          ({-# LINE 132 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2506 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 131, column 33)
                      _lhsOcollectedConstraints =
                          ({-# LINE 131 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2512 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 99, column 48)
                      _lhsOcollectedConstructorsMap =
                          ({-# LINE 99 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2518 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 130, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 130 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2524 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 161, column 32)
                      _lhsOcollectedInsts =
                          ({-# LINE 161 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2530 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1283, column 28)
                      _lhsOcollectedMacros =
                          ({-# LINE 1283 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2536 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 165, column 32)
                      _lhsOcollectedMerges =
                          ({-# LINE 165 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2542 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 91, column 50)
                      _lhsOcollectedNames =
                          ({-# LINE 91 "src-ag/Transform.ag" #-}
                           _setIcollectedNames
                           {-# LINE 2548 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 159, column 32)
                      _lhsOcollectedRules =
                          ({-# LINE 159 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2554 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 90, column 50)
                      _lhsOcollectedSetNames =
                          ({-# LINE 90 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 2560 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 160, column 32)
                      _lhsOcollectedSigs =
                          ({-# LINE 160 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2566 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 162, column 32)
                      _lhsOcollectedUniques =
                          ({-# LINE 162 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2572 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 948, column 34)
                      _lhsOctxCollect =
                          ({-# LINE 948 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2578 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 44, column 19)
                      _lhsOerrors =
                          ({-# LINE 44 "src-ag/Transform.ag" #-}
                           _setIerrors
                           {-# LINE 2584 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1187, column 37)
                      _lhsOmoduleDecl =
                          ({-# LINE 1187 "src-ag/Transform.ag" #-}
                           mzero
                           {-# LINE 2590 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 925, column 37)
                      _lhsOparamsCollect =
                          ({-# LINE 925 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2596 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 787, column 34)
                      _lhsOpragmas =
                          ({-# LINE 787 "src-ag/Transform.ag" #-}
                           id
                           {-# LINE 2602 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 973, column 36)
                      _lhsOquantCollect =
                          ({-# LINE 973 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2608 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 865, column 56)
                      _lhsOsemPragmasCollect =
                          ({-# LINE 865 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2614 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 626, column 32)
                      _lhsOtypeSyns =
                          ({-# LINE 626 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2620 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 145, column 15)
                      _lhsOuseMap =
                          ({-# LINE 145 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2626 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 771, column 32)
                      _lhsOwrappers =
                          ({-# LINE 771 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 2632 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          ({-# LINE 144 "src-ag/Transform.ag" #-}
                           _lhsIattrDecls
                           {-# LINE 2638 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrs =
                          ({-# LINE 1316 "src-ag/Transform.ag" #-}
                           _lhsIattrs
                           {-# LINE 2644 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOdefSets =
                          ({-# LINE 109 "src-ag/Transform.ag" #-}
                           _lhsIdefSets
                           {-# LINE 2650 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _setOallFields =
                          ({-# LINE 136 "src-ag/Transform.ag" #-}
                           _lhsIallFields
                           {-# LINE 2656 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _setOallNonterminals =
                          ({-# LINE 93 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 2662 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _setOdefinedSets =
                          ({-# LINE 112 "src-ag/Transform.ag" #-}
                           _lhsIdefinedSets
                           {-# LINE 2668 "dist/build/Transform" #-}
                           )
                      ( _setIcollectedNames,_setIerrors,_setInontSet) =
                          set_ _setOallFields _setOallNonterminals _setOdefinedSets
                      ___node =
                          (Syn_Elem _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers)
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConParams,_lhsOcollectedConstraints,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMacros,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))))
sem_Elem_Module :: Pos ->
                   String ->
                   String ->
                   String ->
                   T_Elem
sem_Elem_Module pos_ name_ exports_ imports_ =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallAttrs
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIattrs
               _lhsIdefSets
               _lhsIdefinedSets
               _lhsIoptions ->
                 (let _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _lhsOcollectedConParams :: ([(NontermIdent, ConstructorIdent, Set Identifier)])
                      _lhsOcollectedConstraints :: ([(NontermIdent, ConstructorIdent, [Type])])
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedMacros :: ([(NontermIdent, ConstructorIdent, MaybeMacro)])
                      _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOctxCollect :: ContextMap
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOparamsCollect :: ParamMap
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOquantCollect :: QuantMap
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      -- "src-ag/Transform.ag"(line 1191, column 7)
                      _lhsOmoduleDecl =
                          ({-# LINE 1191 "src-ag/Transform.ag" #-}
                           Just (name_, exports_, imports_)
                           {-# LINE 2725 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 893, column 55)
                      _lhsOattrOrderCollect =
                          ({-# LINE 893 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2731 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 46, column 19)
                      _lhsOblocks =
                          ({-# LINE 46 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2737 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 164, column 32)
                      _lhsOcollectedArounds =
                          ({-# LINE 164 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2743 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 163, column 32)
                      _lhsOcollectedAugments =
                          ({-# LINE 163 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2749 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 132, column 31)
                      _lhsOcollectedConParams =
                          ({-# LINE 132 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2755 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 131, column 33)
                      _lhsOcollectedConstraints =
                          ({-# LINE 131 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2761 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 99, column 48)
                      _lhsOcollectedConstructorsMap =
                          ({-# LINE 99 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2767 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 130, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 130 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2773 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 161, column 32)
                      _lhsOcollectedInsts =
                          ({-# LINE 161 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2779 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1283, column 28)
                      _lhsOcollectedMacros =
                          ({-# LINE 1283 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2785 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 165, column 32)
                      _lhsOcollectedMerges =
                          ({-# LINE 165 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2791 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 91, column 50)
                      _lhsOcollectedNames =
                          ({-# LINE 91 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 2797 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 159, column 32)
                      _lhsOcollectedRules =
                          ({-# LINE 159 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2803 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 90, column 50)
                      _lhsOcollectedSetNames =
                          ({-# LINE 90 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 2809 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 160, column 32)
                      _lhsOcollectedSigs =
                          ({-# LINE 160 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2815 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 162, column 32)
                      _lhsOcollectedUniques =
                          ({-# LINE 162 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2821 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 948, column 34)
                      _lhsOctxCollect =
                          ({-# LINE 948 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2827 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 990, column 33)
                      _lhsOderivings =
                          ({-# LINE 990 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2833 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 44, column 19)
                      _lhsOerrors =
                          ({-# LINE 44 "src-ag/Transform.ag" #-}
                           Seq.empty
                           {-# LINE 2839 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 925, column 37)
                      _lhsOparamsCollect =
                          ({-# LINE 925 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2845 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 787, column 34)
                      _lhsOpragmas =
                          ({-# LINE 787 "src-ag/Transform.ag" #-}
                           id
                           {-# LINE 2851 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 973, column 36)
                      _lhsOquantCollect =
                          ({-# LINE 973 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2857 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 865, column 56)
                      _lhsOsemPragmasCollect =
                          ({-# LINE 865 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2863 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 626, column 32)
                      _lhsOtypeSyns =
                          ({-# LINE 626 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2869 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 145, column 15)
                      _lhsOuseMap =
                          ({-# LINE 145 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2875 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 771, column 32)
                      _lhsOwrappers =
                          ({-# LINE 771 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 2881 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          ({-# LINE 144 "src-ag/Transform.ag" #-}
                           _lhsIattrDecls
                           {-# LINE 2887 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrs =
                          ({-# LINE 1316 "src-ag/Transform.ag" #-}
                           _lhsIattrs
                           {-# LINE 2893 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOdefSets =
                          ({-# LINE 109 "src-ag/Transform.ag" #-}
                           _lhsIdefSets
                           {-# LINE 2899 "dist/build/Transform" #-}
                           )
                      ___node =
                          (Syn_Elem _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers)
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConParams,_lhsOcollectedConstraints,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMacros,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))))
sem_Elem_Nocatas :: Pos ->
                    T_NontSet ->
                    T_Elem
sem_Elem_Nocatas pos_ (T_NontSet set_) =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallAttrs
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIattrs
               _lhsIdefSets
               _lhsIdefinedSets
               _lhsIoptions ->
                 (let _lhsOpragmas :: (Options -> Options)
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _lhsOcollectedConParams :: ([(NontermIdent, ConstructorIdent, Set Identifier)])
                      _lhsOcollectedConstraints :: ([(NontermIdent, ConstructorIdent, [Type])])
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedMacros :: ([(NontermIdent, ConstructorIdent, MaybeMacro)])
                      _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOctxCollect :: ContextMap
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOquantCollect :: QuantMap
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      _setOallFields :: DataTypes
                      _setOallNonterminals :: (Set NontermIdent)
                      _setOdefinedSets :: DefinedSets
                      _setIcollectedNames :: (Set Identifier)
                      _setIerrors :: (Seq Error)
                      _setInontSet :: (Set NontermIdent)
                      -- "src-ag/Transform.ag"(line 781, column 14)
                      _lhsOpragmas =
                          ({-# LINE 781 "src-ag/Transform.ag" #-}
                           \o -> o { nocatas = _setInontSet `Set.union` nocatas o }
                           {-# LINE 2958 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 893, column 55)
                      _lhsOattrOrderCollect =
                          ({-# LINE 893 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2964 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 46, column 19)
                      _lhsOblocks =
                          ({-# LINE 46 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 2970 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 164, column 32)
                      _lhsOcollectedArounds =
                          ({-# LINE 164 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2976 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 163, column 32)
                      _lhsOcollectedAugments =
                          ({-# LINE 163 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2982 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 132, column 31)
                      _lhsOcollectedConParams =
                          ({-# LINE 132 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2988 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 131, column 33)
                      _lhsOcollectedConstraints =
                          ({-# LINE 131 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 2994 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 99, column 48)
                      _lhsOcollectedConstructorsMap =
                          ({-# LINE 99 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3000 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 130, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 130 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3006 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 161, column 32)
                      _lhsOcollectedInsts =
                          ({-# LINE 161 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3012 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1283, column 28)
                      _lhsOcollectedMacros =
                          ({-# LINE 1283 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3018 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 165, column 32)
                      _lhsOcollectedMerges =
                          ({-# LINE 165 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3024 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 91, column 50)
                      _lhsOcollectedNames =
                          ({-# LINE 91 "src-ag/Transform.ag" #-}
                           _setIcollectedNames
                           {-# LINE 3030 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 159, column 32)
                      _lhsOcollectedRules =
                          ({-# LINE 159 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3036 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 90, column 50)
                      _lhsOcollectedSetNames =
                          ({-# LINE 90 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 3042 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 160, column 32)
                      _lhsOcollectedSigs =
                          ({-# LINE 160 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3048 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 162, column 32)
                      _lhsOcollectedUniques =
                          ({-# LINE 162 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3054 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 948, column 34)
                      _lhsOctxCollect =
                          ({-# LINE 948 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3060 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 990, column 33)
                      _lhsOderivings =
                          ({-# LINE 990 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3066 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 44, column 19)
                      _lhsOerrors =
                          ({-# LINE 44 "src-ag/Transform.ag" #-}
                           _setIerrors
                           {-# LINE 3072 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1187, column 37)
                      _lhsOmoduleDecl =
                          ({-# LINE 1187 "src-ag/Transform.ag" #-}
                           mzero
                           {-# LINE 3078 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 925, column 37)
                      _lhsOparamsCollect =
                          ({-# LINE 925 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3084 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 973, column 36)
                      _lhsOquantCollect =
                          ({-# LINE 973 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3090 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 865, column 56)
                      _lhsOsemPragmasCollect =
                          ({-# LINE 865 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3096 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 626, column 32)
                      _lhsOtypeSyns =
                          ({-# LINE 626 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3102 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 145, column 15)
                      _lhsOuseMap =
                          ({-# LINE 145 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3108 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 771, column 32)
                      _lhsOwrappers =
                          ({-# LINE 771 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 3114 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          ({-# LINE 144 "src-ag/Transform.ag" #-}
                           _lhsIattrDecls
                           {-# LINE 3120 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrs =
                          ({-# LINE 1316 "src-ag/Transform.ag" #-}
                           _lhsIattrs
                           {-# LINE 3126 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOdefSets =
                          ({-# LINE 109 "src-ag/Transform.ag" #-}
                           _lhsIdefSets
                           {-# LINE 3132 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _setOallFields =
                          ({-# LINE 136 "src-ag/Transform.ag" #-}
                           _lhsIallFields
                           {-# LINE 3138 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _setOallNonterminals =
                          ({-# LINE 93 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 3144 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _setOdefinedSets =
                          ({-# LINE 112 "src-ag/Transform.ag" #-}
                           _lhsIdefinedSets
                           {-# LINE 3150 "dist/build/Transform" #-}
                           )
                      ( _setIcollectedNames,_setIerrors,_setInontSet) =
                          set_ _setOallFields _setOallNonterminals _setOdefinedSets
                      ___node =
                          (Syn_Elem _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers)
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConParams,_lhsOcollectedConstraints,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMacros,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))))
sem_Elem_Pragma :: Pos ->
                   ([NontermIdent]) ->
                   T_Elem
sem_Elem_Pragma pos_ names_ =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallAttrs
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIattrs
               _lhsIdefSets
               _lhsIdefinedSets
               _lhsIoptions ->
                 (let _lhsOpragmas :: (Options -> Options)
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _lhsOcollectedConParams :: ([(NontermIdent, ConstructorIdent, Set Identifier)])
                      _lhsOcollectedConstraints :: ([(NontermIdent, ConstructorIdent, [Type])])
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedMacros :: ([(NontermIdent, ConstructorIdent, MaybeMacro)])
                      _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOctxCollect :: ContextMap
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOquantCollect :: QuantMap
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      -- "src-ag/Transform.ag"(line 790, column 13)
                      _lhsOpragmas =
                          ({-# LINE 790 "src-ag/Transform.ag" #-}
                           let mk n o = case getName n of
                                          "gencatas"     -> o { folds       = True  }
                                          "nogencatas"   -> o { folds       = False }
                                          "gendatas"     -> o { dataTypes   = True  }
                                          "datarecords"  -> o { dataRecords = True  }
                                          "nogendatas"   -> o { dataTypes   = False }
                                          "gensems"      -> o { semfuns     = True  }
                                          "nogensems"    -> o { semfuns     = False }
                                          "gentypesigs"  -> o { typeSigs    = True  }
                                          "nogentypesigs"-> o { typeSigs    = False }
                                          "nocycle"      -> o { withCycle   = False }
                                          "cycle"        -> o { withCycle   = True  }
                                          "nostrictdata" -> o { strictData  = False }
                                          "strictdata"   -> o { strictData  = True  }
                                          "nostrictcase" -> o { strictCases = False }
                                          "strictcase"   -> o { strictCases = True  }
                                          "strictercase" -> o { strictCases = True, stricterCases = True }
                                          "nostrictwrap" -> o { strictWrap  = False }
                                          "strictwrap"   -> o { strictWrap  = True  }
                                          "novisit"      -> o { visit       = False }
                                          "visit"        -> o { visit       = True  }
                                          "nocase"       -> o { cases       = False }
                                          "case"         -> o { cases       = True  }
                                          "noseq"        -> o { withSeq     = False }
                                          "seq"          -> o { withSeq     = True  }
                                          "nounbox"      -> o { unbox       = False }
                                          "unbox"        -> o { unbox       = True  }
                                          "bangpats"     -> o { bangpats    = True  }
                                          "breadthfirst" -> o { breadthFirst = True }
                                          "breadthfirstStrict" -> o { breadthFirstStrict = True }
                                          "nooptimize"   -> o { cases = False , visit = False }
                                          "optimize"     -> o { cases = True  , visit = True  }
                                          "strictsem"    -> o { strictSems = True }
                                          "gentraces"    -> o { genTraces = True }
                                          "genusetraces" -> o { genUseTraces = True }
                                          "splitsems"    -> o { splitSems = True }
                                          "gencostcentres" -> o { genCostCentres = True }
                                          "sepsemmods"   -> o { sepSemMods = True }
                                          "genlinepragmas" -> o { genLinePragmas = True }
                                          "newtypes"       -> o { newtypes = True }
                                          "nonewtypes"     -> o { newtypes = False }
                                          "nooptimizations" -> o { noOptimizations = True }
                                          "kennedywarren"   -> o { kennedyWarren = True }
                                          "aspectag"        -> o { genAspectAG = True }
                                          'n':'o':'g':'r':'o':'u':'p':'_':atts
                                                            -> o { noGroup =  extract atts  ++ noGroup o }
                                          "rename"          -> o { rename = True }
                                          "parallel"        -> o { parallelInvoke = True }
                                          "monadicwrappers" -> o { monadicWrappers = True }
                                          "dummytokenvisit" -> o { dummyTokenVisit = True }
                                          "tupleasdummytoken" -> o { tupleAsDummyToken = True }
                                          "stateasdummytoken" -> o { tupleAsDummyToken = False }
                                          "strictdummytoken" -> o { strictDummyToken = True }
                                          "noperruletypesigs" -> o { noPerRuleTypeSigs = True }
                                          "noperstatetypesigs" -> o { noPerStateTypeSigs = True }
                                          "noeagerblackholing" -> o { noEagerBlackholing = True }
                                          "noperrulecostcentres" -> o { noPerRuleCostCentres = True }
                                          "nopervisitcostcentres" -> o { noPerVisitCostCentres = True }
                                          "helpinlining" -> o { helpInlining = True }
                                          "noinlinepragmas" -> o { noInlinePragmas = True }
                                          "aggressiveinlinepragmas" -> o { aggressiveInlinePragmas = True }
                                          "latehigherorderbindings" -> o { lateHigherOrderBinding = True }
                                          s              -> trace ("uuagc: ignoring unknown pragma: " ++ s) o
                           in \o -> foldr mk o names_
                           {-# LINE 3268 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 893, column 55)
                      _lhsOattrOrderCollect =
                          ({-# LINE 893 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3274 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 46, column 19)
                      _lhsOblocks =
                          ({-# LINE 46 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3280 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 164, column 32)
                      _lhsOcollectedArounds =
                          ({-# LINE 164 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3286 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 163, column 32)
                      _lhsOcollectedAugments =
                          ({-# LINE 163 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3292 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 132, column 31)
                      _lhsOcollectedConParams =
                          ({-# LINE 132 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3298 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 131, column 33)
                      _lhsOcollectedConstraints =
                          ({-# LINE 131 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3304 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 99, column 48)
                      _lhsOcollectedConstructorsMap =
                          ({-# LINE 99 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3310 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 130, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 130 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3316 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 161, column 32)
                      _lhsOcollectedInsts =
                          ({-# LINE 161 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3322 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1283, column 28)
                      _lhsOcollectedMacros =
                          ({-# LINE 1283 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3328 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 165, column 32)
                      _lhsOcollectedMerges =
                          ({-# LINE 165 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3334 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 91, column 50)
                      _lhsOcollectedNames =
                          ({-# LINE 91 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 3340 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 159, column 32)
                      _lhsOcollectedRules =
                          ({-# LINE 159 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3346 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 90, column 50)
                      _lhsOcollectedSetNames =
                          ({-# LINE 90 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 3352 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 160, column 32)
                      _lhsOcollectedSigs =
                          ({-# LINE 160 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3358 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 162, column 32)
                      _lhsOcollectedUniques =
                          ({-# LINE 162 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3364 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 948, column 34)
                      _lhsOctxCollect =
                          ({-# LINE 948 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3370 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 990, column 33)
                      _lhsOderivings =
                          ({-# LINE 990 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3376 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 44, column 19)
                      _lhsOerrors =
                          ({-# LINE 44 "src-ag/Transform.ag" #-}
                           Seq.empty
                           {-# LINE 3382 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1187, column 37)
                      _lhsOmoduleDecl =
                          ({-# LINE 1187 "src-ag/Transform.ag" #-}
                           mzero
                           {-# LINE 3388 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 925, column 37)
                      _lhsOparamsCollect =
                          ({-# LINE 925 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3394 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 973, column 36)
                      _lhsOquantCollect =
                          ({-# LINE 973 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3400 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 865, column 56)
                      _lhsOsemPragmasCollect =
                          ({-# LINE 865 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3406 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 626, column 32)
                      _lhsOtypeSyns =
                          ({-# LINE 626 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3412 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 145, column 15)
                      _lhsOuseMap =
                          ({-# LINE 145 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3418 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 771, column 32)
                      _lhsOwrappers =
                          ({-# LINE 771 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 3424 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          ({-# LINE 144 "src-ag/Transform.ag" #-}
                           _lhsIattrDecls
                           {-# LINE 3430 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrs =
                          ({-# LINE 1316 "src-ag/Transform.ag" #-}
                           _lhsIattrs
                           {-# LINE 3436 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOdefSets =
                          ({-# LINE 109 "src-ag/Transform.ag" #-}
                           _lhsIdefSets
                           {-# LINE 3442 "dist/build/Transform" #-}
                           )
                      ___node =
                          (Syn_Elem _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers)
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConParams,_lhsOcollectedConstraints,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMacros,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))))
sem_Elem_Sem :: Pos ->
                ClassContext ->
                T_NontSet ->
                T_Attrs ->
                ([String]) ->
                T_SemAlts ->
                T_Elem
sem_Elem_Sem pos_ ctx_ (T_NontSet names_) (T_Attrs attrs_) quants_ (T_SemAlts alts_) =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallAttrs
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIattrs
               _lhsIdefSets
               _lhsIdefinedSets
               _lhsIoptions ->
                 (let _altsOnts :: (Set NontermIdent)
                      _lhsOctxCollect :: ContextMap
                      _lhsOquantCollect :: QuantMap
                      _attrsOnts :: (Set NontermIdent)
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _lhsOcollectedConParams :: ([(NontermIdent, ConstructorIdent, Set Identifier)])
                      _lhsOcollectedConstraints :: ([(NontermIdent, ConstructorIdent, [Type])])
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedMacros :: ([(NontermIdent, ConstructorIdent, MaybeMacro)])
                      _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      _namesOallFields :: DataTypes
                      _namesOallNonterminals :: (Set NontermIdent)
                      _namesOdefinedSets :: DefinedSets
                      _attrsOallFields :: DataTypes
                      _attrsOallNonterminals :: (Set NontermIdent)
                      _attrsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _attrsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _attrsOoptions :: Options
                      _altsOallAttrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _altsOallAttrs :: (Map NontermIdent (Attributes, Attributes))
                      _altsOallFields :: DataTypes
                      _altsOoptions :: Options
                      _namesIcollectedNames :: (Set Identifier)
                      _namesIerrors :: (Seq Error)
                      _namesInontSet :: (Set NontermIdent)
                      _attrsIattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _attrsIattrs :: (Map NontermIdent (Attributes, Attributes))
                      _attrsIerrors :: (Seq Error)
                      _attrsIuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _altsIattrOrderCollect :: AttrOrderMap
                      _altsIcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _altsIcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _altsIcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _altsIcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _altsIcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _altsIcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _altsIcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _altsIerrors :: (Seq Error)
                      _altsIsemPragmasCollect :: PragmaMap
                      -- "src-ag/Transform.ag"(line 179, column 10)
                      _altsOnts =
                          ({-# LINE 179 "src-ag/Transform.ag" #-}
                           _namesInontSet
                           {-# LINE 3530 "dist/build/Transform" #-}
                           )
                      -- "src-ag/Transform.ag"(line 952, column 7)
                      _lhsOctxCollect =
                          ({-# LINE 952 "src-ag/Transform.ag" #-}
                           if null ctx_
                           then Map.empty
                           else Map.fromList [(nt, ctx_) | nt <- Set.toList _namesInontSet]
                           {-# LINE 3538 "dist/build/Transform" #-}
                           )
                      -- "src-ag/Transform.ag"(line 977, column 7)
                      _lhsOquantCollect =
                          ({-# LINE 977 "src-ag/Transform.ag" #-}
                           if null quants_
                           then Map.empty
                           else Map.fromList [(nt, quants_) | nt <- Set.toList _namesInontSet]
                           {-# LINE 3546 "dist/build/Transform" #-}
                           )
                      -- "src-ag/Transform.ag"(line 1014, column 10)
                      _attrsOnts =
                          ({-# LINE 1014 "src-ag/Transform.ag" #-}
                           _namesInontSet
                           {-# LINE 3552 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 893, column 55)
                      _lhsOattrOrderCollect =
                          ({-# LINE 893 "src-ag/Transform.ag" #-}
                           _altsIattrOrderCollect
                           {-# LINE 3558 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 46, column 19)
                      _lhsOblocks =
                          ({-# LINE 46 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3564 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 164, column 32)
                      _lhsOcollectedArounds =
                          ({-# LINE 164 "src-ag/Transform.ag" #-}
                           _altsIcollectedArounds
                           {-# LINE 3570 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 163, column 32)
                      _lhsOcollectedAugments =
                          ({-# LINE 163 "src-ag/Transform.ag" #-}
                           _altsIcollectedAugments
                           {-# LINE 3576 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 132, column 31)
                      _lhsOcollectedConParams =
                          ({-# LINE 132 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3582 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 131, column 33)
                      _lhsOcollectedConstraints =
                          ({-# LINE 131 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3588 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 99, column 48)
                      _lhsOcollectedConstructorsMap =
                          ({-# LINE 99 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3594 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 130, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 130 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3600 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 161, column 32)
                      _lhsOcollectedInsts =
                          ({-# LINE 161 "src-ag/Transform.ag" #-}
                           _altsIcollectedInsts
                           {-# LINE 3606 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1283, column 28)
                      _lhsOcollectedMacros =
                          ({-# LINE 1283 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3612 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 165, column 32)
                      _lhsOcollectedMerges =
                          ({-# LINE 165 "src-ag/Transform.ag" #-}
                           _altsIcollectedMerges
                           {-# LINE 3618 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 91, column 50)
                      _lhsOcollectedNames =
                          ({-# LINE 91 "src-ag/Transform.ag" #-}
                           _namesIcollectedNames
                           {-# LINE 3624 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 159, column 32)
                      _lhsOcollectedRules =
                          ({-# LINE 159 "src-ag/Transform.ag" #-}
                           _altsIcollectedRules
                           {-# LINE 3630 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 90, column 50)
                      _lhsOcollectedSetNames =
                          ({-# LINE 90 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 3636 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 160, column 32)
                      _lhsOcollectedSigs =
                          ({-# LINE 160 "src-ag/Transform.ag" #-}
                           _altsIcollectedSigs
                           {-# LINE 3642 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 162, column 32)
                      _lhsOcollectedUniques =
                          ({-# LINE 162 "src-ag/Transform.ag" #-}
                           _altsIcollectedUniques
                           {-# LINE 3648 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 990, column 33)
                      _lhsOderivings =
                          ({-# LINE 990 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3654 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 44, column 19)
                      _lhsOerrors =
                          ({-# LINE 44 "src-ag/Transform.ag" #-}
                           _namesIerrors Seq.>< _attrsIerrors Seq.>< _altsIerrors
                           {-# LINE 3660 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1187, column 37)
                      _lhsOmoduleDecl =
                          ({-# LINE 1187 "src-ag/Transform.ag" #-}
                           mzero
                           {-# LINE 3666 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 925, column 37)
                      _lhsOparamsCollect =
                          ({-# LINE 925 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3672 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 787, column 34)
                      _lhsOpragmas =
                          ({-# LINE 787 "src-ag/Transform.ag" #-}
                           id
                           {-# LINE 3678 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 865, column 56)
                      _lhsOsemPragmasCollect =
                          ({-# LINE 865 "src-ag/Transform.ag" #-}
                           _altsIsemPragmasCollect
                           {-# LINE 3684 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 626, column 32)
                      _lhsOtypeSyns =
                          ({-# LINE 626 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3690 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 145, column 15)
                      _lhsOuseMap =
                          ({-# LINE 145 "src-ag/Transform.ag" #-}
                           _attrsIuseMap
                           {-# LINE 3696 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 771, column 32)
                      _lhsOwrappers =
                          ({-# LINE 771 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 3702 "dist/build/Transform" #-}
                           )
                      -- copy rule (up)
                      _lhsOattrDecls =
                          ({-# LINE 144 "src-ag/Transform.ag" #-}
                           _attrsIattrDecls
                           {-# LINE 3708 "dist/build/Transform" #-}
                           )
                      -- copy rule (up)
                      _lhsOattrs =
                          ({-# LINE 1316 "src-ag/Transform.ag" #-}
                           _attrsIattrs
                           {-# LINE 3714 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOdefSets =
                          ({-# LINE 109 "src-ag/Transform.ag" #-}
                           _lhsIdefSets
                           {-# LINE 3720 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _namesOallFields =
                          ({-# LINE 136 "src-ag/Transform.ag" #-}
                           _lhsIallFields
                           {-# LINE 3726 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _namesOallNonterminals =
                          ({-# LINE 93 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 3732 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _namesOdefinedSets =
                          ({-# LINE 112 "src-ag/Transform.ag" #-}
                           _lhsIdefinedSets
                           {-# LINE 3738 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _attrsOallFields =
                          ({-# LINE 136 "src-ag/Transform.ag" #-}
                           _lhsIallFields
                           {-# LINE 3744 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _attrsOallNonterminals =
                          ({-# LINE 93 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 3750 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _attrsOattrDecls =
                          ({-# LINE 144 "src-ag/Transform.ag" #-}
                           _lhsIattrDecls
                           {-# LINE 3756 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _attrsOattrs =
                          ({-# LINE 1316 "src-ag/Transform.ag" #-}
                           _lhsIattrs
                           {-# LINE 3762 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _attrsOoptions =
                          ({-# LINE 40 "src-ag/Transform.ag" #-}
                           _lhsIoptions
                           {-# LINE 3768 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _altsOallAttrDecls =
                          ({-# LINE 894 "src-ag/Transform.ag" #-}
                           _lhsIallAttrDecls
                           {-# LINE 3774 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _altsOallAttrs =
                          ({-# LINE 1306 "src-ag/Transform.ag" #-}
                           _lhsIallAttrs
                           {-# LINE 3780 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _altsOallFields =
                          ({-# LINE 136 "src-ag/Transform.ag" #-}
                           _lhsIallFields
                           {-# LINE 3786 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _altsOoptions =
                          ({-# LINE 40 "src-ag/Transform.ag" #-}
                           _lhsIoptions
                           {-# LINE 3792 "dist/build/Transform" #-}
                           )
                      ( _namesIcollectedNames,_namesIerrors,_namesInontSet) =
                          names_ _namesOallFields _namesOallNonterminals _namesOdefinedSets
                      ( _attrsIattrDecls,_attrsIattrs,_attrsIerrors,_attrsIuseMap) =
                          attrs_ _attrsOallFields _attrsOallNonterminals _attrsOattrDecls _attrsOattrs _attrsOnts _attrsOoptions
                      ( _altsIattrOrderCollect,_altsIcollectedArounds,_altsIcollectedAugments,_altsIcollectedInsts,_altsIcollectedMerges,_altsIcollectedRules,_altsIcollectedSigs,_altsIcollectedUniques,_altsIerrors,_altsIsemPragmasCollect) =
                          alts_ _altsOallAttrDecls _altsOallAttrs _altsOallFields _altsOnts _altsOoptions
                      ___node =
                          (Syn_Elem _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers)
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConParams,_lhsOcollectedConstraints,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMacros,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))))
sem_Elem_Set :: Pos ->
                NontermIdent ->
                Bool ->
                T_NontSet ->
                T_Elem
sem_Elem_Set pos_ name_ merge_ (T_NontSet set_) =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallAttrs
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIattrs
               _lhsIdefSets
               _lhsIdefinedSets
               _lhsIoptions ->
                 (let _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      _lhsOerrors :: (Seq Error)
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _lhsOcollectedConParams :: ([(NontermIdent, ConstructorIdent, Set Identifier)])
                      _lhsOcollectedConstraints :: ([(NontermIdent, ConstructorIdent, [Type])])
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedMacros :: ([(NontermIdent, ConstructorIdent, MaybeMacro)])
                      _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOctxCollect :: ContextMap
                      _lhsOderivings :: Derivings
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOquantCollect :: QuantMap
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _setOallFields :: DataTypes
                      _setOallNonterminals :: (Set NontermIdent)
                      _setOdefinedSets :: DefinedSets
                      _setIcollectedNames :: (Set Identifier)
                      _setIerrors :: (Seq Error)
                      _setInontSet :: (Set NontermIdent)
                      -- "src-ag/Transform.ag"(line 582, column 10)
                      _lhsOcollectedSetNames =
                          ({-# LINE 582 "src-ag/Transform.ag" #-}
                           Set.singleton name_
                           {-# LINE 3859 "dist/build/Transform" #-}
                           )
                      -- "src-ag/Transform.ag"(line 699, column 13)
                      (_defSets2,_errs) =
                          ({-# LINE 699 "src-ag/Transform.ag" #-}
                           let allUsedNames = Set.unions [ maybe (Set.singleton n)
                                                                 snd
                                                                 (Map.lookup n _lhsIdefSets)
                                                         | n <- Set.toList _setIcollectedNames
                                                         ]
                               (nontSet,e1) | Set.member name_ allUsedNames
                                                        = (Set.empty, Seq.singleton(CyclicSet name_))
                                            | otherwise = (_setInontSet, Seq.empty)
                               (res, e2) = let toAdd = (nontSet,Set.insert name_ allUsedNames)
                                               union (a,b) (c,d) = (a `Set.union` c, b `Set.union` d)
                                           in if Set.member name_ _lhsIallNonterminals || not merge_
                                              then checkDuplicate DupSet name_ toAdd _lhsIdefSets
                                              else (Map.insertWith union name_ toAdd _lhsIdefSets, Seq.empty)
                           in (res, e1 Seq.>< e2)
                           {-# LINE 3878 "dist/build/Transform" #-}
                           )
                      -- "src-ag/Transform.ag"(line 713, column 9)
                      _lhsOdefSets =
                          ({-# LINE 713 "src-ag/Transform.ag" #-}
                           _defSets2
                           {-# LINE 3884 "dist/build/Transform" #-}
                           )
                      -- "src-ag/Transform.ag"(line 713, column 9)
                      _lhsOerrors =
                          ({-# LINE 714 "src-ag/Transform.ag" #-}
                           _errs >< _setIerrors
                           {-# LINE 3890 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 893, column 55)
                      _lhsOattrOrderCollect =
                          ({-# LINE 893 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3896 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 46, column 19)
                      _lhsOblocks =
                          ({-# LINE 46 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3902 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 164, column 32)
                      _lhsOcollectedArounds =
                          ({-# LINE 164 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3908 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 163, column 32)
                      _lhsOcollectedAugments =
                          ({-# LINE 163 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3914 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 132, column 31)
                      _lhsOcollectedConParams =
                          ({-# LINE 132 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3920 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 131, column 33)
                      _lhsOcollectedConstraints =
                          ({-# LINE 131 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3926 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 99, column 48)
                      _lhsOcollectedConstructorsMap =
                          ({-# LINE 99 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3932 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 130, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 130 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3938 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 161, column 32)
                      _lhsOcollectedInsts =
                          ({-# LINE 161 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3944 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1283, column 28)
                      _lhsOcollectedMacros =
                          ({-# LINE 1283 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3950 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 165, column 32)
                      _lhsOcollectedMerges =
                          ({-# LINE 165 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3956 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 91, column 50)
                      _lhsOcollectedNames =
                          ({-# LINE 91 "src-ag/Transform.ag" #-}
                           _setIcollectedNames
                           {-# LINE 3962 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 159, column 32)
                      _lhsOcollectedRules =
                          ({-# LINE 159 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3968 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 160, column 32)
                      _lhsOcollectedSigs =
                          ({-# LINE 160 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3974 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 162, column 32)
                      _lhsOcollectedUniques =
                          ({-# LINE 162 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 3980 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 948, column 34)
                      _lhsOctxCollect =
                          ({-# LINE 948 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3986 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 990, column 33)
                      _lhsOderivings =
                          ({-# LINE 990 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 3992 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1187, column 37)
                      _lhsOmoduleDecl =
                          ({-# LINE 1187 "src-ag/Transform.ag" #-}
                           mzero
                           {-# LINE 3998 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 925, column 37)
                      _lhsOparamsCollect =
                          ({-# LINE 925 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4004 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 787, column 34)
                      _lhsOpragmas =
                          ({-# LINE 787 "src-ag/Transform.ag" #-}
                           id
                           {-# LINE 4010 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 973, column 36)
                      _lhsOquantCollect =
                          ({-# LINE 973 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4016 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 865, column 56)
                      _lhsOsemPragmasCollect =
                          ({-# LINE 865 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4022 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 626, column 32)
                      _lhsOtypeSyns =
                          ({-# LINE 626 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4028 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 145, column 15)
                      _lhsOuseMap =
                          ({-# LINE 145 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4034 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 771, column 32)
                      _lhsOwrappers =
                          ({-# LINE 771 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 4040 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          ({-# LINE 144 "src-ag/Transform.ag" #-}
                           _lhsIattrDecls
                           {-# LINE 4046 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrs =
                          ({-# LINE 1316 "src-ag/Transform.ag" #-}
                           _lhsIattrs
                           {-# LINE 4052 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _setOallFields =
                          ({-# LINE 136 "src-ag/Transform.ag" #-}
                           _lhsIallFields
                           {-# LINE 4058 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _setOallNonterminals =
                          ({-# LINE 93 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 4064 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _setOdefinedSets =
                          ({-# LINE 112 "src-ag/Transform.ag" #-}
                           _lhsIdefinedSets
                           {-# LINE 4070 "dist/build/Transform" #-}
                           )
                      ( _setIcollectedNames,_setIerrors,_setInontSet) =
                          set_ _setOallFields _setOallNonterminals _setOdefinedSets
                      ___node =
                          (Syn_Elem _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers)
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConParams,_lhsOcollectedConstraints,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMacros,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))))
sem_Elem_Txt :: Pos ->
                BlockKind ->
                (Maybe NontermIdent) ->
                ([String]) ->
                T_Elem
sem_Elem_Txt pos_ kind_ mbNt_ lines_ =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallAttrs
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIattrs
               _lhsIdefSets
               _lhsIdefinedSets
               _lhsIoptions ->
                 (let _lhsOblocks :: Blocks
                      _lhsOerrors :: (Seq Error)
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _lhsOcollectedConParams :: ([(NontermIdent, ConstructorIdent, Set Identifier)])
                      _lhsOcollectedConstraints :: ([(NontermIdent, ConstructorIdent, [Type])])
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedMacros :: ([(NontermIdent, ConstructorIdent, MaybeMacro)])
                      _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOctxCollect :: ContextMap
                      _lhsOderivings :: Derivings
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOquantCollect :: QuantMap
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      -- "src-ag/Transform.ag"(line 188, column 10)
                      _blockInfo =
                          ({-# LINE 188 "src-ag/Transform.ag" #-}
                           ( kind_
                           , mbNt_
                           )
                           {-# LINE 4129 "dist/build/Transform" #-}
                           )
                      -- "src-ag/Transform.ag"(line 191, column 10)
                      _blockValue =
                          ({-# LINE 191 "src-ag/Transform.ag" #-}
                           [(lines_, pos_)]
                           {-# LINE 4135 "dist/build/Transform" #-}
                           )
                      -- "src-ag/Transform.ag"(line 192, column 10)
                      _lhsOblocks =
                          ({-# LINE 192 "src-ag/Transform.ag" #-}
                           Map.singleton _blockInfo     _blockValue
                           {-# LINE 4141 "dist/build/Transform" #-}
                           )
                      -- "src-ag/Transform.ag"(line 193, column 10)
                      _lhsOerrors =
                          ({-# LINE 193 "src-ag/Transform.ag" #-}
                           if checkParseBlock _lhsIoptions
                           then let exp = Expression pos_ tks
                                    tks = [tk]
                                    tk  = HsToken (unlines lines_) pos_
                                in Seq.fromList $ checkBlock $ exp
                           else Seq.empty
                           {-# LINE 4152 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 893, column 55)
                      _lhsOattrOrderCollect =
                          ({-# LINE 893 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4158 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 164, column 32)
                      _lhsOcollectedArounds =
                          ({-# LINE 164 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4164 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 163, column 32)
                      _lhsOcollectedAugments =
                          ({-# LINE 163 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4170 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 132, column 31)
                      _lhsOcollectedConParams =
                          ({-# LINE 132 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4176 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 131, column 33)
                      _lhsOcollectedConstraints =
                          ({-# LINE 131 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4182 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 99, column 48)
                      _lhsOcollectedConstructorsMap =
                          ({-# LINE 99 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4188 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 130, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 130 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4194 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 161, column 32)
                      _lhsOcollectedInsts =
                          ({-# LINE 161 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4200 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1283, column 28)
                      _lhsOcollectedMacros =
                          ({-# LINE 1283 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4206 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 165, column 32)
                      _lhsOcollectedMerges =
                          ({-# LINE 165 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4212 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 91, column 50)
                      _lhsOcollectedNames =
                          ({-# LINE 91 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 4218 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 159, column 32)
                      _lhsOcollectedRules =
                          ({-# LINE 159 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4224 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 90, column 50)
                      _lhsOcollectedSetNames =
                          ({-# LINE 90 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 4230 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 160, column 32)
                      _lhsOcollectedSigs =
                          ({-# LINE 160 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4236 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 162, column 32)
                      _lhsOcollectedUniques =
                          ({-# LINE 162 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4242 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 948, column 34)
                      _lhsOctxCollect =
                          ({-# LINE 948 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4248 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 990, column 33)
                      _lhsOderivings =
                          ({-# LINE 990 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4254 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1187, column 37)
                      _lhsOmoduleDecl =
                          ({-# LINE 1187 "src-ag/Transform.ag" #-}
                           mzero
                           {-# LINE 4260 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 925, column 37)
                      _lhsOparamsCollect =
                          ({-# LINE 925 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4266 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 787, column 34)
                      _lhsOpragmas =
                          ({-# LINE 787 "src-ag/Transform.ag" #-}
                           id
                           {-# LINE 4272 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 973, column 36)
                      _lhsOquantCollect =
                          ({-# LINE 973 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4278 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 865, column 56)
                      _lhsOsemPragmasCollect =
                          ({-# LINE 865 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4284 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 626, column 32)
                      _lhsOtypeSyns =
                          ({-# LINE 626 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4290 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 145, column 15)
                      _lhsOuseMap =
                          ({-# LINE 145 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4296 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 771, column 32)
                      _lhsOwrappers =
                          ({-# LINE 771 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 4302 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          ({-# LINE 144 "src-ag/Transform.ag" #-}
                           _lhsIattrDecls
                           {-# LINE 4308 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrs =
                          ({-# LINE 1316 "src-ag/Transform.ag" #-}
                           _lhsIattrs
                           {-# LINE 4314 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOdefSets =
                          ({-# LINE 109 "src-ag/Transform.ag" #-}
                           _lhsIdefSets
                           {-# LINE 4320 "dist/build/Transform" #-}
                           )
                      ___node =
                          (Syn_Elem _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers)
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConParams,_lhsOcollectedConstraints,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMacros,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))))
sem_Elem_Type :: Pos ->
                 ClassContext ->
                 NontermIdent ->
                 ([Identifier]) ->
                 ComplexType ->
                 T_Elem
sem_Elem_Type pos_ ctx_ name_ params_ type_ =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallAttrs
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIattrs
               _lhsIdefSets
               _lhsIdefinedSets
               _lhsIoptions ->
                 (let _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOparamsCollect :: ParamMap
                      _lhsOctxCollect :: ContextMap
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _lhsOcollectedConParams :: ([(NontermIdent, ConstructorIdent, Set Identifier)])
                      _lhsOcollectedConstraints :: ([(NontermIdent, ConstructorIdent, [Type])])
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedMacros :: ([(NontermIdent, ConstructorIdent, MaybeMacro)])
                      _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOquantCollect :: QuantMap
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      -- "src-ag/Transform.ag"(line 255, column 10)
                      _lhsOcollectedFields =
                          ({-# LINE 255 "src-ag/Transform.ag" #-}
                           map (\(x,y)->(name_, x, y)) _expanded
                           {-# LINE 4376 "dist/build/Transform" #-}
                           )
                      -- "src-ag/Transform.ag"(line 585, column 11)
                      _lhsOcollectedNames =
                          ({-# LINE 585 "src-ag/Transform.ag" #-}
                           Set.singleton name_
                           {-# LINE 4382 "dist/build/Transform" #-}
                           )
                      -- "src-ag/Transform.ag"(line 639, column 11)
                      _expanded =
                          ({-# LINE 639 "src-ag/Transform.ag" #-}
                           case _argType of
                                   List tp -> [(Ident "Cons" pos_, [(Ident "hd" pos_, tp)
                                                                   ,(Ident "tl" pos_, NT name_ (map getName params_) False)
                                                                   ]
                                               )
                                              ,(Ident "Nil" pos_,  [])
                                              ]
                                   Maybe tp -> [(Ident "Just" pos_, [(Ident "just" pos_, tp)
                                                                   ]
                                               )
                                              ,(Ident "Nothing" pos_,  [])
                                              ]
                                   Either tp1 tp2 -> [
                                                (Ident "Left"    pos_,  [(Ident "left"  pos_, tp1) ])
                                              , (Ident "Right"   pos_,  [(Ident "right" pos_, tp2) ])
                                              ]
                                   Map tp1 tp2 -> [ (Ident "Entry" pos_, [ (Ident "key" pos_, tp1)
                                                                         , (Ident "val" pos_, tp2)
                                                                         , (Ident "tl" pos_, NT name_ (map getName params_) False)
                                                                         ])
                                                  , (Ident "Nil" pos_, [])
                                                  ]
                                   IntMap tp   -> [ (Ident "Entry" pos_, [ (Ident "key" pos_, Haskell "Int")
                                                                         , (Ident "val" pos_, tp)
                                                                         , (Ident "tl" pos_, NT name_ (map getName params_) False)
                                                                         ])
                                                  , (Ident "Nil" pos_, [])
                                                  ]
                                   OrdSet tp   -> [ (Ident "Entry" pos_, [ (Ident "val" pos_, tp)
                                                                         , (Ident "tl" pos_, NT name_ (map getName params_) False) ])
                                                  , (Ident "Nil" pos_, [])
                                                  ]
                                   IntSet      -> [ (Ident "Entry" pos_, [ (Ident "val" pos_, Haskell "Int")
                                                                         , (Ident "tl" pos_, NT name_ (map getName params_) False) ])
                                                  , (Ident "Nil" pos_, [])
                                                  ]
                                   Tuple xs -> [(Ident "Tuple" pos_, xs)]
                           {-# LINE 4424 "dist/build/Transform" #-}
                           )
                      -- "src-ag/Transform.ag"(line 676, column 11)
                      _argType =
                          ({-# LINE 676 "src-ag/Transform.ag" #-}
                           case type_ of
                            Maybe tp       -> Maybe  (  makeType _lhsIallNonterminals tp)
                            Either tp1 tp2 -> Either (  makeType _lhsIallNonterminals tp1) (makeType _lhsIallNonterminals tp2)
                            List tp        -> List   (  makeType _lhsIallNonterminals tp)
                            Tuple xs       -> Tuple [(f,makeType _lhsIallNonterminals tp) | (f,tp) <- xs]
                            Map tp1 tp2    -> Map    (  makeType _lhsIallNonterminals tp1) (makeType _lhsIallNonterminals tp2)
                            IntMap tp      -> IntMap (  makeType _lhsIallNonterminals tp)
                            OrdSet tp      -> OrdSet (  makeType _lhsIallNonterminals tp)
                            IntSet         -> IntSet
                           {-# LINE 4438 "dist/build/Transform" #-}
                           )
                      -- "src-ag/Transform.ag"(line 685, column 11)
                      _lhsOtypeSyns =
                          ({-# LINE 685 "src-ag/Transform.ag" #-}
                           [(name_,_argType)]
                           {-# LINE 4444 "dist/build/Transform" #-}
                           )
                      -- "src-ag/Transform.ag"(line 935, column 7)
                      _lhsOparamsCollect =
                          ({-# LINE 935 "src-ag/Transform.ag" #-}
                           if null params_
                           then Map.empty
                           else Map.singleton name_ params_
                           {-# LINE 4452 "dist/build/Transform" #-}
                           )
                      -- "src-ag/Transform.ag"(line 958, column 7)
                      _lhsOctxCollect =
                          ({-# LINE 958 "src-ag/Transform.ag" #-}
                           if null ctx_
                           then Map.empty
                           else Map.singleton name_ ctx_
                           {-# LINE 4460 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 893, column 55)
                      _lhsOattrOrderCollect =
                          ({-# LINE 893 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4466 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 46, column 19)
                      _lhsOblocks =
                          ({-# LINE 46 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4472 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 164, column 32)
                      _lhsOcollectedArounds =
                          ({-# LINE 164 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4478 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 163, column 32)
                      _lhsOcollectedAugments =
                          ({-# LINE 163 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4484 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 132, column 31)
                      _lhsOcollectedConParams =
                          ({-# LINE 132 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4490 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 131, column 33)
                      _lhsOcollectedConstraints =
                          ({-# LINE 131 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4496 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 99, column 48)
                      _lhsOcollectedConstructorsMap =
                          ({-# LINE 99 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4502 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 161, column 32)
                      _lhsOcollectedInsts =
                          ({-# LINE 161 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4508 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1283, column 28)
                      _lhsOcollectedMacros =
                          ({-# LINE 1283 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4514 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 165, column 32)
                      _lhsOcollectedMerges =
                          ({-# LINE 165 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4520 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 159, column 32)
                      _lhsOcollectedRules =
                          ({-# LINE 159 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4526 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 90, column 50)
                      _lhsOcollectedSetNames =
                          ({-# LINE 90 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 4532 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 160, column 32)
                      _lhsOcollectedSigs =
                          ({-# LINE 160 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4538 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 162, column 32)
                      _lhsOcollectedUniques =
                          ({-# LINE 162 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4544 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 990, column 33)
                      _lhsOderivings =
                          ({-# LINE 990 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4550 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 44, column 19)
                      _lhsOerrors =
                          ({-# LINE 44 "src-ag/Transform.ag" #-}
                           Seq.empty
                           {-# LINE 4556 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1187, column 37)
                      _lhsOmoduleDecl =
                          ({-# LINE 1187 "src-ag/Transform.ag" #-}
                           mzero
                           {-# LINE 4562 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 787, column 34)
                      _lhsOpragmas =
                          ({-# LINE 787 "src-ag/Transform.ag" #-}
                           id
                           {-# LINE 4568 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 973, column 36)
                      _lhsOquantCollect =
                          ({-# LINE 973 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4574 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 865, column 56)
                      _lhsOsemPragmasCollect =
                          ({-# LINE 865 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4580 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 145, column 15)
                      _lhsOuseMap =
                          ({-# LINE 145 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4586 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 771, column 32)
                      _lhsOwrappers =
                          ({-# LINE 771 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 4592 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          ({-# LINE 144 "src-ag/Transform.ag" #-}
                           _lhsIattrDecls
                           {-# LINE 4598 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrs =
                          ({-# LINE 1316 "src-ag/Transform.ag" #-}
                           _lhsIattrs
                           {-# LINE 4604 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOdefSets =
                          ({-# LINE 109 "src-ag/Transform.ag" #-}
                           _lhsIdefSets
                           {-# LINE 4610 "dist/build/Transform" #-}
                           )
                      ___node =
                          (Syn_Elem _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers)
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConParams,_lhsOcollectedConstraints,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMacros,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))))
sem_Elem_Wrapper :: Pos ->
                    T_NontSet ->
                    T_Elem
sem_Elem_Wrapper pos_ (T_NontSet set_) =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallAttrs
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIattrs
               _lhsIdefSets
               _lhsIdefinedSets
               _lhsIoptions ->
                 (let _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                      _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                      _lhsOcollectedConParams :: ([(NontermIdent, ConstructorIdent, Set Identifier)])
                      _lhsOcollectedConstraints :: ([(NontermIdent, ConstructorIdent, [Type])])
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedMacros :: ([(NontermIdent, ConstructorIdent, MaybeMacro)])
                      _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOctxCollect :: ContextMap
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOquantCollect :: QuantMap
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      _setOallFields :: DataTypes
                      _setOallNonterminals :: (Set NontermIdent)
                      _setOdefinedSets :: DefinedSets
                      _setIcollectedNames :: (Set Identifier)
                      _setIerrors :: (Seq Error)
                      _setInontSet :: (Set NontermIdent)
                      -- "src-ag/Transform.ag"(line 774, column 13)
                      _lhsOwrappers =
                          ({-# LINE 774 "src-ag/Transform.ag" #-}
                           _setInontSet
                           {-# LINE 4669 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 893, column 55)
                      _lhsOattrOrderCollect =
                          ({-# LINE 893 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4675 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 46, column 19)
                      _lhsOblocks =
                          ({-# LINE 46 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4681 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 164, column 32)
                      _lhsOcollectedArounds =
                          ({-# LINE 164 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4687 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 163, column 32)
                      _lhsOcollectedAugments =
                          ({-# LINE 163 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4693 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 132, column 31)
                      _lhsOcollectedConParams =
                          ({-# LINE 132 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4699 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 131, column 33)
                      _lhsOcollectedConstraints =
                          ({-# LINE 131 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4705 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 99, column 48)
                      _lhsOcollectedConstructorsMap =
                          ({-# LINE 99 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4711 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 130, column 28)
                      _lhsOcollectedFields =
                          ({-# LINE 130 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4717 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 161, column 32)
                      _lhsOcollectedInsts =
                          ({-# LINE 161 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4723 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1283, column 28)
                      _lhsOcollectedMacros =
                          ({-# LINE 1283 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4729 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 165, column 32)
                      _lhsOcollectedMerges =
                          ({-# LINE 165 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4735 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 91, column 50)
                      _lhsOcollectedNames =
                          ({-# LINE 91 "src-ag/Transform.ag" #-}
                           _setIcollectedNames
                           {-# LINE 4741 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 159, column 32)
                      _lhsOcollectedRules =
                          ({-# LINE 159 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4747 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 90, column 50)
                      _lhsOcollectedSetNames =
                          ({-# LINE 90 "src-ag/Transform.ag" #-}
                           Set.empty
                           {-# LINE 4753 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 160, column 32)
                      _lhsOcollectedSigs =
                          ({-# LINE 160 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4759 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 162, column 32)
                      _lhsOcollectedUniques =
                          ({-# LINE 162 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4765 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 948, column 34)
                      _lhsOctxCollect =
                          ({-# LINE 948 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4771 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 990, column 33)
                      _lhsOderivings =
                          ({-# LINE 990 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4777 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 44, column 19)
                      _lhsOerrors =
                          ({-# LINE 44 "src-ag/Transform.ag" #-}
                           _setIerrors
                           {-# LINE 4783 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 1187, column 37)
                      _lhsOmoduleDecl =
                          ({-# LINE 1187 "src-ag/Transform.ag" #-}
                           mzero
                           {-# LINE 4789 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 925, column 37)
                      _lhsOparamsCollect =
                          ({-# LINE 925 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4795 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 787, column 34)
                      _lhsOpragmas =
                          ({-# LINE 787 "src-ag/Transform.ag" #-}
                           id
                           {-# LINE 4801 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 973, column 36)
                      _lhsOquantCollect =
                          ({-# LINE 973 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4807 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 865, column 56)
                      _lhsOsemPragmasCollect =
                          ({-# LINE 865 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4813 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 626, column 32)
                      _lhsOtypeSyns =
                          ({-# LINE 626 "src-ag/Transform.ag" #-}
                           []
                           {-# LINE 4819 "dist/build/Transform" #-}
                           )
                      -- use rule "src-ag/Transform.ag"(line 145, column 15)
                      _lhsOuseMap =
                          ({-# LINE 145 "src-ag/Transform.ag" #-}
                           Map.empty
                           {-# LINE 4825 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          ({-# LINE 144 "src-ag/Transform.ag" #-}
                           _lhsIattrDecls
                           {-# LINE 4831 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOattrs =
                          ({-# LINE 1316 "src-ag/Transform.ag" #-}
                           _lhsIattrs
                           {-# LINE 4837 "dist/build/Transform" #-}
                           )
                      -- copy rule (chain)
                      _lhsOdefSets =
                          ({-# LINE 109 "src-ag/Transform.ag" #-}
                           _lhsIdefSets
                           {-# LINE 4843 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _setOallFields =
                          ({-# LINE 136 "src-ag/Transform.ag" #-}
                           _lhsIallFields
                           {-# LINE 4849 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _setOallNonterminals =
                          ({-# LINE 93 "src-ag/Transform.ag" #-}
                           _lhsIallNonterminals
                           {-# LINE 4855 "dist/build/Transform" #-}
                           )
                      -- copy rule (down)
                      _setOdefinedSets =
                          ({-# LINE 112 "src-ag/Transform.ag" #-}
                           _lhsIdefinedSets
                           {-# LINE 4861 "dist/build/Transform" #-}
                           )
                      ( _setIcollectedNames,_setIerrors,_setInontSet) =
                          set_ _setOallFields _setOallNonterminals _setOdefinedSets
                      ___node =
                          (Syn_Elem _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers)
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConParams,_lhsOcollectedConstraints,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMacros,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))))
-- Elems -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allAttrDecls         : Map NontermIdent (Attributes, Attributes)
         allAttrs             : Map NontermIdent (Attributes, Attributes)
         allConstructors      : Map NontermIdent (Set ConstructorIdent)
         allFields            : DataTypes
         allNonterminals      : Set NontermIdent
         definedSets          : DefinedSets
         options              : Options
      chained attributes:
         attrDecls            : Map NontermIdent (Attributes, Attributes)
         attrs                : Map NontermIdent (Attributes, Attributes)
         defSets              : Map Identifier (Set NontermIdent,Set Identifier)
      synthesized attributes:
         attrOrderCollect     : AttrOrderMap
         blocks               : Blocks
         collectedArounds     : [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         collectedAugments    : [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         collectedConParams   : [(NontermIdent, ConstructorIdent, Set Identifier)]
         collectedConstraints : [(NontermIdent, ConstructorIdent, [Type])]
         collectedConstructorsMap : Map NontermIdent (Set ConstructorIdent)
         collectedFields      : [(NontermIdent, ConstructorIdent, FieldMap)]
         collectedInsts       : [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         collectedMacros      : [(NontermIdent, ConstructorIdent, MaybeMacro)]
         collectedMerges      : [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         collectedNames       : Set Identifier
         collectedRules       : [ (NontermIdent, ConstructorIdent, RuleInfo)]
         collectedSetNames    : Set Identifier
         collectedSigs        : [ (NontermIdent, ConstructorIdent, SigInfo) ]
         collectedUniques     : [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         ctxCollect           : ContextMap
         derivings            : Derivings
         errors               : Seq Error
         moduleDecl           : Maybe (String,String,String)
         paramsCollect        : ParamMap
         pragmas              : Options -> Options
         quantCollect         : QuantMap
         semPragmasCollect    : PragmaMap
         typeSyns             : TypeSyns
         useMap               : Map NontermIdent (Map Identifier (String,String,String))
         wrappers             : Set NontermIdent
   alternatives:
      alternative Cons:
         child hd             : Elem 
         child tl             : Elems 
      alternative Nil:
-}
-- cata
sem_Elems :: Elems ->
             T_Elems
sem_Elems list =
    (Prelude.foldr sem_Elems_Cons sem_Elems_Nil (Prelude.map sem_Elem list))
-- semantic domain
newtype T_Elems = T_Elems ((Map NontermIdent (Attributes, Attributes)) ->
                           (Map NontermIdent (Attributes, Attributes)) ->
                           (Map NontermIdent (Set ConstructorIdent)) ->
                           DataTypes ->
                           (Set NontermIdent) ->
                           (Map NontermIdent (Attributes, Attributes)) ->
                           (Map NontermIdent (Attributes, Attributes)) ->
                           (Map Identifier (Set NontermIdent,Set Identifier)) ->
                           DefinedSets ->
                           Options ->
                           ( (Map NontermIdent (Attributes, Attributes)),AttrOrderMap,(Map NontermIdent (Attributes, Attributes)),Blocks,([ (NontermIdent, ConstructorIdent, [AroundInfo])  ]),([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]),([(NontermIdent, ConstructorIdent, Set Identifier)]),([(NontermIdent, ConstructorIdent, [Type])]),(Map NontermIdent (Set ConstructorIdent)),([(NontermIdent, ConstructorIdent, FieldMap)]),([ (NontermIdent, ConstructorIdent, [Identifier]) ]),([(NontermIdent, ConstructorIdent, MaybeMacro)]),([ (NontermIdent, ConstructorIdent, [MergeInfo])   ]),(Set Identifier),([ (NontermIdent, ConstructorIdent, RuleInfo)]),(Set Identifier),([ (NontermIdent, ConstructorIdent, SigInfo) ]),([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]),ContextMap,(Map Identifier (Set NontermIdent,Set Identifier)),Derivings,(Seq Error),(Maybe (String,String,String)),ParamMap,(Options -> Options),QuantMap,PragmaMap,TypeSyns,(Map NontermIdent (Map Identifier (String,String,String))),(Set NontermIdent)))
data Inh_Elems = Inh_Elems {allAttrDecls_Inh_Elems :: !((Map NontermIdent (Attributes, Attributes))),allAttrs_Inh_Elems :: !((Map NontermIdent (Attributes, Attributes))),allConstructors_Inh_Elems :: !((Map NontermIdent (Set ConstructorIdent))),allFields_Inh_Elems :: !(DataTypes),allNonterminals_Inh_Elems :: !((Set NontermIdent)),attrDecls_Inh_Elems :: !((Map NontermIdent (Attributes, Attributes))),attrs_Inh_Elems :: !((Map NontermIdent (Attributes, Attributes))),defSets_Inh_Elems :: !((Map Identifier (Set NontermIdent,Set Identifier))),definedSets_Inh_Elems :: !(DefinedSets),options_Inh_Elems :: !(Options)}
data Syn_Elems = Syn_Elems {attrDecls_Syn_Elems :: !((Map NontermIdent (Attributes, Attributes))),attrOrderCollect_Syn_Elems :: !(AttrOrderMap),attrs_Syn_Elems :: !((Map NontermIdent (Attributes, Attributes))),blocks_Syn_Elems :: !(Blocks),collectedArounds_Syn_Elems :: !(([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])),collectedAugments_Syn_Elems :: !(([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])),collectedConParams_Syn_Elems :: !(([(NontermIdent, ConstructorIdent, Set Identifier)])),collectedConstraints_Syn_Elems :: !(([(NontermIdent, ConstructorIdent, [Type])])),collectedConstructorsMap_Syn_Elems :: !((Map NontermIdent (Set ConstructorIdent))),collectedFields_Syn_Elems :: !(([(NontermIdent, ConstructorIdent, FieldMap)])),collectedInsts_Syn_Elems :: !(([ (NontermIdent, ConstructorIdent, [Identifier]) ])),collectedMacros_Syn_Elems :: !(([(NontermIdent, ConstructorIdent, MaybeMacro)])),collectedMerges_Syn_Elems :: !(([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])),collectedNames_Syn_Elems :: !((Set Identifier)),collectedRules_Syn_Elems :: !(([ (NontermIdent, ConstructorIdent, RuleInfo)])),collectedSetNames_Syn_Elems :: !((Set Identifier)),collectedSigs_Syn_Elems :: !(([ (NontermIdent, ConstructorIdent, SigInfo) ])),collectedUniques_Syn_Elems :: !(([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])),ctxCollect_Syn_Elems :: !(ContextMap),defSets_Syn_Elems :: !((Map Identifier (Set NontermIdent,Set Identifier))),derivings_Syn_Elems :: !(Derivings),errors_Syn_Elems :: !((Seq Error)),moduleDecl_Syn_Elems :: !((Maybe (String,String,String))),paramsCollect_Syn_Elems :: !(ParamMap),pragmas_Syn_Elems :: !((Options -> Options)),quantCollect_Syn_Elems :: !(QuantMap),semPragmasCollect_Syn_Elems :: !(PragmaMap),typeSyns_Syn_Elems :: !(TypeSyns),useMap_Syn_Elems :: !((Map NontermIdent (Map Identifier (String,String,String)))),wrappers_Syn_Elems :: !((Set NontermIdent))}
wrap_Elems :: T_Elems ->
              Inh_Elems ->
              Syn_Elems
wrap_Elems (T_Elems sem) (Inh_Elems _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions) =
    (let ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConParams,_lhsOcollectedConstraints,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMacros,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers) = sem _lhsIallAttrDecls _lhsIallAttrs _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIattrs _lhsIdefSets _lhsIdefinedSets _lhsIoptions
     in  (Syn_Elems _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers))
sem_Elems_Cons :: T_Elem ->
                  T_Elems ->
                  T_Elems
sem_Elems_Cons (T_Elem hd_) (T_Elems tl_) =
    (T_Elems (\ _lhsIallAttrDecls
                _lhsIallAttrs
                _lhsIallConstructors
                _lhsIallFields
                _lhsIallNonterminals
                _lhsIattrDecls
                _lhsIattrs
                _lhsIdefSets
                _lhsIdefinedSets
                _lhsIoptions ->
                  (let _lhsOattrOrderCollect :: AttrOrderMap
                       _lhsOblocks :: Blocks
                       _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                       _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                       _lhsOcollectedConParams :: ([(NontermIdent, ConstructorIdent, Set Identifier)])
                       _lhsOcollectedConstraints :: ([(NontermIdent, ConstructorIdent, [Type])])
                       _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                       _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                       _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                       _lhsOcollectedMacros :: ([(NontermIdent, ConstructorIdent, MaybeMacro)])
                       _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                       _lhsOcollectedNames :: (Set Identifier)
                       _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                       _lhsOcollectedSetNames :: (Set Identifier)
                       _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                       _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                       _lhsOctxCollect :: ContextMap
                       _lhsOderivings :: Derivings
                       _lhsOerrors :: (Seq Error)
                       _lhsOmoduleDecl :: (Maybe (String,String,String))
                       _lhsOparamsCollect :: ParamMap
                       _lhsOpragmas :: (Options -> Options)
                       _lhsOquantCollect :: QuantMap
                       _lhsOsemPragmasCollect :: PragmaMap
                       _lhsOtypeSyns :: TypeSyns
                       _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                       _lhsOwrappers :: (Set NontermIdent)
                       _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                       _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                       _hdOallAttrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _hdOallAttrs :: (Map NontermIdent (Attributes, Attributes))
                       _hdOallConstructors :: (Map NontermIdent (Set ConstructorIdent))
                       _hdOallFields :: DataTypes
                       _hdOallNonterminals :: (Set NontermIdent)
                       _hdOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _hdOattrs :: (Map NontermIdent (Attributes, Attributes))
                       _hdOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                       _hdOdefinedSets :: DefinedSets
                       _hdOoptions :: Options
                       _tlOallAttrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _tlOallAttrs :: (Map NontermIdent (Attributes, Attributes))
                       _tlOallConstructors :: (Map NontermIdent (Set ConstructorIdent))
                       _tlOallFields :: DataTypes
                       _tlOallNonterminals :: (Set NontermIdent)
                       _tlOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _tlOattrs :: (Map NontermIdent (Attributes, Attributes))
                       _tlOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                       _tlOdefinedSets :: DefinedSets
                       _tlOoptions :: Options
                       _hdIattrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _hdIattrOrderCollect :: AttrOrderMap
                       _hdIattrs :: (Map NontermIdent (Attributes, Attributes))
                       _hdIblocks :: Blocks
                       _hdIcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                       _hdIcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                       _hdIcollectedConParams :: ([(NontermIdent, ConstructorIdent, Set Identifier)])
                       _hdIcollectedConstraints :: ([(NontermIdent, ConstructorIdent, [Type])])
                       _hdIcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                       _hdIcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                       _hdIcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                       _hdIcollectedMacros :: ([(NontermIdent, ConstructorIdent, MaybeMacro)])
                       _hdIcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                       _hdIcollectedNames :: (Set Identifier)
                       _hdIcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                       _hdIcollectedSetNames :: (Set Identifier)
                       _hdIcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                       _hdIcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                       _hdIctxCollect :: ContextMap
                       _hdIdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                       _hdIderivings :: Derivings
                       _hdIerrors :: (Seq Error)
                       _hdImoduleDecl :: (Maybe (String,String,String))
                       _hdIparamsCollect :: ParamMap
                       _hdIpragmas :: (Options -> Options)
                       _hdIquantCollect :: QuantMap
                       _hdIsemPragmasCollect :: PragmaMap
                       _hdItypeSyns :: TypeSyns
                       _hdIuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                       _hdIwrappers :: (Set NontermIdent)
                       _tlIattrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _tlIattrOrderCollect :: AttrOrderMap
                       _tlIattrs :: (Map NontermIdent (Attributes, Attributes))
                       _tlIblocks :: Blocks
                       _tlIcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                       _tlIcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                       _tlIcollectedConParams :: ([(NontermIdent, ConstructorIdent, Set Identifier)])
                       _tlIcollectedConstraints :: ([(NontermIdent, ConstructorIdent, [Type])])
                       _tlIcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                       _tlIcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                       _tlIcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                       _tlIcollectedMacros :: ([(NontermIdent, ConstructorIdent, MaybeMacro)])
                       _tlIcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                       _tlIcollectedNames :: (Set Identifier)
                       _tlIcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                       _tlIcollectedSetNames :: (Set Identifier)
                       _tlIcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                       _tlIcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                       _tlIctxCollect :: ContextMap
                       _tlIdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                       _tlIderivings :: Derivings
                       _tlIerrors :: (Seq Error)
                       _tlImoduleDecl :: (Maybe (String,String,String))
                       _tlIparamsCollect :: ParamMap
                       _tlIpragmas :: (Options -> Options)
                       _tlIquantCollect :: QuantMap
                       _tlIsemPragmasCollect :: PragmaMap
                       _tlItypeSyns :: TypeSyns
                       _tlIuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                       _tlIwrappers :: (Set NontermIdent)
                       -- use rule "src-ag/Transform.ag"(line 893, column 55)
                       _lhsOattrOrderCollect =
                           ({-# LINE 893 "src-ag/Transform.ag" #-}
                            _hdIattrOrderCollect `orderMapUnion` _tlIattrOrderCollect
                            {-# LINE 5070 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 46, column 19)
                       _lhsOblocks =
                           ({-# LINE 46 "src-ag/Transform.ag" #-}
                            _hdIblocks `mapUnionWithPlusPlus` _tlIblocks
                            {-# LINE 5076 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 164, column 32)
                       _lhsOcollectedArounds =
                           ({-# LINE 164 "src-ag/Transform.ag" #-}
                            _hdIcollectedArounds ++ _tlIcollectedArounds
                            {-# LINE 5082 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 163, column 32)
                       _lhsOcollectedAugments =
                           ({-# LINE 163 "src-ag/Transform.ag" #-}
                            _hdIcollectedAugments ++ _tlIcollectedAugments
                            {-# LINE 5088 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 132, column 31)
                       _lhsOcollectedConParams =
                           ({-# LINE 132 "src-ag/Transform.ag" #-}
                            _hdIcollectedConParams ++ _tlIcollectedConParams
                            {-# LINE 5094 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 131, column 33)
                       _lhsOcollectedConstraints =
                           ({-# LINE 131 "src-ag/Transform.ag" #-}
                            _hdIcollectedConstraints ++ _tlIcollectedConstraints
                            {-# LINE 5100 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 99, column 48)
                       _lhsOcollectedConstructorsMap =
                           ({-# LINE 99 "src-ag/Transform.ag" #-}
                            _hdIcollectedConstructorsMap `mapUnionWithSetUnion` _tlIcollectedConstructorsMap
                            {-# LINE 5106 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 130, column 28)
                       _lhsOcollectedFields =
                           ({-# LINE 130 "src-ag/Transform.ag" #-}
                            _hdIcollectedFields ++ _tlIcollectedFields
                            {-# LINE 5112 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 161, column 32)
                       _lhsOcollectedInsts =
                           ({-# LINE 161 "src-ag/Transform.ag" #-}
                            _hdIcollectedInsts ++ _tlIcollectedInsts
                            {-# LINE 5118 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 1283, column 28)
                       _lhsOcollectedMacros =
                           ({-# LINE 1283 "src-ag/Transform.ag" #-}
                            _hdIcollectedMacros ++ _tlIcollectedMacros
                            {-# LINE 5124 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 165, column 32)
                       _lhsOcollectedMerges =
                           ({-# LINE 165 "src-ag/Transform.ag" #-}
                            _hdIcollectedMerges ++ _tlIcollectedMerges
                            {-# LINE 5130 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 91, column 50)
                       _lhsOcollectedNames =
                           ({-# LINE 91 "src-ag/Transform.ag" #-}
                            _hdIcollectedNames `Set.union` _tlIcollectedNames
                            {-# LINE 5136 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 159, column 32)
                       _lhsOcollectedRules =
                           ({-# LINE 159 "src-ag/Transform.ag" #-}
                            _hdIcollectedRules ++ _tlIcollectedRules
                            {-# LINE 5142 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 90, column 50)
                       _lhsOcollectedSetNames =
                           ({-# LINE 90 "src-ag/Transform.ag" #-}
                            _hdIcollectedSetNames `Set.union` _tlIcollectedSetNames
                            {-# LINE 5148 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 160, column 32)
                       _lhsOcollectedSigs =
                           ({-# LINE 160 "src-ag/Transform.ag" #-}
                            _hdIcollectedSigs ++ _tlIcollectedSigs
                            {-# LINE 5154 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 162, column 32)
                       _lhsOcollectedUniques =
                           ({-# LINE 162 "src-ag/Transform.ag" #-}
                            _hdIcollectedUniques ++ _tlIcollectedUniques
                            {-# LINE 5160 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 948, column 34)
                       _lhsOctxCollect =
                           ({-# LINE 948 "src-ag/Transform.ag" #-}
                            _hdIctxCollect `mergeCtx` _tlIctxCollect
                            {-# LINE 5166 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 990, column 33)
                       _lhsOderivings =
                           ({-# LINE 990 "src-ag/Transform.ag" #-}
                            _hdIderivings `mergeDerivings` _tlIderivings
                            {-# LINE 5172 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 44, column 19)
                       _lhsOerrors =
                           ({-# LINE 44 "src-ag/Transform.ag" #-}
                            _hdIerrors Seq.>< _tlIerrors
                            {-# LINE 5178 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 1187, column 37)
                       _lhsOmoduleDecl =
                           ({-# LINE 1187 "src-ag/Transform.ag" #-}
                            _hdImoduleDecl `mplus` _tlImoduleDecl
                            {-# LINE 5184 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 925, column 37)
                       _lhsOparamsCollect =
                           ({-# LINE 925 "src-ag/Transform.ag" #-}
                            _hdIparamsCollect `mergeParams` _tlIparamsCollect
                            {-# LINE 5190 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 787, column 34)
                       _lhsOpragmas =
                           ({-# LINE 787 "src-ag/Transform.ag" #-}
                            _hdIpragmas . _tlIpragmas
                            {-# LINE 5196 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 973, column 36)
                       _lhsOquantCollect =
                           ({-# LINE 973 "src-ag/Transform.ag" #-}
                            _hdIquantCollect `mergeQuant` _tlIquantCollect
                            {-# LINE 5202 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 865, column 56)
                       _lhsOsemPragmasCollect =
                           ({-# LINE 865 "src-ag/Transform.ag" #-}
                            _hdIsemPragmasCollect `pragmaMapUnion` _tlIsemPragmasCollect
                            {-# LINE 5208 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 626, column 32)
                       _lhsOtypeSyns =
                           ({-# LINE 626 "src-ag/Transform.ag" #-}
                            _hdItypeSyns ++ _tlItypeSyns
                            {-# LINE 5214 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 145, column 15)
                       _lhsOuseMap =
                           ({-# LINE 145 "src-ag/Transform.ag" #-}
                            _hdIuseMap `merge` _tlIuseMap
                            {-# LINE 5220 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 771, column 32)
                       _lhsOwrappers =
                           ({-# LINE 771 "src-ag/Transform.ag" #-}
                            _hdIwrappers `Set.union` _tlIwrappers
                            {-# LINE 5226 "dist/build/Transform" #-}
                            )
                       -- copy rule (up)
                       _lhsOattrDecls =
                           ({-# LINE 144 "src-ag/Transform.ag" #-}
                            _tlIattrDecls
                            {-# LINE 5232 "dist/build/Transform" #-}
                            )
                       -- copy rule (up)
                       _lhsOattrs =
                           ({-# LINE 1316 "src-ag/Transform.ag" #-}
                            _tlIattrs
                            {-# LINE 5238 "dist/build/Transform" #-}
                            )
                       -- copy rule (up)
                       _lhsOdefSets =
                           ({-# LINE 109 "src-ag/Transform.ag" #-}
                            _tlIdefSets
                            {-# LINE 5244 "dist/build/Transform" #-}
                            )
                       -- copy rule (down)
                       _hdOallAttrDecls =
                           ({-# LINE 894 "src-ag/Transform.ag" #-}
                            _lhsIallAttrDecls
                            {-# LINE 5250 "dist/build/Transform" #-}
                            )
                       -- copy rule (down)
                       _hdOallAttrs =
                           ({-# LINE 1306 "src-ag/Transform.ag" #-}
                            _lhsIallAttrs
                            {-# LINE 5256 "dist/build/Transform" #-}
                            )
                       -- copy rule (down)
                       _hdOallConstructors =
                           ({-# LINE 101 "src-ag/Transform.ag" #-}
                            _lhsIallConstructors
                            {-# LINE 5262 "dist/build/Transform" #-}
                            )
                       -- copy rule (down)
                       _hdOallFields =
                           ({-# LINE 136 "src-ag/Transform.ag" #-}
                            _lhsIallFields
                            {-# LINE 5268 "dist/build/Transform" #-}
                            )
                       -- copy rule (down)
                       _hdOallNonterminals =
                           ({-# LINE 93 "src-ag/Transform.ag" #-}
                            _lhsIallNonterminals
                            {-# LINE 5274 "dist/build/Transform" #-}
                            )
                       -- copy rule (down)
                       _hdOattrDecls =
                           ({-# LINE 144 "src-ag/Transform.ag" #-}
                            _lhsIattrDecls
                            {-# LINE 5280 "dist/build/Transform" #-}
                            )
                       -- copy rule (down)
                       _hdOattrs =
                           ({-# LINE 1316 "src-ag/Transform.ag" #-}
                            _lhsIattrs
                            {-# LINE 5286 "dist/build/Transform" #-}
                            )
                       -- copy rule (down)
                       _hdOdefSets =
                           ({-# LINE 109 "src-ag/Transform.ag" #-}
                            _lhsIdefSets
                            {-# LINE 5292 "dist/build/Transform" #-}
                            )
                       -- copy rule (down)
                       _hdOdefinedSets =
                           ({-# LINE 112 "src-ag/Transform.ag" #-}
                            _lhsIdefinedSets
                            {-# LINE 5298 "dist/build/Transform" #-}
                            )
                       -- copy rule (down)
                       _hdOoptions =
                           ({-# LINE 40 "src-ag/Transform.ag" #-}
                            _lhsIoptions
                            {-# LINE 5304 "dist/build/Transform" #-}
                            )
                       -- copy rule (down)
                       _tlOallAttrDecls =
                           ({-# LINE 894 "src-ag/Transform.ag" #-}
                            _lhsIallAttrDecls
                            {-# LINE 5310 "dist/build/Transform" #-}
                            )
                       -- copy rule (down)
                       _tlOallAttrs =
                           ({-# LINE 1306 "src-ag/Transform.ag" #-}
                            _lhsIallAttrs
                            {-# LINE 5316 "dist/build/Transform" #-}
                            )
                       -- copy rule (down)
                       _tlOallConstructors =
                           ({-# LINE 101 "src-ag/Transform.ag" #-}
                            _lhsIallConstructors
                            {-# LINE 5322 "dist/build/Transform" #-}
                            )
                       -- copy rule (down)
                       _tlOallFields =
                           ({-# LINE 136 "src-ag/Transform.ag" #-}
                            _lhsIallFields
                            {-# LINE 5328 "dist/build/Transform" #-}
                            )
                       -- copy rule (down)
                       _tlOallNonterminals =
                           ({-# LINE 93 "src-ag/Transform.ag" #-}
                            _lhsIallNonterminals
                            {-# LINE 5334 "dist/build/Transform" #-}
                            )
                       -- copy rule (chain)
                       _tlOattrDecls =
                           ({-# LINE 144 "src-ag/Transform.ag" #-}
                            _hdIattrDecls
                            {-# LINE 5340 "dist/build/Transform" #-}
                            )
                       -- copy rule (chain)
                       _tlOattrs =
                           ({-# LINE 1316 "src-ag/Transform.ag" #-}
                            _hdIattrs
                            {-# LINE 5346 "dist/build/Transform" #-}
                            )
                       -- copy rule (chain)
                       _tlOdefSets =
                           ({-# LINE 109 "src-ag/Transform.ag" #-}
                            _hdIdefSets
                            {-# LINE 5352 "dist/build/Transform" #-}
                            )
                       -- copy rule (down)
                       _tlOdefinedSets =
                           ({-# LINE 112 "src-ag/Transform.ag" #-}
                            _lhsIdefinedSets
                            {-# LINE 5358 "dist/build/Transform" #-}
                            )
                       -- copy rule (down)
                       _tlOoptions =
                           ({-# LINE 40 "src-ag/Transform.ag" #-}
                            _lhsIoptions
                            {-# LINE 5364 "dist/build/Transform" #-}
                            )
                       ( _hdIattrDecls,_hdIattrOrderCollect,_hdIattrs,_hdIblocks,_hdIcollectedArounds,_hdIcollectedAugments,_hdIcollectedConParams,_hdIcollectedConstraints,_hdIcollectedConstructorsMap,_hdIcollectedFields,_hdIcollectedInsts,_hdIcollectedMacros,_hdIcollectedMerges,_hdIcollectedNames,_hdIcollectedRules,_hdIcollectedSetNames,_hdIcollectedSigs,_hdIcollectedUniques,_hdIctxCollect,_hdIdefSets,_hdIderivings,_hdIerrors,_hdImoduleDecl,_hdIparamsCollect,_hdIpragmas,_hdIquantCollect,_hdIsemPragmasCollect,_hdItypeSyns,_hdIuseMap,_hdIwrappers) =
                           hd_ _hdOallAttrDecls _hdOallAttrs _hdOallConstructors _hdOallFields _hdOallNonterminals _hdOattrDecls _hdOattrs _hdOdefSets _hdOdefinedSets _hdOoptions
                       ( _tlIattrDecls,_tlIattrOrderCollect,_tlIattrs,_tlIblocks,_tlIcollectedArounds,_tlIcollectedAugments,_tlIcollectedConParams,_tlIcollectedConstraints,_tlIcollectedConstructorsMap,_tlIcollectedFields,_tlIcollectedInsts,_tlIcollectedMacros,_tlIcollectedMerges,_tlIcollectedNames,_tlIcollectedRules,_tlIcollectedSetNames,_tlIcollectedSigs,_tlIcollectedUniques,_tlIctxCollect,_tlIdefSets,_tlIderivings,_tlIerrors,_tlImoduleDecl,_tlIparamsCollect,_tlIpragmas,_tlIquantCollect,_tlIsemPragmasCollect,_tlItypeSyns,_tlIuseMap,_tlIwrappers) =
                           tl_ _tlOallAttrDecls _tlOallAttrs _tlOallConstructors _tlOallFields _tlOallNonterminals _tlOattrDecls _tlOattrs _tlOdefSets _tlOdefinedSets _tlOoptions
                       ___node =
                           (Syn_Elems _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers)
                   in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConParams,_lhsOcollectedConstraints,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMacros,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))))
sem_Elems_Nil :: T_Elems
sem_Elems_Nil =
    (T_Elems (\ _lhsIallAttrDecls
                _lhsIallAttrs
                _lhsIallConstructors
                _lhsIallFields
                _lhsIallNonterminals
                _lhsIattrDecls
                _lhsIattrs
                _lhsIdefSets
                _lhsIdefinedSets
                _lhsIoptions ->
                  (let _lhsOattrOrderCollect :: AttrOrderMap
                       _lhsOblocks :: Blocks
                       _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                       _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                       _lhsOcollectedConParams :: ([(NontermIdent, ConstructorIdent, Set Identifier)])
                       _lhsOcollectedConstraints :: ([(NontermIdent, ConstructorIdent, [Type])])
                       _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                       _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                       _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                       _lhsOcollectedMacros :: ([(NontermIdent, ConstructorIdent, MaybeMacro)])
                       _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                       _lhsOcollectedNames :: (Set Identifier)
                       _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                       _lhsOcollectedSetNames :: (Set Identifier)
                       _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                       _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                       _lhsOctxCollect :: ContextMap
                       _lhsOderivings :: Derivings
                       _lhsOerrors :: (Seq Error)
                       _lhsOmoduleDecl :: (Maybe (String,String,String))
                       _lhsOparamsCollect :: ParamMap
                       _lhsOpragmas :: (Options -> Options)
                       _lhsOquantCollect :: QuantMap
                       _lhsOsemPragmasCollect :: PragmaMap
                       _lhsOtypeSyns :: TypeSyns
                       _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                       _lhsOwrappers :: (Set NontermIdent)
                       _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _lhsOattrs :: (Map NontermIdent (Attributes, Attributes))
                       _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                       -- use rule "src-ag/Transform.ag"(line 893, column 55)
                       _lhsOattrOrderCollect =
                           ({-# LINE 893 "src-ag/Transform.ag" #-}
                            Map.empty
                            {-# LINE 5419 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 46, column 19)
                       _lhsOblocks =
                           ({-# LINE 46 "src-ag/Transform.ag" #-}
                            Map.empty
                            {-# LINE 5425 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 164, column 32)
                       _lhsOcollectedArounds =
                           ({-# LINE 164 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 5431 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 163, column 32)
                       _lhsOcollectedAugments =
                           ({-# LINE 163 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 5437 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 132, column 31)
                       _lhsOcollectedConParams =
                           ({-# LINE 132 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 5443 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 131, column 33)
                       _lhsOcollectedConstraints =
                           ({-# LINE 131 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 5449 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 99, column 48)
                       _lhsOcollectedConstructorsMap =
                           ({-# LINE 99 "src-ag/Transform.ag" #-}
                            Map.empty
                            {-# LINE 5455 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 130, column 28)
                       _lhsOcollectedFields =
                           ({-# LINE 130 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 5461 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 161, column 32)
                       _lhsOcollectedInsts =
                           ({-# LINE 161 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 5467 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 1283, column 28)
                       _lhsOcollectedMacros =
                           ({-# LINE 1283 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 5473 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 165, column 32)
                       _lhsOcollectedMerges =
                           ({-# LINE 165 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 5479 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 91, column 50)
                       _lhsOcollectedNames =
                           ({-# LINE 91 "src-ag/Transform.ag" #-}
                            Set.empty
                            {-# LINE 5485 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 159, column 32)
                       _lhsOcollectedRules =
                           ({-# LINE 159 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 5491 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 90, column 50)
                       _lhsOcollectedSetNames =
                           ({-# LINE 90 "src-ag/Transform.ag" #-}
                            Set.empty
                            {-# LINE 5497 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 160, column 32)
                       _lhsOcollectedSigs =
                           ({-# LINE 160 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 5503 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 162, column 32)
                       _lhsOcollectedUniques =
                           ({-# LINE 162 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 5509 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 948, column 34)
                       _lhsOctxCollect =
                           ({-# LINE 948 "src-ag/Transform.ag" #-}
                            Map.empty
                            {-# LINE 5515 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 990, column 33)
                       _lhsOderivings =
                           ({-# LINE 990 "src-ag/Transform.ag" #-}
                            Map.empty
                            {-# LINE 5521 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 44, column 19)
                       _lhsOerrors =
                           ({-# LINE 44 "src-ag/Transform.ag" #-}
                            Seq.empty
                            {-# LINE 5527 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 1187, column 37)
                       _lhsOmoduleDecl =
                           ({-# LINE 1187 "src-ag/Transform.ag" #-}
                            mzero
                            {-# LINE 5533 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 925, column 37)
                       _lhsOparamsCollect =
                           ({-# LINE 925 "src-ag/Transform.ag" #-}
                            Map.empty
                            {-# LINE 5539 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 787, column 34)
                       _lhsOpragmas =
                           ({-# LINE 787 "src-ag/Transform.ag" #-}
                            id
                            {-# LINE 5545 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 973, column 36)
                       _lhsOquantCollect =
                           ({-# LINE 973 "src-ag/Transform.ag" #-}
                            Map.empty
                            {-# LINE 5551 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 865, column 56)
                       _lhsOsemPragmasCollect =
                           ({-# LINE 865 "src-ag/Transform.ag" #-}
                            Map.empty
                            {-# LINE 5557 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 626, column 32)
                       _lhsOtypeSyns =
                           ({-# LINE 626 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 5563 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 145, column 15)
                       _lhsOuseMap =
                           ({-# LINE 145 "src-ag/Transform.ag" #-}
                            Map.empty
                            {-# LINE 5569 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 771, column 32)
                       _lhsOwrappers =
                           ({-# LINE 771 "src-ag/Transform.ag" #-}
                            Set.empty
                            {-# LINE 5575 "dist/build/Transform" #-}
                            )
                       -- copy rule (chain)
                       _lhsOattrDecls =
                           ({-# LINE 144 "src-ag/Transform.ag" #-}
                            _lhsIattrDecls
                            {-# LINE 5581 "dist/build/Transform" #-}
                            )
                       -- copy rule (chain)
                       _lhsOattrs =
                           ({-# LINE 1316 "src-ag/Transform.ag" #-}
                            _lhsIattrs
                            {-# LINE 5587 "dist/build/Transform" #-}
                            )
                       -- copy rule (chain)
                       _lhsOdefSets =
                           ({-# LINE 109 "src-ag/Transform.ag" #-}
                            _lhsIdefSets
                            {-# LINE 5593 "dist/build/Transform" #-}
                            )
                       ___node =
                           (Syn_Elems _lhsOattrDecls _lhsOattrOrderCollect _lhsOattrs _lhsOblocks _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedConParams _lhsOcollectedConstraints _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedMacros _lhsOcollectedMerges _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOquantCollect _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers)
                   in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOattrs,_lhsOblocks,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedConParams,_lhsOcollectedConstraints,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedMacros,_lhsOcollectedMerges,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOquantCollect,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))))
-- Field -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         allNonterminals      : Set NontermIdent
      synthesized attributes:
         collectedConstraints : [Type]
         collectedFields      : [(Identifier, Type)]
   alternatives:
      alternative FChild:
         child name           : {Identifier}
         child tp             : {Type}
      alternative FCtx:
         child tps            : {[Type]}
-}
-- cata
sem_Field :: Field ->
             T_Field
sem_Field (FChild _name _tp) =
    (sem_Field_FChild _name _tp)
sem_Field (FCtx _tps) =
    (sem_Field_FCtx _tps)
-- semantic domain
newtype T_Field = T_Field ((Set NontermIdent) ->
                           ( ([Type]),([(Identifier, Type)])))
data Inh_Field = Inh_Field {allNonterminals_Inh_Field :: !((Set NontermIdent))}
data Syn_Field = Syn_Field {collectedConstraints_Syn_Field :: !(([Type])),collectedFields_Syn_Field :: !(([(Identifier, Type)]))}
wrap_Field :: T_Field ->
              Inh_Field ->
              Syn_Field
wrap_Field (T_Field sem) (Inh_Field _lhsIallNonterminals) =
    (let ( _lhsOcollectedConstraints,_lhsOcollectedFields) = sem _lhsIallNonterminals
     in  (Syn_Field _lhsOcollectedConstraints _lhsOcollectedFields))
sem_Field_FChild :: Identifier ->
                    Type ->
                    T_Field
sem_Field_FChild name_ tp_ =
    (T_Field (\ _lhsIallNonterminals ->
                  (let _lhsOcollectedFields :: ([(Identifier, Type)])
                       _lhsOcollectedConstraints :: ([Type])
                       -- "src-ag/Transform.ag"(line 564, column 3)
                       _lhsOcollectedFields =
                           ({-# LINE 564 "src-ag/Transform.ag" #-}
                            [(name_, makeType _lhsIallNonterminals tp_)]
                            {-# LINE 5642 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 570, column 46)
                       _lhsOcollectedConstraints =
                           ({-# LINE 570 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 5648 "dist/build/Transform" #-}
                            )
                       ___node =
                           (Syn_Field _lhsOcollectedConstraints _lhsOcollectedFields)
                   in  ( _lhsOcollectedConstraints,_lhsOcollectedFields))))
sem_Field_FCtx :: ([Type]) ->
                  T_Field
sem_Field_FCtx tps_ =
    (T_Field (\ _lhsIallNonterminals ->
                  (let _lhsOcollectedConstraints :: ([Type])
                       _lhsOcollectedFields :: ([(Identifier, Type)])
                       -- "src-ag/Transform.ag"(line 573, column 3)
                       _lhsOcollectedConstraints =
                           ({-# LINE 573 "src-ag/Transform.ag" #-}
                            tps_
                            {-# LINE 5663 "dist/build/Transform" #-}
                            )
                       -- use rule "src-ag/Transform.ag"(line 561, column 41)
                       _lhsOcollectedFields =
                           ({-# LINE 561 "src-ag/Transform.ag" #-}
                            []
                            {-# LINE 5669 "dist/build/Transform" #-}
                            )
                       ___node =
                           (Syn_Field _lhsOcollectedConstraints _lhsOcollectedFields)
                   in  ( _lhsOcollectedConstraints,_lhsOcollectedFields))))
-- Fields ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         allNonterminals      : Set NontermIdent
      synthesized attributes:
         collectedConstraints : [Type]
         collectedFields      : [(Identifier, Type)]
   alternatives:
      alternative Cons:
         child hd             : Field 
         child tl             : Fields 
      alternative Nil:
-}
-- cata
sem_Fields :: Fields ->
              T_Fields
sem_Fields list =
    (Prelude.foldr sem_Fields_Cons sem_Fields_Nil (Prelude.map sem_Field list))
-- semantic domain
newtype T_Fields = T_Fields ((Set NontermIdent) ->
                             ( ([Type]),([(Identifier, Type)])))
data Inh_Fields = Inh_Fields {allNonterminals_Inh_Fields :: !((Set NontermIdent))}
data Syn_Fields = Syn_Fields {collectedConstraints_Syn_Fields :: !(([Type])),collectedFields_Syn_Fields :: !(([(Identifier, Type)]))}
wrap_Fields :: T_Fields ->
               Inh_Fields ->
               Syn_Fields
wrap_Fields (T_Fields sem) (Inh_Fields _lhsIallNonterminals) =
    (let ( _lhsOcollectedConstraints,_lhsOcollectedFields) = sem _lhsIallNonterminals
     in  (Syn_Fields _lhsOcollectedConstraints _lhsOcollectedFields))
sem_Fields_Cons :: T_Field ->
                   T_Fields ->
                   T_Fields
sem_Fields_Cons (T_Field hd_) (T_Fields tl_) =
    (T_Fields (\ _lhsIallNonterminals ->
                   (let _lhsOcollectedConstraints :: ([Type])
                        _lhsOcollectedFields :: ([(Identifier, Type)])
                        _hdOallNonterminals :: (Set NontermIdent)
                        _tlOallNonterminals :: (Set NontermIdent)
                        _hdIcollectedConstraints :: ([Type])
                        _hdIcollectedFields :: ([(Identifier, Type)])
                        _tlIcollectedConstraints :: ([Type])
                        _tlIcollectedFields :: ([(Identifier, Type)])
                        -- use rule "src-ag/Transform.ag"(line 570, column 46)
                        _lhsOcollectedConstraints =
                            ({-# LINE 570 "src-ag/Transform.ag" #-}
                             _hdIcollectedConstraints ++ _tlIcollectedConstraints
                             {-# LINE 5721 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 561, column 41)
                        _lhsOcollectedFields =
                            ({-# LINE 561 "src-ag/Transform.ag" #-}
                             _hdIcollectedFields ++ _tlIcollectedFields
                             {-# LINE 5727 "dist/build/Transform" #-}
                             )
                        -- copy rule (down)
                        _hdOallNonterminals =
                            ({-# LINE 93 "src-ag/Transform.ag" #-}
                             _lhsIallNonterminals
                             {-# LINE 5733 "dist/build/Transform" #-}
                             )
                        -- copy rule (down)
                        _tlOallNonterminals =
                            ({-# LINE 93 "src-ag/Transform.ag" #-}
                             _lhsIallNonterminals
                             {-# LINE 5739 "dist/build/Transform" #-}
                             )
                        ( _hdIcollectedConstraints,_hdIcollectedFields) =
                            hd_ _hdOallNonterminals
                        ( _tlIcollectedConstraints,_tlIcollectedFields) =
                            tl_ _tlOallNonterminals
                        ___node =
                            (Syn_Fields _lhsOcollectedConstraints _lhsOcollectedFields)
                    in  ( _lhsOcollectedConstraints,_lhsOcollectedFields))))
sem_Fields_Nil :: T_Fields
sem_Fields_Nil =
    (T_Fields (\ _lhsIallNonterminals ->
                   (let _lhsOcollectedConstraints :: ([Type])
                        _lhsOcollectedFields :: ([(Identifier, Type)])
                        -- use rule "src-ag/Transform.ag"(line 570, column 46)
                        _lhsOcollectedConstraints =
                            ({-# LINE 570 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 5757 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 561, column 41)
                        _lhsOcollectedFields =
                            ({-# LINE 561 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 5763 "dist/build/Transform" #-}
                             )
                        ___node =
                            (Syn_Fields _lhsOcollectedConstraints _lhsOcollectedFields)
                    in  ( _lhsOcollectedConstraints,_lhsOcollectedFields))))
-- NontSet -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allFields            : DataTypes
         allNonterminals      : Set NontermIdent
         definedSets          : DefinedSets
      synthesized attributes:
         collectedNames       : Set Identifier
         errors               : Seq Error
         nontSet              : Set NontermIdent
   alternatives:
      alternative All:
      alternative Difference:
         child set1           : NontSet 
         child set2           : NontSet 
      alternative Intersect:
         child set1           : NontSet 
         child set2           : NontSet 
      alternative NamedSet:
         child name           : {NontermIdent}
         visit 0:
            local nontSet     : _
            local errors      : _
      alternative Path:
         child from           : {NontermIdent}
         child to             : {NontermIdent}
      alternative Union:
         child set1           : NontSet 
         child set2           : NontSet 
-}
-- cata
sem_NontSet :: NontSet ->
               T_NontSet
sem_NontSet (All) =
    (sem_NontSet_All)
sem_NontSet (Difference _set1 _set2) =
    (sem_NontSet_Difference (sem_NontSet _set1) (sem_NontSet _set2))
sem_NontSet (Intersect _set1 _set2) =
    (sem_NontSet_Intersect (sem_NontSet _set1) (sem_NontSet _set2))
sem_NontSet (NamedSet _name) =
    (sem_NontSet_NamedSet _name)
sem_NontSet (Path _from _to) =
    (sem_NontSet_Path _from _to)
sem_NontSet (Union _set1 _set2) =
    (sem_NontSet_Union (sem_NontSet _set1) (sem_NontSet _set2))
-- semantic domain
newtype T_NontSet = T_NontSet (DataTypes ->
                               (Set NontermIdent) ->
                               DefinedSets ->
                               ( (Set Identifier),(Seq Error),(Set NontermIdent)))
data Inh_NontSet = Inh_NontSet {allFields_Inh_NontSet :: !(DataTypes),allNonterminals_Inh_NontSet :: !((Set NontermIdent)),definedSets_Inh_NontSet :: !(DefinedSets)}
data Syn_NontSet = Syn_NontSet {collectedNames_Syn_NontSet :: !((Set Identifier)),errors_Syn_NontSet :: !((Seq Error)),nontSet_Syn_NontSet :: !((Set NontermIdent))}
wrap_NontSet :: T_NontSet ->
                Inh_NontSet ->
                Syn_NontSet
wrap_NontSet (T_NontSet sem) (Inh_NontSet _lhsIallFields _lhsIallNonterminals _lhsIdefinedSets) =
    (let ( _lhsOcollectedNames,_lhsOerrors,_lhsOnontSet) = sem _lhsIallFields _lhsIallNonterminals _lhsIdefinedSets
     in  (Syn_NontSet _lhsOcollectedNames _lhsOerrors _lhsOnontSet))
sem_NontSet_All :: T_NontSet
sem_NontSet_All =
    (T_NontSet (\ _lhsIallFields
                  _lhsIallNonterminals
                  _lhsIdefinedSets ->
                    (let _lhsOnontSet :: (Set NontermIdent)
                         _lhsOcollectedNames :: (Set Identifier)
                         _lhsOerrors :: (Seq Error)
                         -- "src-ag/Transform.ag"(line 717, column 16)
                         _lhsOnontSet =
                             ({-# LINE 717 "src-ag/Transform.ag" #-}
                              _lhsIallNonterminals
                              {-# LINE 5839 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 91, column 50)
                         _lhsOcollectedNames =
                             ({-# LINE 91 "src-ag/Transform.ag" #-}
                              Set.empty
                              {-# LINE 5845 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 44, column 19)
                         _lhsOerrors =
                             ({-# LINE 44 "src-ag/Transform.ag" #-}
                              Seq.empty
                              {-# LINE 5851 "dist/build/Transform" #-}
                              )
                         ___node =
                             (Syn_NontSet _lhsOcollectedNames _lhsOerrors _lhsOnontSet)
                     in  ( _lhsOcollectedNames,_lhsOerrors,_lhsOnontSet))))
sem_NontSet_Difference :: T_NontSet ->
                          T_NontSet ->
                          T_NontSet
sem_NontSet_Difference (T_NontSet set1_) (T_NontSet set2_) =
    (T_NontSet (\ _lhsIallFields
                  _lhsIallNonterminals
                  _lhsIdefinedSets ->
                    (let _lhsOnontSet :: (Set NontermIdent)
                         _lhsOcollectedNames :: (Set Identifier)
                         _lhsOerrors :: (Seq Error)
                         _set1OallFields :: DataTypes
                         _set1OallNonterminals :: (Set NontermIdent)
                         _set1OdefinedSets :: DefinedSets
                         _set2OallFields :: DataTypes
                         _set2OallNonterminals :: (Set NontermIdent)
                         _set2OdefinedSets :: DefinedSets
                         _set1IcollectedNames :: (Set Identifier)
                         _set1Ierrors :: (Seq Error)
                         _set1InontSet :: (Set NontermIdent)
                         _set2IcollectedNames :: (Set Identifier)
                         _set2Ierrors :: (Seq Error)
                         _set2InontSet :: (Set NontermIdent)
                         -- "src-ag/Transform.ag"(line 723, column 16)
                         _lhsOnontSet =
                             ({-# LINE 723 "src-ag/Transform.ag" #-}
                              Set.difference    _set1InontSet _set2InontSet
                              {-# LINE 5882 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 91, column 50)
                         _lhsOcollectedNames =
                             ({-# LINE 91 "src-ag/Transform.ag" #-}
                              _set1IcollectedNames `Set.union` _set2IcollectedNames
                              {-# LINE 5888 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 44, column 19)
                         _lhsOerrors =
                             ({-# LINE 44 "src-ag/Transform.ag" #-}
                              _set1Ierrors Seq.>< _set2Ierrors
                              {-# LINE 5894 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _set1OallFields =
                             ({-# LINE 136 "src-ag/Transform.ag" #-}
                              _lhsIallFields
                              {-# LINE 5900 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _set1OallNonterminals =
                             ({-# LINE 93 "src-ag/Transform.ag" #-}
                              _lhsIallNonterminals
                              {-# LINE 5906 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _set1OdefinedSets =
                             ({-# LINE 112 "src-ag/Transform.ag" #-}
                              _lhsIdefinedSets
                              {-# LINE 5912 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _set2OallFields =
                             ({-# LINE 136 "src-ag/Transform.ag" #-}
                              _lhsIallFields
                              {-# LINE 5918 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _set2OallNonterminals =
                             ({-# LINE 93 "src-ag/Transform.ag" #-}
                              _lhsIallNonterminals
                              {-# LINE 5924 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _set2OdefinedSets =
                             ({-# LINE 112 "src-ag/Transform.ag" #-}
                              _lhsIdefinedSets
                              {-# LINE 5930 "dist/build/Transform" #-}
                              )
                         ( _set1IcollectedNames,_set1Ierrors,_set1InontSet) =
                             set1_ _set1OallFields _set1OallNonterminals _set1OdefinedSets
                         ( _set2IcollectedNames,_set2Ierrors,_set2InontSet) =
                             set2_ _set2OallFields _set2OallNonterminals _set2OdefinedSets
                         ___node =
                             (Syn_NontSet _lhsOcollectedNames _lhsOerrors _lhsOnontSet)
                     in  ( _lhsOcollectedNames,_lhsOerrors,_lhsOnontSet))))
sem_NontSet_Intersect :: T_NontSet ->
                         T_NontSet ->
                         T_NontSet
sem_NontSet_Intersect (T_NontSet set1_) (T_NontSet set2_) =
    (T_NontSet (\ _lhsIallFields
                  _lhsIallNonterminals
                  _lhsIdefinedSets ->
                    (let _lhsOnontSet :: (Set NontermIdent)
                         _lhsOcollectedNames :: (Set Identifier)
                         _lhsOerrors :: (Seq Error)
                         _set1OallFields :: DataTypes
                         _set1OallNonterminals :: (Set NontermIdent)
                         _set1OdefinedSets :: DefinedSets
                         _set2OallFields :: DataTypes
                         _set2OallNonterminals :: (Set NontermIdent)
                         _set2OdefinedSets :: DefinedSets
                         _set1IcollectedNames :: (Set Identifier)
                         _set1Ierrors :: (Seq Error)
                         _set1InontSet :: (Set NontermIdent)
                         _set2IcollectedNames :: (Set Identifier)
                         _set2Ierrors :: (Seq Error)
                         _set2InontSet :: (Set NontermIdent)
                         -- "src-ag/Transform.ag"(line 722, column 16)
                         _lhsOnontSet =
                             ({-# LINE 722 "src-ag/Transform.ag" #-}
                              Set.intersection  _set1InontSet _set2InontSet
                              {-# LINE 5965 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 91, column 50)
                         _lhsOcollectedNames =
                             ({-# LINE 91 "src-ag/Transform.ag" #-}
                              _set1IcollectedNames `Set.union` _set2IcollectedNames
                              {-# LINE 5971 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 44, column 19)
                         _lhsOerrors =
                             ({-# LINE 44 "src-ag/Transform.ag" #-}
                              _set1Ierrors Seq.>< _set2Ierrors
                              {-# LINE 5977 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _set1OallFields =
                             ({-# LINE 136 "src-ag/Transform.ag" #-}
                              _lhsIallFields
                              {-# LINE 5983 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _set1OallNonterminals =
                             ({-# LINE 93 "src-ag/Transform.ag" #-}
                              _lhsIallNonterminals
                              {-# LINE 5989 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _set1OdefinedSets =
                             ({-# LINE 112 "src-ag/Transform.ag" #-}
                              _lhsIdefinedSets
                              {-# LINE 5995 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _set2OallFields =
                             ({-# LINE 136 "src-ag/Transform.ag" #-}
                              _lhsIallFields
                              {-# LINE 6001 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _set2OallNonterminals =
                             ({-# LINE 93 "src-ag/Transform.ag" #-}
                              _lhsIallNonterminals
                              {-# LINE 6007 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _set2OdefinedSets =
                             ({-# LINE 112 "src-ag/Transform.ag" #-}
                              _lhsIdefinedSets
                              {-# LINE 6013 "dist/build/Transform" #-}
                              )
                         ( _set1IcollectedNames,_set1Ierrors,_set1InontSet) =
                             set1_ _set1OallFields _set1OallNonterminals _set1OdefinedSets
                         ( _set2IcollectedNames,_set2Ierrors,_set2InontSet) =
                             set2_ _set2OallFields _set2OallNonterminals _set2OdefinedSets
                         ___node =
                             (Syn_NontSet _lhsOcollectedNames _lhsOerrors _lhsOnontSet)
                     in  ( _lhsOcollectedNames,_lhsOerrors,_lhsOnontSet))))
sem_NontSet_NamedSet :: NontermIdent ->
                        T_NontSet
sem_NontSet_NamedSet name_ =
    (T_NontSet (\ _lhsIallFields
                  _lhsIallNonterminals
                  _lhsIdefinedSets ->
                    (let _lhsOcollectedNames :: (Set Identifier)
                         _lhsOerrors :: (Seq Error)
                         _lhsOnontSet :: (Set NontermIdent)
                         -- "src-ag/Transform.ag"(line 588, column 14)
                         _lhsOcollectedNames =
                             ({-# LINE 588 "src-ag/Transform.ag" #-}
                              Set.singleton name_
                              {-# LINE 6035 "dist/build/Transform" #-}
                              )
                         -- "src-ag/Transform.ag"(line 718, column 20)
                         (_nontSet,_errors) =
                             ({-# LINE 718 "src-ag/Transform.ag" #-}
                              case Map.lookup name_ _lhsIdefinedSets of
                                           Nothing  -> (Set.empty, Seq.singleton (UndefNont name_))
                                           Just set -> (set, Seq.empty)
                              {-# LINE 6043 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 44, column 19)
                         _lhsOerrors =
                             ({-# LINE 44 "src-ag/Transform.ag" #-}
                              _errors
                              {-# LINE 6049 "dist/build/Transform" #-}
                              )
                         -- copy rule (from local)
                         _lhsOnontSet =
                             ({-# LINE 117 "src-ag/Transform.ag" #-}
                              _nontSet
                              {-# LINE 6055 "dist/build/Transform" #-}
                              )
                         ___node =
                             (Syn_NontSet _lhsOcollectedNames _lhsOerrors _lhsOnontSet)
                     in  ( _lhsOcollectedNames,_lhsOerrors,_lhsOnontSet))))
sem_NontSet_Path :: NontermIdent ->
                    NontermIdent ->
                    T_NontSet
sem_NontSet_Path from_ to_ =
    (T_NontSet (\ _lhsIallFields
                  _lhsIallNonterminals
                  _lhsIdefinedSets ->
                    (let _lhsOnontSet :: (Set NontermIdent)
                         _lhsOerrors :: (Seq Error)
                         _lhsOcollectedNames :: (Set Identifier)
                         -- "src-ag/Transform.ag"(line 724, column 16)
                         _lhsOnontSet =
                             ({-# LINE 724 "src-ag/Transform.ag" #-}
                              let table = flattenDatas _lhsIallFields
                              in path table from_ to_
                              {-# LINE 6075 "dist/build/Transform" #-}
                              )
                         -- "src-ag/Transform.ag"(line 726, column 16)
                         _lhsOerrors =
                             ({-# LINE 726 "src-ag/Transform.ag" #-}
                              let check name | Set.member name _lhsIallNonterminals
                                                         = Seq.empty
                                             | otherwise = Seq.singleton (UndefNont name)
                              in check from_ >< check to_
                              {-# LINE 6084 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 91, column 50)
                         _lhsOcollectedNames =
                             ({-# LINE 91 "src-ag/Transform.ag" #-}
                              Set.empty
                              {-# LINE 6090 "dist/build/Transform" #-}
                              )
                         ___node =
                             (Syn_NontSet _lhsOcollectedNames _lhsOerrors _lhsOnontSet)
                     in  ( _lhsOcollectedNames,_lhsOerrors,_lhsOnontSet))))
sem_NontSet_Union :: T_NontSet ->
                     T_NontSet ->
                     T_NontSet
sem_NontSet_Union (T_NontSet set1_) (T_NontSet set2_) =
    (T_NontSet (\ _lhsIallFields
                  _lhsIallNonterminals
                  _lhsIdefinedSets ->
                    (let _lhsOnontSet :: (Set NontermIdent)
                         _lhsOcollectedNames :: (Set Identifier)
                         _lhsOerrors :: (Seq Error)
                         _set1OallFields :: DataTypes
                         _set1OallNonterminals :: (Set NontermIdent)
                         _set1OdefinedSets :: DefinedSets
                         _set2OallFields :: DataTypes
                         _set2OallNonterminals :: (Set NontermIdent)
                         _set2OdefinedSets :: DefinedSets
                         _set1IcollectedNames :: (Set Identifier)
                         _set1Ierrors :: (Seq Error)
                         _set1InontSet :: (Set NontermIdent)
                         _set2IcollectedNames :: (Set Identifier)
                         _set2Ierrors :: (Seq Error)
                         _set2InontSet :: (Set NontermIdent)
                         -- "src-ag/Transform.ag"(line 721, column 16)
                         _lhsOnontSet =
                             ({-# LINE 721 "src-ag/Transform.ag" #-}
                              Set.union         _set1InontSet _set2InontSet
                              {-# LINE 6121 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 91, column 50)
                         _lhsOcollectedNames =
                             ({-# LINE 91 "src-ag/Transform.ag" #-}
                              _set1IcollectedNames `Set.union` _set2IcollectedNames
                              {-# LINE 6127 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 44, column 19)
                         _lhsOerrors =
                             ({-# LINE 44 "src-ag/Transform.ag" #-}
                              _set1Ierrors Seq.>< _set2Ierrors
                              {-# LINE 6133 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _set1OallFields =
                             ({-# LINE 136 "src-ag/Transform.ag" #-}
                              _lhsIallFields
                              {-# LINE 6139 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _set1OallNonterminals =
                             ({-# LINE 93 "src-ag/Transform.ag" #-}
                              _lhsIallNonterminals
                              {-# LINE 6145 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _set1OdefinedSets =
                             ({-# LINE 112 "src-ag/Transform.ag" #-}
                              _lhsIdefinedSets
                              {-# LINE 6151 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _set2OallFields =
                             ({-# LINE 136 "src-ag/Transform.ag" #-}
                              _lhsIallFields
                              {-# LINE 6157 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _set2OallNonterminals =
                             ({-# LINE 93 "src-ag/Transform.ag" #-}
                              _lhsIallNonterminals
                              {-# LINE 6163 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _set2OdefinedSets =
                             ({-# LINE 112 "src-ag/Transform.ag" #-}
                              _lhsIdefinedSets
                              {-# LINE 6169 "dist/build/Transform" #-}
                              )
                         ( _set1IcollectedNames,_set1Ierrors,_set1InontSet) =
                             set1_ _set1OallFields _set1OallNonterminals _set1OdefinedSets
                         ( _set2IcollectedNames,_set2Ierrors,_set2InontSet) =
                             set2_ _set2OallFields _set2OallNonterminals _set2OdefinedSets
                         ___node =
                             (Syn_NontSet _lhsOcollectedNames _lhsOerrors _lhsOnontSet)
                     in  ( _lhsOcollectedNames,_lhsOerrors,_lhsOnontSet))))
-- Pattern -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : Pattern 
         definedAttrs         : [AttrName]
         definedInsts         : [Identifier]
         patunder             : [AttrName]->Pattern
         stpos                : Pos
   alternatives:
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         visit 0:
            local copy        : _
      alternative Constr:
         child name           : {ConstructorIdent}
         child pats           : Patterns 
         visit 0:
            local copy        : _
      alternative Irrefutable:
         child pat            : Pattern 
         visit 0:
            local copy        : _
      alternative Product:
         child pos            : {Pos}
         child pats           : Patterns 
         visit 0:
            local copy        : _
      alternative Underscore:
         child pos            : {Pos}
         visit 0:
            local copy        : _
-}
-- cata
sem_Pattern :: Pattern ->
               T_Pattern
sem_Pattern (Alias _field _attr _pat) =
    (sem_Pattern_Alias _field _attr (sem_Pattern _pat))
sem_Pattern (Constr _name _pats) =
    (sem_Pattern_Constr _name (sem_Patterns _pats))
sem_Pattern (Irrefutable _pat) =
    (sem_Pattern_Irrefutable (sem_Pattern _pat))
sem_Pattern (Product _pos _pats) =
    (sem_Pattern_Product _pos (sem_Patterns _pats))
sem_Pattern (Underscore _pos) =
    (sem_Pattern_Underscore _pos)
-- semantic domain
newtype T_Pattern = T_Pattern (( Pattern,([AttrName]),([Identifier]),([AttrName]->Pattern),Pos))
data Inh_Pattern = Inh_Pattern {}
data Syn_Pattern = Syn_Pattern {copy_Syn_Pattern :: !(Pattern),definedAttrs_Syn_Pattern :: !(([AttrName])),definedInsts_Syn_Pattern :: !(([Identifier])),patunder_Syn_Pattern :: !(([AttrName]->Pattern)),stpos_Syn_Pattern :: !(Pos)}
wrap_Pattern :: T_Pattern ->
                Inh_Pattern ->
                Syn_Pattern
wrap_Pattern (T_Pattern sem) (Inh_Pattern) =
    (let ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder,_lhsOstpos) = sem
     in  (Syn_Pattern _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder _lhsOstpos))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern ->
                     T_Pattern
sem_Pattern_Alias field_ attr_ (T_Pattern pat_) =
    (T_Pattern (let _lhsOdefinedAttrs :: ([AttrName])
                    _lhsOpatunder :: ([AttrName]->Pattern)
                    _lhsOdefinedInsts :: ([Identifier])
                    _lhsOstpos :: Pos
                    _lhsOcopy :: Pattern
                    _patIcopy :: Pattern
                    _patIdefinedAttrs :: ([AttrName])
                    _patIdefinedInsts :: ([Identifier])
                    _patIpatunder :: ([AttrName]->Pattern)
                    _patIstpos :: Pos
                    -- "src-ag/Transform.ag"(line 1163, column 11)
                    _lhsOdefinedAttrs =
                        ({-# LINE 1163 "src-ag/Transform.ag" #-}
                         (field_, attr_) : _patIdefinedAttrs
                         {-# LINE 6255 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 1164, column 11)
                    _lhsOpatunder =
                        ({-# LINE 1164 "src-ag/Transform.ag" #-}
                         \us -> if ((field_,attr_) `elem` us) then Underscore noPos else _copy
                         {-# LINE 6261 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 1165, column 11)
                    _lhsOdefinedInsts =
                        ({-# LINE 1165 "src-ag/Transform.ag" #-}
                         (if field_ == _INST then [attr_] else []) ++ _patIdefinedInsts
                         {-# LINE 6267 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 1180, column 16)
                    _lhsOstpos =
                        ({-# LINE 1180 "src-ag/Transform.ag" #-}
                         getPos field_
                         {-# LINE 6273 "dist/build/Transform" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         Alias field_ attr_ _patIcopy
                         {-# LINE 6279 "dist/build/Transform" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 6285 "dist/build/Transform" #-}
                         )
                    ( _patIcopy,_patIdefinedAttrs,_patIdefinedInsts,_patIpatunder,_patIstpos) =
                        pat_
                    ___node =
                        (Syn_Pattern _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder _lhsOstpos)
                in  ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder,_lhsOstpos)))
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns ->
                      T_Pattern
sem_Pattern_Constr name_ (T_Patterns pats_) =
    (T_Pattern (let _lhsOpatunder :: ([AttrName]->Pattern)
                    _lhsOstpos :: Pos
                    _lhsOdefinedAttrs :: ([AttrName])
                    _lhsOdefinedInsts :: ([Identifier])
                    _lhsOcopy :: Pattern
                    _patsIcopy :: Patterns
                    _patsIdefinedAttrs :: ([AttrName])
                    _patsIdefinedInsts :: ([Identifier])
                    _patsIpatunder :: ([AttrName]->Patterns)
                    -- "src-ag/Transform.ag"(line 1167, column 12)
                    _lhsOpatunder =
                        ({-# LINE 1167 "src-ag/Transform.ag" #-}
                         \us -> Constr name_ (_patsIpatunder us)
                         {-# LINE 6309 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 1178, column 16)
                    _lhsOstpos =
                        ({-# LINE 1178 "src-ag/Transform.ag" #-}
                         getPos name_
                         {-# LINE 6315 "dist/build/Transform" #-}
                         )
                    -- use rule "src-ag/Transform.ag"(line 1158, column 42)
                    _lhsOdefinedAttrs =
                        ({-# LINE 1158 "src-ag/Transform.ag" #-}
                         _patsIdefinedAttrs
                         {-# LINE 6321 "dist/build/Transform" #-}
                         )
                    -- use rule "src-ag/Transform.ag"(line 1157, column 55)
                    _lhsOdefinedInsts =
                        ({-# LINE 1157 "src-ag/Transform.ag" #-}
                         _patsIdefinedInsts
                         {-# LINE 6327 "dist/build/Transform" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         Constr name_ _patsIcopy
                         {-# LINE 6333 "dist/build/Transform" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 6339 "dist/build/Transform" #-}
                         )
                    ( _patsIcopy,_patsIdefinedAttrs,_patsIdefinedInsts,_patsIpatunder) =
                        pats_
                    ___node =
                        (Syn_Pattern _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder _lhsOstpos)
                in  ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder,_lhsOstpos)))
sem_Pattern_Irrefutable :: T_Pattern ->
                           T_Pattern
sem_Pattern_Irrefutable (T_Pattern pat_) =
    (T_Pattern (let _lhsOpatunder :: ([AttrName]->Pattern)
                    _lhsOdefinedAttrs :: ([AttrName])
                    _lhsOdefinedInsts :: ([Identifier])
                    _lhsOcopy :: Pattern
                    _lhsOstpos :: Pos
                    _patIcopy :: Pattern
                    _patIdefinedAttrs :: ([AttrName])
                    _patIdefinedInsts :: ([Identifier])
                    _patIpatunder :: ([AttrName]->Pattern)
                    _patIstpos :: Pos
                    -- "src-ag/Transform.ag"(line 1169, column 17)
                    _lhsOpatunder =
                        ({-# LINE 1169 "src-ag/Transform.ag" #-}
                         \us -> Irrefutable (_patIpatunder us)
                         {-# LINE 6363 "dist/build/Transform" #-}
                         )
                    -- use rule "src-ag/Transform.ag"(line 1158, column 42)
                    _lhsOdefinedAttrs =
                        ({-# LINE 1158 "src-ag/Transform.ag" #-}
                         _patIdefinedAttrs
                         {-# LINE 6369 "dist/build/Transform" #-}
                         )
                    -- use rule "src-ag/Transform.ag"(line 1157, column 55)
                    _lhsOdefinedInsts =
                        ({-# LINE 1157 "src-ag/Transform.ag" #-}
                         _patIdefinedInsts
                         {-# LINE 6375 "dist/build/Transform" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         Irrefutable _patIcopy
                         {-# LINE 6381 "dist/build/Transform" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 6387 "dist/build/Transform" #-}
                         )
                    -- copy rule (up)
                    _lhsOstpos =
                        ({-# LINE 1175 "src-ag/Transform.ag" #-}
                         _patIstpos
                         {-# LINE 6393 "dist/build/Transform" #-}
                         )
                    ( _patIcopy,_patIdefinedAttrs,_patIdefinedInsts,_patIpatunder,_patIstpos) =
                        pat_
                    ___node =
                        (Syn_Pattern _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder _lhsOstpos)
                in  ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder,_lhsOstpos)))
sem_Pattern_Product :: Pos ->
                       T_Patterns ->
                       T_Pattern
sem_Pattern_Product pos_ (T_Patterns pats_) =
    (T_Pattern (let _lhsOpatunder :: ([AttrName]->Pattern)
                    _lhsOstpos :: Pos
                    _lhsOdefinedAttrs :: ([AttrName])
                    _lhsOdefinedInsts :: ([Identifier])
                    _lhsOcopy :: Pattern
                    _patsIcopy :: Patterns
                    _patsIdefinedAttrs :: ([AttrName])
                    _patsIdefinedInsts :: ([Identifier])
                    _patsIpatunder :: ([AttrName]->Patterns)
                    -- "src-ag/Transform.ag"(line 1168, column 13)
                    _lhsOpatunder =
                        ({-# LINE 1168 "src-ag/Transform.ag" #-}
                         \us -> Product pos_ (_patsIpatunder us)
                         {-# LINE 6417 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 1179, column 16)
                    _lhsOstpos =
                        ({-# LINE 1179 "src-ag/Transform.ag" #-}
                         pos_
                         {-# LINE 6423 "dist/build/Transform" #-}
                         )
                    -- use rule "src-ag/Transform.ag"(line 1158, column 42)
                    _lhsOdefinedAttrs =
                        ({-# LINE 1158 "src-ag/Transform.ag" #-}
                         _patsIdefinedAttrs
                         {-# LINE 6429 "dist/build/Transform" #-}
                         )
                    -- use rule "src-ag/Transform.ag"(line 1157, column 55)
                    _lhsOdefinedInsts =
                        ({-# LINE 1157 "src-ag/Transform.ag" #-}
                         _patsIdefinedInsts
                         {-# LINE 6435 "dist/build/Transform" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         Product pos_ _patsIcopy
                         {-# LINE 6441 "dist/build/Transform" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 6447 "dist/build/Transform" #-}
                         )
                    ( _patsIcopy,_patsIdefinedAttrs,_patsIdefinedInsts,_patsIpatunder) =
                        pats_
                    ___node =
                        (Syn_Pattern _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder _lhsOstpos)
                in  ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder,_lhsOstpos)))
sem_Pattern_Underscore :: Pos ->
                          T_Pattern
sem_Pattern_Underscore pos_ =
    (T_Pattern (let _lhsOpatunder :: ([AttrName]->Pattern)
                    _lhsOstpos :: Pos
                    _lhsOdefinedAttrs :: ([AttrName])
                    _lhsOdefinedInsts :: ([Identifier])
                    _lhsOcopy :: Pattern
                    -- "src-ag/Transform.ag"(line 1166, column 16)
                    _lhsOpatunder =
                        ({-# LINE 1166 "src-ag/Transform.ag" #-}
                         \us -> _copy
                         {-# LINE 6466 "dist/build/Transform" #-}
                         )
                    -- "src-ag/Transform.ag"(line 1181, column 16)
                    _lhsOstpos =
                        ({-# LINE 1181 "src-ag/Transform.ag" #-}
                         pos_
                         {-# LINE 6472 "dist/build/Transform" #-}
                         )
                    -- use rule "src-ag/Transform.ag"(line 1158, column 42)
                    _lhsOdefinedAttrs =
                        ({-# LINE 1158 "src-ag/Transform.ag" #-}
                         []
                         {-# LINE 6478 "dist/build/Transform" #-}
                         )
                    -- use rule "src-ag/Transform.ag"(line 1157, column 55)
                    _lhsOdefinedInsts =
                        ({-# LINE 1157 "src-ag/Transform.ag" #-}
                         []
                         {-# LINE 6484 "dist/build/Transform" #-}
                         )
                    -- self rule
                    _copy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         Underscore pos_
                         {-# LINE 6490 "dist/build/Transform" #-}
                         )
                    -- self rule
                    _lhsOcopy =
                        ({-# LINE 22 "src-ag/Patterns.ag" #-}
                         _copy
                         {-# LINE 6496 "dist/build/Transform" #-}
                         )
                    ___node =
                        (Syn_Pattern _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder _lhsOstpos)
                in  ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder,_lhsOstpos)))
-- Patterns ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : Patterns 
         definedAttrs         : [AttrName]
         definedInsts         : [Identifier]
         patunder             : [AttrName]->Patterns
   alternatives:
      alternative Cons:
         child hd             : Pattern 
         child tl             : Patterns 
         visit 0:
            local copy        : _
      alternative Nil:
         visit 0:
            local copy        : _
-}
-- cata
sem_Patterns :: Patterns ->
                T_Patterns
sem_Patterns list =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list))
-- semantic domain
newtype T_Patterns = T_Patterns (( Patterns,([AttrName]),([Identifier]),([AttrName]->Patterns)))
data Inh_Patterns = Inh_Patterns {}
data Syn_Patterns = Syn_Patterns {copy_Syn_Patterns :: !(Patterns),definedAttrs_Syn_Patterns :: !(([AttrName])),definedInsts_Syn_Patterns :: !(([Identifier])),patunder_Syn_Patterns :: !(([AttrName]->Patterns))}
wrap_Patterns :: T_Patterns ->
                 Inh_Patterns ->
                 Syn_Patterns
wrap_Patterns (T_Patterns sem) (Inh_Patterns) =
    (let ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder) = sem
     in  (Syn_Patterns _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder))
sem_Patterns_Cons :: T_Pattern ->
                     T_Patterns ->
                     T_Patterns
sem_Patterns_Cons (T_Pattern hd_) (T_Patterns tl_) =
    (T_Patterns (let _lhsOpatunder :: ([AttrName]->Patterns)
                     _lhsOdefinedAttrs :: ([AttrName])
                     _lhsOdefinedInsts :: ([Identifier])
                     _lhsOcopy :: Patterns
                     _hdIcopy :: Pattern
                     _hdIdefinedAttrs :: ([AttrName])
                     _hdIdefinedInsts :: ([Identifier])
                     _hdIpatunder :: ([AttrName]->Pattern)
                     _hdIstpos :: Pos
                     _tlIcopy :: Patterns
                     _tlIdefinedAttrs :: ([AttrName])
                     _tlIdefinedInsts :: ([Identifier])
                     _tlIpatunder :: ([AttrName]->Patterns)
                     -- "src-ag/Transform.ag"(line 1173, column 10)
                     _lhsOpatunder =
                         ({-# LINE 1173 "src-ag/Transform.ag" #-}
                          \us -> (_hdIpatunder us) : (_tlIpatunder us)
                          {-# LINE 6555 "dist/build/Transform" #-}
                          )
                     -- use rule "src-ag/Transform.ag"(line 1158, column 42)
                     _lhsOdefinedAttrs =
                         ({-# LINE 1158 "src-ag/Transform.ag" #-}
                          _hdIdefinedAttrs ++ _tlIdefinedAttrs
                          {-# LINE 6561 "dist/build/Transform" #-}
                          )
                     -- use rule "src-ag/Transform.ag"(line 1157, column 55)
                     _lhsOdefinedInsts =
                         ({-# LINE 1157 "src-ag/Transform.ag" #-}
                          _hdIdefinedInsts ++ _tlIdefinedInsts
                          {-# LINE 6567 "dist/build/Transform" #-}
                          )
                     -- self rule
                     _copy =
                         ({-# LINE 22 "src-ag/Patterns.ag" #-}
                          (:) _hdIcopy _tlIcopy
                          {-# LINE 6573 "dist/build/Transform" #-}
                          )
                     -- self rule
                     _lhsOcopy =
                         ({-# LINE 22 "src-ag/Patterns.ag" #-}
                          _copy
                          {-# LINE 6579 "dist/build/Transform" #-}
                          )
                     ( _hdIcopy,_hdIdefinedAttrs,_hdIdefinedInsts,_hdIpatunder,_hdIstpos) =
                         hd_
                     ( _tlIcopy,_tlIdefinedAttrs,_tlIdefinedInsts,_tlIpatunder) =
                         tl_
                     ___node =
                         (Syn_Patterns _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder)
                 in  ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder)))
sem_Patterns_Nil :: T_Patterns
sem_Patterns_Nil =
    (T_Patterns (let _lhsOpatunder :: ([AttrName]->Patterns)
                     _lhsOdefinedAttrs :: ([AttrName])
                     _lhsOdefinedInsts :: ([Identifier])
                     _lhsOcopy :: Patterns
                     -- "src-ag/Transform.ag"(line 1172, column 9)
                     _lhsOpatunder =
                         ({-# LINE 1172 "src-ag/Transform.ag" #-}
                          \us ->  []
                          {-# LINE 6598 "dist/build/Transform" #-}
                          )
                     -- use rule "src-ag/Transform.ag"(line 1158, column 42)
                     _lhsOdefinedAttrs =
                         ({-# LINE 1158 "src-ag/Transform.ag" #-}
                          []
                          {-# LINE 6604 "dist/build/Transform" #-}
                          )
                     -- use rule "src-ag/Transform.ag"(line 1157, column 55)
                     _lhsOdefinedInsts =
                         ({-# LINE 1157 "src-ag/Transform.ag" #-}
                          []
                          {-# LINE 6610 "dist/build/Transform" #-}
                          )
                     -- self rule
                     _copy =
                         ({-# LINE 22 "src-ag/Patterns.ag" #-}
                          []
                          {-# LINE 6616 "dist/build/Transform" #-}
                          )
                     -- self rule
                     _lhsOcopy =
                         ({-# LINE 22 "src-ag/Patterns.ag" #-}
                          _copy
                          {-# LINE 6622 "dist/build/Transform" #-}
                          )
                     ___node =
                         (Syn_Patterns _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder)
                 in  ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder)))
-- SemAlt ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allAttrDecls         : Map NontermIdent (Attributes, Attributes)
         allAttrs             : Map NontermIdent (Attributes, Attributes)
         allFields            : DataTypes
         nts                  : Set NontermIdent
         options              : Options
      synthesized attributes:
         attrOrderCollect     : AttrOrderMap
         collectedArounds     : [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         collectedAugments    : [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         collectedInsts       : [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         collectedMerges      : [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         collectedRules       : [ (NontermIdent, ConstructorIdent, RuleInfo)]
         collectedSigs        : [ (NontermIdent, ConstructorIdent, SigInfo) ]
         collectedUniques     : [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         errors               : Seq Error
         semPragmasCollect    : PragmaMap
   alternatives:
      alternative SemAlt:
         child pos            : {Pos}
         child constructorSet : ConstructorSet 
         child rules          : SemDefs 
         visit 0:
            local pragmaNames : _
            local attrOrders  : _
            local coninfo     : _
-}
-- cata
sem_SemAlt :: SemAlt ->
              T_SemAlt
sem_SemAlt (SemAlt _pos _constructorSet _rules) =
    (sem_SemAlt_SemAlt _pos (sem_ConstructorSet _constructorSet) (sem_SemDefs _rules))
-- semantic domain
newtype T_SemAlt = T_SemAlt ((Map NontermIdent (Attributes, Attributes)) ->
                             (Map NontermIdent (Attributes, Attributes)) ->
                             DataTypes ->
                             (Set NontermIdent) ->
                             Options ->
                             ( AttrOrderMap,([ (NontermIdent, ConstructorIdent, [AroundInfo])  ]),([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]),([ (NontermIdent, ConstructorIdent, [Identifier]) ]),([ (NontermIdent, ConstructorIdent, [MergeInfo])   ]),([ (NontermIdent, ConstructorIdent, RuleInfo)]),([ (NontermIdent, ConstructorIdent, SigInfo) ]),([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]),(Seq Error),PragmaMap))
data Inh_SemAlt = Inh_SemAlt {allAttrDecls_Inh_SemAlt :: !((Map NontermIdent (Attributes, Attributes))),allAttrs_Inh_SemAlt :: !((Map NontermIdent (Attributes, Attributes))),allFields_Inh_SemAlt :: !(DataTypes),nts_Inh_SemAlt :: !((Set NontermIdent)),options_Inh_SemAlt :: !(Options)}
data Syn_SemAlt = Syn_SemAlt {attrOrderCollect_Syn_SemAlt :: !(AttrOrderMap),collectedArounds_Syn_SemAlt :: !(([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])),collectedAugments_Syn_SemAlt :: !(([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])),collectedInsts_Syn_SemAlt :: !(([ (NontermIdent, ConstructorIdent, [Identifier]) ])),collectedMerges_Syn_SemAlt :: !(([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])),collectedRules_Syn_SemAlt :: !(([ (NontermIdent, ConstructorIdent, RuleInfo)])),collectedSigs_Syn_SemAlt :: !(([ (NontermIdent, ConstructorIdent, SigInfo) ])),collectedUniques_Syn_SemAlt :: !(([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])),errors_Syn_SemAlt :: !((Seq Error)),semPragmasCollect_Syn_SemAlt :: !(PragmaMap)}
wrap_SemAlt :: T_SemAlt ->
               Inh_SemAlt ->
               Syn_SemAlt
wrap_SemAlt (T_SemAlt sem) (Inh_SemAlt _lhsIallAttrDecls _lhsIallAttrs _lhsIallFields _lhsInts _lhsIoptions) =
    (let ( _lhsOattrOrderCollect,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedRules,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOerrors,_lhsOsemPragmasCollect) = sem _lhsIallAttrDecls _lhsIallAttrs _lhsIallFields _lhsInts _lhsIoptions
     in  (Syn_SemAlt _lhsOattrOrderCollect _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedInsts _lhsOcollectedMerges _lhsOcollectedRules _lhsOcollectedSigs _lhsOcollectedUniques _lhsOerrors _lhsOsemPragmasCollect))
sem_SemAlt_SemAlt :: Pos ->
                     T_ConstructorSet ->
                     T_SemDefs ->
                     T_SemAlt
sem_SemAlt_SemAlt pos_ (T_ConstructorSet constructorSet_) (T_SemDefs rules_) =
    (T_SemAlt (\ _lhsIallAttrDecls
                 _lhsIallAttrs
                 _lhsIallFields
                 _lhsInts
                 _lhsIoptions ->
                   (let _lhsOsemPragmasCollect :: PragmaMap
                        _lhsOattrOrderCollect :: AttrOrderMap
                        _lhsOerrors :: (Seq Error)
                        _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                        _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                        _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                        _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                        _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                        _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                        _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                        _rulesOoptions :: Options
                        _constructorSetIcollectedConstructorNames :: (Set ConstructorIdent)
                        _constructorSetIconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                        _constructorSetIerrors :: (Seq Error)
                        _rulesIaroundInfos :: ([AroundInfo])
                        _rulesIaugmentInfos :: ([AugmentInfo])
                        _rulesIdefinedInsts :: ([Identifier])
                        _rulesIerrors :: (Seq Error)
                        _rulesImergeInfos :: ([MergeInfo])
                        _rulesIorderDepsCollect :: (Set Dependency)
                        _rulesIpragmaNamesCollect :: ([Identifier])
                        _rulesIruleInfos :: ([RuleInfo])
                        _rulesIsigInfos :: ([SigInfo])
                        _rulesIuniqueInfos :: ([UniqueInfo])
                        -- "src-ag/Transform.ag"(line 869, column 7)
                        _pragmaNames =
                            ({-# LINE 869 "src-ag/Transform.ag" #-}
                             Set.fromList _rulesIpragmaNamesCollect
                             {-# LINE 6715 "dist/build/Transform" #-}
                             )
                        -- "src-ag/Transform.ag"(line 870, column 7)
                        _lhsOsemPragmasCollect =
                            ({-# LINE 870 "src-ag/Transform.ag" #-}
                             foldr pragmaMapUnion Map.empty [ pragmaMapSingle nt con _pragmaNames
                                                            | (nt, conset, _) <- _coninfo
                                                            , con <- Set.toList conset
                                                            ]
                             {-# LINE 6724 "dist/build/Transform" #-}
                             )
                        -- "src-ag/Transform.ag"(line 898, column 7)
                        _attrOrders =
                            ({-# LINE 898 "src-ag/Transform.ag" #-}
                             [ orderMapSingle nt con _rulesIorderDepsCollect
                             | (nt, conset, _) <- _coninfo
                             , con <- Set.toList conset
                             ]
                             {-# LINE 6733 "dist/build/Transform" #-}
                             )
                        -- "src-ag/Transform.ag"(line 904, column 7)
                        _lhsOattrOrderCollect =
                            ({-# LINE 904 "src-ag/Transform.ag" #-}
                             foldr orderMapUnion Map.empty _attrOrders
                             {-# LINE 6739 "dist/build/Transform" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1082, column 12)
                        _coninfo =
                            ({-# LINE 1082 "src-ag/Transform.ag" #-}
                             [ (nt, conset, conkeys)
                             | nt  <- Set.toList _lhsInts
                             , let conmap = Map.findWithDefault Map.empty nt _lhsIallFields
                             , let conkeys = Set.fromList (Map.keys conmap)
                             , let conset  = _constructorSetIconstructors conkeys
                             ]
                             {-# LINE 6750 "dist/build/Transform" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1089, column 12)
                        _lhsOerrors =
                            ({-# LINE 1089 "src-ag/Transform.ag" #-}
                             Seq.fromList
                                [ UndefAlt nt con
                                | (nt, conset, conkeys) <- _coninfo
                                , con <- Set.toList (Set.difference conset conkeys)
                                ]
                             Seq.>< _rulesIerrors
                             {-# LINE 6761 "dist/build/Transform" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1095, column 12)
                        _lhsOcollectedRules =
                            ({-# LINE 1095 "src-ag/Transform.ag" #-}
                             [ (nt,con,r)
                             | (nt, conset, _) <- _coninfo
                             , con <- Set.toList conset
                             , r <- _rulesIruleInfos
                             ]
                             {-# LINE 6771 "dist/build/Transform" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1101, column 12)
                        _lhsOcollectedSigs =
                            ({-# LINE 1101 "src-ag/Transform.ag" #-}
                             [ (nt,con,ts)
                             | (nt, conset, _) <- _coninfo
                             , con <- Set.toList conset
                             , ts <- _rulesIsigInfos
                             ]
                             {-# LINE 6781 "dist/build/Transform" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1108, column 12)
                        _lhsOcollectedInsts =
                            ({-# LINE 1108 "src-ag/Transform.ag" #-}
                             [ (nt,con,_rulesIdefinedInsts)
                             | (nt, conset, _) <- _coninfo
                             , con <- Set.toList conset
                             ]
                             {-# LINE 6790 "dist/build/Transform" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1114, column 12)
                        _lhsOcollectedUniques =
                            ({-# LINE 1114 "src-ag/Transform.ag" #-}
                             [ (nt,con,_rulesIuniqueInfos)
                             | (nt, conset, _) <- _coninfo
                             , con <- Set.toList conset
                             ]
                             {-# LINE 6799 "dist/build/Transform" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1120, column 12)
                        _lhsOcollectedAugments =
                            ({-# LINE 1120 "src-ag/Transform.ag" #-}
                             [ (nt, con, _rulesIaugmentInfos)
                             | (nt, conset, _) <- _coninfo
                             , con <- Set.toList conset
                             ]
                             {-# LINE 6808 "dist/build/Transform" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1126, column 12)
                        _lhsOcollectedArounds =
                            ({-# LINE 1126 "src-ag/Transform.ag" #-}
                             [ (nt, con, _rulesIaroundInfos)
                             | (nt, conset, _) <- _coninfo
                             , con <- Set.toList conset
                             ]
                             {-# LINE 6817 "dist/build/Transform" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1132, column 12)
                        _lhsOcollectedMerges =
                            ({-# LINE 1132 "src-ag/Transform.ag" #-}
                             [ (nt, con, _rulesImergeInfos)
                             | (nt, conset, _) <- _coninfo
                             , con <- Set.toList conset
                             ]
                             {-# LINE 6826 "dist/build/Transform" #-}
                             )
                        -- copy rule (down)
                        _rulesOoptions =
                            ({-# LINE 40 "src-ag/Transform.ag" #-}
                             _lhsIoptions
                             {-# LINE 6832 "dist/build/Transform" #-}
                             )
                        ( _constructorSetIcollectedConstructorNames,_constructorSetIconstructors,_constructorSetIerrors) =
                            constructorSet_
                        ( _rulesIaroundInfos,_rulesIaugmentInfos,_rulesIdefinedInsts,_rulesIerrors,_rulesImergeInfos,_rulesIorderDepsCollect,_rulesIpragmaNamesCollect,_rulesIruleInfos,_rulesIsigInfos,_rulesIuniqueInfos) =
                            rules_ _rulesOoptions
                        ___node =
                            (Syn_SemAlt _lhsOattrOrderCollect _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedInsts _lhsOcollectedMerges _lhsOcollectedRules _lhsOcollectedSigs _lhsOcollectedUniques _lhsOerrors _lhsOsemPragmasCollect)
                    in  ( _lhsOattrOrderCollect,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedRules,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOerrors,_lhsOsemPragmasCollect))))
-- SemAlts -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allAttrDecls         : Map NontermIdent (Attributes, Attributes)
         allAttrs             : Map NontermIdent (Attributes, Attributes)
         allFields            : DataTypes
         nts                  : Set NontermIdent
         options              : Options
      synthesized attributes:
         attrOrderCollect     : AttrOrderMap
         collectedArounds     : [ (NontermIdent, ConstructorIdent, [AroundInfo])  ]
         collectedAugments    : [ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]
         collectedInsts       : [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         collectedMerges      : [ (NontermIdent, ConstructorIdent, [MergeInfo])   ]
         collectedRules       : [ (NontermIdent, ConstructorIdent, RuleInfo)]
         collectedSigs        : [ (NontermIdent, ConstructorIdent, SigInfo) ]
         collectedUniques     : [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         errors               : Seq Error
         semPragmasCollect    : PragmaMap
   alternatives:
      alternative Cons:
         child hd             : SemAlt 
         child tl             : SemAlts 
      alternative Nil:
-}
-- cata
sem_SemAlts :: SemAlts ->
               T_SemAlts
sem_SemAlts list =
    (Prelude.foldr sem_SemAlts_Cons sem_SemAlts_Nil (Prelude.map sem_SemAlt list))
-- semantic domain
newtype T_SemAlts = T_SemAlts ((Map NontermIdent (Attributes, Attributes)) ->
                               (Map NontermIdent (Attributes, Attributes)) ->
                               DataTypes ->
                               (Set NontermIdent) ->
                               Options ->
                               ( AttrOrderMap,([ (NontermIdent, ConstructorIdent, [AroundInfo])  ]),([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ]),([ (NontermIdent, ConstructorIdent, [Identifier]) ]),([ (NontermIdent, ConstructorIdent, [MergeInfo])   ]),([ (NontermIdent, ConstructorIdent, RuleInfo)]),([ (NontermIdent, ConstructorIdent, SigInfo) ]),([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]),(Seq Error),PragmaMap))
data Inh_SemAlts = Inh_SemAlts {allAttrDecls_Inh_SemAlts :: !((Map NontermIdent (Attributes, Attributes))),allAttrs_Inh_SemAlts :: !((Map NontermIdent (Attributes, Attributes))),allFields_Inh_SemAlts :: !(DataTypes),nts_Inh_SemAlts :: !((Set NontermIdent)),options_Inh_SemAlts :: !(Options)}
data Syn_SemAlts = Syn_SemAlts {attrOrderCollect_Syn_SemAlts :: !(AttrOrderMap),collectedArounds_Syn_SemAlts :: !(([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])),collectedAugments_Syn_SemAlts :: !(([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])),collectedInsts_Syn_SemAlts :: !(([ (NontermIdent, ConstructorIdent, [Identifier]) ])),collectedMerges_Syn_SemAlts :: !(([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])),collectedRules_Syn_SemAlts :: !(([ (NontermIdent, ConstructorIdent, RuleInfo)])),collectedSigs_Syn_SemAlts :: !(([ (NontermIdent, ConstructorIdent, SigInfo) ])),collectedUniques_Syn_SemAlts :: !(([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])),errors_Syn_SemAlts :: !((Seq Error)),semPragmasCollect_Syn_SemAlts :: !(PragmaMap)}
wrap_SemAlts :: T_SemAlts ->
                Inh_SemAlts ->
                Syn_SemAlts
wrap_SemAlts (T_SemAlts sem) (Inh_SemAlts _lhsIallAttrDecls _lhsIallAttrs _lhsIallFields _lhsInts _lhsIoptions) =
    (let ( _lhsOattrOrderCollect,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedRules,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOerrors,_lhsOsemPragmasCollect) = sem _lhsIallAttrDecls _lhsIallAttrs _lhsIallFields _lhsInts _lhsIoptions
     in  (Syn_SemAlts _lhsOattrOrderCollect _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedInsts _lhsOcollectedMerges _lhsOcollectedRules _lhsOcollectedSigs _lhsOcollectedUniques _lhsOerrors _lhsOsemPragmasCollect))
sem_SemAlts_Cons :: T_SemAlt ->
                    T_SemAlts ->
                    T_SemAlts
sem_SemAlts_Cons (T_SemAlt hd_) (T_SemAlts tl_) =
    (T_SemAlts (\ _lhsIallAttrDecls
                  _lhsIallAttrs
                  _lhsIallFields
                  _lhsInts
                  _lhsIoptions ->
                    (let _lhsOattrOrderCollect :: AttrOrderMap
                         _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                         _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                         _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                         _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                         _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                         _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                         _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                         _lhsOerrors :: (Seq Error)
                         _lhsOsemPragmasCollect :: PragmaMap
                         _hdOallAttrDecls :: (Map NontermIdent (Attributes, Attributes))
                         _hdOallAttrs :: (Map NontermIdent (Attributes, Attributes))
                         _hdOallFields :: DataTypes
                         _hdOnts :: (Set NontermIdent)
                         _hdOoptions :: Options
                         _tlOallAttrDecls :: (Map NontermIdent (Attributes, Attributes))
                         _tlOallAttrs :: (Map NontermIdent (Attributes, Attributes))
                         _tlOallFields :: DataTypes
                         _tlOnts :: (Set NontermIdent)
                         _tlOoptions :: Options
                         _hdIattrOrderCollect :: AttrOrderMap
                         _hdIcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                         _hdIcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                         _hdIcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                         _hdIcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                         _hdIcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                         _hdIcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                         _hdIcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                         _hdIerrors :: (Seq Error)
                         _hdIsemPragmasCollect :: PragmaMap
                         _tlIattrOrderCollect :: AttrOrderMap
                         _tlIcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                         _tlIcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                         _tlIcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                         _tlIcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                         _tlIcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                         _tlIcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                         _tlIcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                         _tlIerrors :: (Seq Error)
                         _tlIsemPragmasCollect :: PragmaMap
                         -- use rule "src-ag/Transform.ag"(line 893, column 55)
                         _lhsOattrOrderCollect =
                             ({-# LINE 893 "src-ag/Transform.ag" #-}
                              _hdIattrOrderCollect `orderMapUnion` _tlIattrOrderCollect
                              {-# LINE 6940 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 164, column 32)
                         _lhsOcollectedArounds =
                             ({-# LINE 164 "src-ag/Transform.ag" #-}
                              _hdIcollectedArounds ++ _tlIcollectedArounds
                              {-# LINE 6946 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 163, column 32)
                         _lhsOcollectedAugments =
                             ({-# LINE 163 "src-ag/Transform.ag" #-}
                              _hdIcollectedAugments ++ _tlIcollectedAugments
                              {-# LINE 6952 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 161, column 32)
                         _lhsOcollectedInsts =
                             ({-# LINE 161 "src-ag/Transform.ag" #-}
                              _hdIcollectedInsts ++ _tlIcollectedInsts
                              {-# LINE 6958 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 165, column 32)
                         _lhsOcollectedMerges =
                             ({-# LINE 165 "src-ag/Transform.ag" #-}
                              _hdIcollectedMerges ++ _tlIcollectedMerges
                              {-# LINE 6964 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 159, column 32)
                         _lhsOcollectedRules =
                             ({-# LINE 159 "src-ag/Transform.ag" #-}
                              _hdIcollectedRules ++ _tlIcollectedRules
                              {-# LINE 6970 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 160, column 32)
                         _lhsOcollectedSigs =
                             ({-# LINE 160 "src-ag/Transform.ag" #-}
                              _hdIcollectedSigs ++ _tlIcollectedSigs
                              {-# LINE 6976 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 162, column 32)
                         _lhsOcollectedUniques =
                             ({-# LINE 162 "src-ag/Transform.ag" #-}
                              _hdIcollectedUniques ++ _tlIcollectedUniques
                              {-# LINE 6982 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 44, column 19)
                         _lhsOerrors =
                             ({-# LINE 44 "src-ag/Transform.ag" #-}
                              _hdIerrors Seq.>< _tlIerrors
                              {-# LINE 6988 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 865, column 56)
                         _lhsOsemPragmasCollect =
                             ({-# LINE 865 "src-ag/Transform.ag" #-}
                              _hdIsemPragmasCollect `pragmaMapUnion` _tlIsemPragmasCollect
                              {-# LINE 6994 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _hdOallAttrDecls =
                             ({-# LINE 894 "src-ag/Transform.ag" #-}
                              _lhsIallAttrDecls
                              {-# LINE 7000 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _hdOallAttrs =
                             ({-# LINE 1306 "src-ag/Transform.ag" #-}
                              _lhsIallAttrs
                              {-# LINE 7006 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _hdOallFields =
                             ({-# LINE 136 "src-ag/Transform.ag" #-}
                              _lhsIallFields
                              {-# LINE 7012 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _hdOnts =
                             ({-# LINE 175 "src-ag/Transform.ag" #-}
                              _lhsInts
                              {-# LINE 7018 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _hdOoptions =
                             ({-# LINE 40 "src-ag/Transform.ag" #-}
                              _lhsIoptions
                              {-# LINE 7024 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _tlOallAttrDecls =
                             ({-# LINE 894 "src-ag/Transform.ag" #-}
                              _lhsIallAttrDecls
                              {-# LINE 7030 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _tlOallAttrs =
                             ({-# LINE 1306 "src-ag/Transform.ag" #-}
                              _lhsIallAttrs
                              {-# LINE 7036 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _tlOallFields =
                             ({-# LINE 136 "src-ag/Transform.ag" #-}
                              _lhsIallFields
                              {-# LINE 7042 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _tlOnts =
                             ({-# LINE 175 "src-ag/Transform.ag" #-}
                              _lhsInts
                              {-# LINE 7048 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _tlOoptions =
                             ({-# LINE 40 "src-ag/Transform.ag" #-}
                              _lhsIoptions
                              {-# LINE 7054 "dist/build/Transform" #-}
                              )
                         ( _hdIattrOrderCollect,_hdIcollectedArounds,_hdIcollectedAugments,_hdIcollectedInsts,_hdIcollectedMerges,_hdIcollectedRules,_hdIcollectedSigs,_hdIcollectedUniques,_hdIerrors,_hdIsemPragmasCollect) =
                             hd_ _hdOallAttrDecls _hdOallAttrs _hdOallFields _hdOnts _hdOoptions
                         ( _tlIattrOrderCollect,_tlIcollectedArounds,_tlIcollectedAugments,_tlIcollectedInsts,_tlIcollectedMerges,_tlIcollectedRules,_tlIcollectedSigs,_tlIcollectedUniques,_tlIerrors,_tlIsemPragmasCollect) =
                             tl_ _tlOallAttrDecls _tlOallAttrs _tlOallFields _tlOnts _tlOoptions
                         ___node =
                             (Syn_SemAlts _lhsOattrOrderCollect _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedInsts _lhsOcollectedMerges _lhsOcollectedRules _lhsOcollectedSigs _lhsOcollectedUniques _lhsOerrors _lhsOsemPragmasCollect)
                     in  ( _lhsOattrOrderCollect,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedRules,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOerrors,_lhsOsemPragmasCollect))))
sem_SemAlts_Nil :: T_SemAlts
sem_SemAlts_Nil =
    (T_SemAlts (\ _lhsIallAttrDecls
                  _lhsIallAttrs
                  _lhsIallFields
                  _lhsInts
                  _lhsIoptions ->
                    (let _lhsOattrOrderCollect :: AttrOrderMap
                         _lhsOcollectedArounds :: ([ (NontermIdent, ConstructorIdent, [AroundInfo])  ])
                         _lhsOcollectedAugments :: ([ (NontermIdent, ConstructorIdent, [AugmentInfo]) ])
                         _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                         _lhsOcollectedMerges :: ([ (NontermIdent, ConstructorIdent, [MergeInfo])   ])
                         _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                         _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                         _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                         _lhsOerrors :: (Seq Error)
                         _lhsOsemPragmasCollect :: PragmaMap
                         -- use rule "src-ag/Transform.ag"(line 893, column 55)
                         _lhsOattrOrderCollect =
                             ({-# LINE 893 "src-ag/Transform.ag" #-}
                              Map.empty
                              {-# LINE 7084 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 164, column 32)
                         _lhsOcollectedArounds =
                             ({-# LINE 164 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 7090 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 163, column 32)
                         _lhsOcollectedAugments =
                             ({-# LINE 163 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 7096 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 161, column 32)
                         _lhsOcollectedInsts =
                             ({-# LINE 161 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 7102 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 165, column 32)
                         _lhsOcollectedMerges =
                             ({-# LINE 165 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 7108 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 159, column 32)
                         _lhsOcollectedRules =
                             ({-# LINE 159 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 7114 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 160, column 32)
                         _lhsOcollectedSigs =
                             ({-# LINE 160 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 7120 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 162, column 32)
                         _lhsOcollectedUniques =
                             ({-# LINE 162 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 7126 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 44, column 19)
                         _lhsOerrors =
                             ({-# LINE 44 "src-ag/Transform.ag" #-}
                              Seq.empty
                              {-# LINE 7132 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 865, column 56)
                         _lhsOsemPragmasCollect =
                             ({-# LINE 865 "src-ag/Transform.ag" #-}
                              Map.empty
                              {-# LINE 7138 "dist/build/Transform" #-}
                              )
                         ___node =
                             (Syn_SemAlts _lhsOattrOrderCollect _lhsOcollectedArounds _lhsOcollectedAugments _lhsOcollectedInsts _lhsOcollectedMerges _lhsOcollectedRules _lhsOcollectedSigs _lhsOcollectedUniques _lhsOerrors _lhsOsemPragmasCollect)
                     in  ( _lhsOattrOrderCollect,_lhsOcollectedArounds,_lhsOcollectedAugments,_lhsOcollectedInsts,_lhsOcollectedMerges,_lhsOcollectedRules,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOerrors,_lhsOsemPragmasCollect))))
-- SemDef ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attributes:
         aroundInfos          : [AroundInfo]
         augmentInfos         : [AugmentInfo]
         definedInsts         : [Identifier]
         errors               : Seq Error
         mergeInfos           : [MergeInfo]
         orderDepsCollect     : Set Dependency
         pragmaNamesCollect   : [Identifier]
         ruleInfos            : [RuleInfo]
         sigInfos             : [SigInfo]
         uniqueInfos          : [UniqueInfo]
   alternatives:
      alternative AroundDef:
         child ident          : {Identifier}
         child rhs            : {Expression}
      alternative AttrOrderBefore:
         child before         : {[Occurrence]}
         child after          : {[Occurrence]}
         visit 0:
            local dependency  : _
      alternative AugmentDef:
         child ident          : {Identifier}
         child rhs            : {Expression}
      alternative Def:
         child pos            : {Pos}
         child mbName         : {Maybe Identifier}
         child pattern        : Pattern 
         child rhs            : {Expression}
         child owrt           : {Bool}
         child pure           : {Bool}
         child eager          : {Bool}
      alternative MergeDef:
         child target         : {Identifier}
         child nt             : {Identifier}
         child sources        : {[Identifier]}
         child rhs            : {Expression}
      alternative SemPragma:
         child names          : {[NontermIdent]}
      alternative TypeDef:
         child pos            : {Pos}
         child ident          : {Identifier}
         child tp             : {Type}
      alternative UniqueDef:
         child ident          : {Identifier}
         child ref            : {Identifier}
-}
-- cata
sem_SemDef :: SemDef ->
              T_SemDef
sem_SemDef (AroundDef _ident _rhs) =
    (sem_SemDef_AroundDef _ident _rhs)
sem_SemDef (AttrOrderBefore _before _after) =
    (sem_SemDef_AttrOrderBefore _before _after)
sem_SemDef (AugmentDef _ident _rhs) =
    (sem_SemDef_AugmentDef _ident _rhs)
sem_SemDef (Def _pos _mbName _pattern _rhs _owrt _pure _eager) =
    (sem_SemDef_Def _pos _mbName (sem_Pattern _pattern) _rhs _owrt _pure _eager)
sem_SemDef (MergeDef _target _nt _sources _rhs) =
    (sem_SemDef_MergeDef _target _nt _sources _rhs)
sem_SemDef (SemPragma _names) =
    (sem_SemDef_SemPragma _names)
sem_SemDef (TypeDef _pos _ident _tp) =
    (sem_SemDef_TypeDef _pos _ident _tp)
sem_SemDef (UniqueDef _ident _ref) =
    (sem_SemDef_UniqueDef _ident _ref)
-- semantic domain
newtype T_SemDef = T_SemDef (Options ->
                             ( ([AroundInfo]),([AugmentInfo]),([Identifier]),(Seq Error),([MergeInfo]),(Set Dependency),([Identifier]),([RuleInfo]),([SigInfo]),([UniqueInfo])))
data Inh_SemDef = Inh_SemDef {options_Inh_SemDef :: !(Options)}
data Syn_SemDef = Syn_SemDef {aroundInfos_Syn_SemDef :: !(([AroundInfo])),augmentInfos_Syn_SemDef :: !(([AugmentInfo])),definedInsts_Syn_SemDef :: !(([Identifier])),errors_Syn_SemDef :: !((Seq Error)),mergeInfos_Syn_SemDef :: !(([MergeInfo])),orderDepsCollect_Syn_SemDef :: !((Set Dependency)),pragmaNamesCollect_Syn_SemDef :: !(([Identifier])),ruleInfos_Syn_SemDef :: !(([RuleInfo])),sigInfos_Syn_SemDef :: !(([SigInfo])),uniqueInfos_Syn_SemDef :: !(([UniqueInfo]))}
wrap_SemDef :: T_SemDef ->
               Inh_SemDef ->
               Syn_SemDef
wrap_SemDef (T_SemDef sem) (Inh_SemDef _lhsIoptions) =
    (let ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos) = sem _lhsIoptions
     in  (Syn_SemDef _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos))
sem_SemDef_AroundDef :: Identifier ->
                        Expression ->
                        T_SemDef
sem_SemDef_AroundDef ident_ rhs_ =
    (T_SemDef (\ _lhsIoptions ->
                   (let _lhsOaroundInfos :: ([AroundInfo])
                        _lhsOaugmentInfos :: ([AugmentInfo])
                        _lhsOdefinedInsts :: ([Identifier])
                        _lhsOerrors :: (Seq Error)
                        _lhsOmergeInfos :: ([MergeInfo])
                        _lhsOorderDepsCollect :: (Set Dependency)
                        _lhsOpragmaNamesCollect :: ([Identifier])
                        _lhsOruleInfos :: ([RuleInfo])
                        _lhsOsigInfos :: ([SigInfo])
                        _lhsOuniqueInfos :: ([UniqueInfo])
                        -- "src-ag/Transform.ag"(line 1151, column 17)
                        _lhsOaroundInfos =
                            ({-# LINE 1151 "src-ag/Transform.ag" #-}
                             [ (ident_, rhs_) ]
                             {-# LINE 7243 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1075, column 40)
                        _lhsOaugmentInfos =
                            ({-# LINE 1075 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7249 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1157, column 55)
                        _lhsOdefinedInsts =
                            ({-# LINE 1157 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7255 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 44, column 19)
                        _lhsOerrors =
                            ({-# LINE 44 "src-ag/Transform.ag" #-}
                             Seq.empty
                             {-# LINE 7261 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1077, column 40)
                        _lhsOmergeInfos =
                            ({-# LINE 1077 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7267 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 906, column 44)
                        _lhsOorderDepsCollect =
                            ({-# LINE 906 "src-ag/Transform.ag" #-}
                             Set.empty
                             {-# LINE 7273 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 875, column 46)
                        _lhsOpragmaNamesCollect =
                            ({-# LINE 875 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7279 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1072, column 40)
                        _lhsOruleInfos =
                            ({-# LINE 1072 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7285 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1073, column 40)
                        _lhsOsigInfos =
                            ({-# LINE 1073 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7291 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1074, column 40)
                        _lhsOuniqueInfos =
                            ({-# LINE 1074 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7297 "dist/build/Transform" #-}
                             )
                        ___node =
                            (Syn_SemDef _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos)
                    in  ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos))))
sem_SemDef_AttrOrderBefore :: ([Occurrence]) ->
                              ([Occurrence]) ->
                              T_SemDef
sem_SemDef_AttrOrderBefore before_ after_ =
    (T_SemDef (\ _lhsIoptions ->
                   (let _lhsOorderDepsCollect :: (Set Dependency)
                        _lhsOaroundInfos :: ([AroundInfo])
                        _lhsOaugmentInfos :: ([AugmentInfo])
                        _lhsOdefinedInsts :: ([Identifier])
                        _lhsOerrors :: (Seq Error)
                        _lhsOmergeInfos :: ([MergeInfo])
                        _lhsOpragmaNamesCollect :: ([Identifier])
                        _lhsOruleInfos :: ([RuleInfo])
                        _lhsOsigInfos :: ([SigInfo])
                        _lhsOuniqueInfos :: ([UniqueInfo])
                        -- "src-ag/Transform.ag"(line 910, column 7)
                        _dependency =
                            ({-# LINE 910 "src-ag/Transform.ag" #-}
                             [ Dependency b a | b <- before_, a <- after_ ]
                             {-# LINE 7321 "dist/build/Transform" #-}
                             )
                        -- "src-ag/Transform.ag"(line 911, column 7)
                        _lhsOorderDepsCollect =
                            ({-# LINE 911 "src-ag/Transform.ag" #-}
                             Set.fromList _dependency
                             {-# LINE 7327 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1076, column 40)
                        _lhsOaroundInfos =
                            ({-# LINE 1076 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7333 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1075, column 40)
                        _lhsOaugmentInfos =
                            ({-# LINE 1075 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7339 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1157, column 55)
                        _lhsOdefinedInsts =
                            ({-# LINE 1157 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7345 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 44, column 19)
                        _lhsOerrors =
                            ({-# LINE 44 "src-ag/Transform.ag" #-}
                             Seq.empty
                             {-# LINE 7351 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1077, column 40)
                        _lhsOmergeInfos =
                            ({-# LINE 1077 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7357 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 875, column 46)
                        _lhsOpragmaNamesCollect =
                            ({-# LINE 875 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7363 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1072, column 40)
                        _lhsOruleInfos =
                            ({-# LINE 1072 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7369 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1073, column 40)
                        _lhsOsigInfos =
                            ({-# LINE 1073 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7375 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1074, column 40)
                        _lhsOuniqueInfos =
                            ({-# LINE 1074 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7381 "dist/build/Transform" #-}
                             )
                        ___node =
                            (Syn_SemDef _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos)
                    in  ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos))))
sem_SemDef_AugmentDef :: Identifier ->
                         Expression ->
                         T_SemDef
sem_SemDef_AugmentDef ident_ rhs_ =
    (T_SemDef (\ _lhsIoptions ->
                   (let _lhsOaugmentInfos :: ([AugmentInfo])
                        _lhsOaroundInfos :: ([AroundInfo])
                        _lhsOdefinedInsts :: ([Identifier])
                        _lhsOerrors :: (Seq Error)
                        _lhsOmergeInfos :: ([MergeInfo])
                        _lhsOorderDepsCollect :: (Set Dependency)
                        _lhsOpragmaNamesCollect :: ([Identifier])
                        _lhsOruleInfos :: ([RuleInfo])
                        _lhsOsigInfos :: ([SigInfo])
                        _lhsOuniqueInfos :: ([UniqueInfo])
                        -- "src-ag/Transform.ag"(line 1148, column 17)
                        _lhsOaugmentInfos =
                            ({-# LINE 1148 "src-ag/Transform.ag" #-}
                             [ (ident_, rhs_) ]
                             {-# LINE 7405 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1076, column 40)
                        _lhsOaroundInfos =
                            ({-# LINE 1076 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7411 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1157, column 55)
                        _lhsOdefinedInsts =
                            ({-# LINE 1157 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7417 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 44, column 19)
                        _lhsOerrors =
                            ({-# LINE 44 "src-ag/Transform.ag" #-}
                             Seq.empty
                             {-# LINE 7423 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1077, column 40)
                        _lhsOmergeInfos =
                            ({-# LINE 1077 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7429 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 906, column 44)
                        _lhsOorderDepsCollect =
                            ({-# LINE 906 "src-ag/Transform.ag" #-}
                             Set.empty
                             {-# LINE 7435 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 875, column 46)
                        _lhsOpragmaNamesCollect =
                            ({-# LINE 875 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7441 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1072, column 40)
                        _lhsOruleInfos =
                            ({-# LINE 1072 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7447 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1073, column 40)
                        _lhsOsigInfos =
                            ({-# LINE 1073 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7453 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1074, column 40)
                        _lhsOuniqueInfos =
                            ({-# LINE 1074 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7459 "dist/build/Transform" #-}
                             )
                        ___node =
                            (Syn_SemDef _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos)
                    in  ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos))))
sem_SemDef_Def :: Pos ->
                  (Maybe Identifier) ->
                  T_Pattern ->
                  Expression ->
                  Bool ->
                  Bool ->
                  Bool ->
                  T_SemDef
sem_SemDef_Def pos_ mbName_ (T_Pattern pattern_) rhs_ owrt_ pure_ eager_ =
    (T_SemDef (\ _lhsIoptions ->
                   (let _lhsOerrors :: (Seq Error)
                        _lhsOruleInfos :: ([RuleInfo])
                        _lhsOaroundInfos :: ([AroundInfo])
                        _lhsOaugmentInfos :: ([AugmentInfo])
                        _lhsOdefinedInsts :: ([Identifier])
                        _lhsOmergeInfos :: ([MergeInfo])
                        _lhsOorderDepsCollect :: (Set Dependency)
                        _lhsOpragmaNamesCollect :: ([Identifier])
                        _lhsOsigInfos :: ([SigInfo])
                        _lhsOuniqueInfos :: ([UniqueInfo])
                        _patternIcopy :: Pattern
                        _patternIdefinedAttrs :: ([AttrName])
                        _patternIdefinedInsts :: ([Identifier])
                        _patternIpatunder :: ([AttrName]->Pattern)
                        _patternIstpos :: Pos
                        -- "src-ag/Transform.ag"(line 541, column 3)
                        _lhsOerrors =
                            ({-# LINE 541 "src-ag/Transform.ag" #-}
                             if checkParseRhs _lhsIoptions
                             then Seq.fromList $ checkRhs rhs_
                             else Seq.empty
                             {-# LINE 7495 "dist/build/Transform" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1139, column 10)
                        _lhsOruleInfos =
                            ({-# LINE 1139 "src-ag/Transform.ag" #-}
                             [ (mbName_, _patternIpatunder, rhs_, _patternIdefinedAttrs, owrt_, show _patternIstpos, pure_, eager_) ]
                             {-# LINE 7501 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1076, column 40)
                        _lhsOaroundInfos =
                            ({-# LINE 1076 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7507 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1075, column 40)
                        _lhsOaugmentInfos =
                            ({-# LINE 1075 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7513 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1157, column 55)
                        _lhsOdefinedInsts =
                            ({-# LINE 1157 "src-ag/Transform.ag" #-}
                             _patternIdefinedInsts
                             {-# LINE 7519 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1077, column 40)
                        _lhsOmergeInfos =
                            ({-# LINE 1077 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7525 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 906, column 44)
                        _lhsOorderDepsCollect =
                            ({-# LINE 906 "src-ag/Transform.ag" #-}
                             Set.empty
                             {-# LINE 7531 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 875, column 46)
                        _lhsOpragmaNamesCollect =
                            ({-# LINE 875 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7537 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1073, column 40)
                        _lhsOsigInfos =
                            ({-# LINE 1073 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7543 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1074, column 40)
                        _lhsOuniqueInfos =
                            ({-# LINE 1074 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7549 "dist/build/Transform" #-}
                             )
                        ( _patternIcopy,_patternIdefinedAttrs,_patternIdefinedInsts,_patternIpatunder,_patternIstpos) =
                            pattern_
                        ___node =
                            (Syn_SemDef _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos)
                    in  ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos))))
sem_SemDef_MergeDef :: Identifier ->
                       Identifier ->
                       ([Identifier]) ->
                       Expression ->
                       T_SemDef
sem_SemDef_MergeDef target_ nt_ sources_ rhs_ =
    (T_SemDef (\ _lhsIoptions ->
                   (let _lhsOerrors :: (Seq Error)
                        _lhsOmergeInfos :: ([MergeInfo])
                        _lhsOaroundInfos :: ([AroundInfo])
                        _lhsOaugmentInfos :: ([AugmentInfo])
                        _lhsOdefinedInsts :: ([Identifier])
                        _lhsOorderDepsCollect :: (Set Dependency)
                        _lhsOpragmaNamesCollect :: ([Identifier])
                        _lhsOruleInfos :: ([RuleInfo])
                        _lhsOsigInfos :: ([SigInfo])
                        _lhsOuniqueInfos :: ([UniqueInfo])
                        -- "src-ag/Transform.ag"(line 541, column 3)
                        _lhsOerrors =
                            ({-# LINE 541 "src-ag/Transform.ag" #-}
                             if checkParseRhs _lhsIoptions
                             then Seq.fromList $ checkRhs rhs_
                             else Seq.empty
                             {-# LINE 7579 "dist/build/Transform" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1154, column 17)
                        _lhsOmergeInfos =
                            ({-# LINE 1154 "src-ag/Transform.ag" #-}
                             [ (target_, nt_, sources_, rhs_) ]
                             {-# LINE 7585 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1076, column 40)
                        _lhsOaroundInfos =
                            ({-# LINE 1076 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7591 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1075, column 40)
                        _lhsOaugmentInfos =
                            ({-# LINE 1075 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7597 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1157, column 55)
                        _lhsOdefinedInsts =
                            ({-# LINE 1157 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7603 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 906, column 44)
                        _lhsOorderDepsCollect =
                            ({-# LINE 906 "src-ag/Transform.ag" #-}
                             Set.empty
                             {-# LINE 7609 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 875, column 46)
                        _lhsOpragmaNamesCollect =
                            ({-# LINE 875 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7615 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1072, column 40)
                        _lhsOruleInfos =
                            ({-# LINE 1072 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7621 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1073, column 40)
                        _lhsOsigInfos =
                            ({-# LINE 1073 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7627 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1074, column 40)
                        _lhsOuniqueInfos =
                            ({-# LINE 1074 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7633 "dist/build/Transform" #-}
                             )
                        ___node =
                            (Syn_SemDef _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos)
                    in  ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos))))
sem_SemDef_SemPragma :: ([NontermIdent]) ->
                        T_SemDef
sem_SemDef_SemPragma names_ =
    (T_SemDef (\ _lhsIoptions ->
                   (let _lhsOpragmaNamesCollect :: ([Identifier])
                        _lhsOaroundInfos :: ([AroundInfo])
                        _lhsOaugmentInfos :: ([AugmentInfo])
                        _lhsOdefinedInsts :: ([Identifier])
                        _lhsOerrors :: (Seq Error)
                        _lhsOmergeInfos :: ([MergeInfo])
                        _lhsOorderDepsCollect :: (Set Dependency)
                        _lhsOruleInfos :: ([RuleInfo])
                        _lhsOsigInfos :: ([SigInfo])
                        _lhsOuniqueInfos :: ([UniqueInfo])
                        -- "src-ag/Transform.ag"(line 879, column 7)
                        _lhsOpragmaNamesCollect =
                            ({-# LINE 879 "src-ag/Transform.ag" #-}
                             names_
                             {-# LINE 7656 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1076, column 40)
                        _lhsOaroundInfos =
                            ({-# LINE 1076 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7662 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1075, column 40)
                        _lhsOaugmentInfos =
                            ({-# LINE 1075 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7668 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1157, column 55)
                        _lhsOdefinedInsts =
                            ({-# LINE 1157 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7674 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 44, column 19)
                        _lhsOerrors =
                            ({-# LINE 44 "src-ag/Transform.ag" #-}
                             Seq.empty
                             {-# LINE 7680 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1077, column 40)
                        _lhsOmergeInfos =
                            ({-# LINE 1077 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7686 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 906, column 44)
                        _lhsOorderDepsCollect =
                            ({-# LINE 906 "src-ag/Transform.ag" #-}
                             Set.empty
                             {-# LINE 7692 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1072, column 40)
                        _lhsOruleInfos =
                            ({-# LINE 1072 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7698 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1073, column 40)
                        _lhsOsigInfos =
                            ({-# LINE 1073 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7704 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1074, column 40)
                        _lhsOuniqueInfos =
                            ({-# LINE 1074 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7710 "dist/build/Transform" #-}
                             )
                        ___node =
                            (Syn_SemDef _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos)
                    in  ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos))))
sem_SemDef_TypeDef :: Pos ->
                      Identifier ->
                      Type ->
                      T_SemDef
sem_SemDef_TypeDef pos_ ident_ tp_ =
    (T_SemDef (\ _lhsIoptions ->
                   (let _lhsOerrors :: (Seq Error)
                        _lhsOsigInfos :: ([SigInfo])
                        _lhsOaroundInfos :: ([AroundInfo])
                        _lhsOaugmentInfos :: ([AugmentInfo])
                        _lhsOdefinedInsts :: ([Identifier])
                        _lhsOmergeInfos :: ([MergeInfo])
                        _lhsOorderDepsCollect :: (Set Dependency)
                        _lhsOpragmaNamesCollect :: ([Identifier])
                        _lhsOruleInfos :: ([RuleInfo])
                        _lhsOuniqueInfos :: ([UniqueInfo])
                        -- "src-ag/Transform.ag"(line 548, column 3)
                        _lhsOerrors =
                            ({-# LINE 548 "src-ag/Transform.ag" #-}
                             if checkParseTy _lhsIoptions
                             then case tp_ of
                                    Haskell s -> let exp = Expression pos_ tks
                                                     tks = [tk]
                                                     tk  = HsToken s pos_
                                                 in Seq.fromList $ checkTy exp
                                    _ -> Seq.empty
                             else Seq.empty
                             {-# LINE 7742 "dist/build/Transform" #-}
                             )
                        -- "src-ag/Transform.ag"(line 1142, column 14)
                        _lhsOsigInfos =
                            ({-# LINE 1142 "src-ag/Transform.ag" #-}
                             [ (ident_, tp_) ]
                             {-# LINE 7748 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1076, column 40)
                        _lhsOaroundInfos =
                            ({-# LINE 1076 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7754 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1075, column 40)
                        _lhsOaugmentInfos =
                            ({-# LINE 1075 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7760 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1157, column 55)
                        _lhsOdefinedInsts =
                            ({-# LINE 1157 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7766 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1077, column 40)
                        _lhsOmergeInfos =
                            ({-# LINE 1077 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7772 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 906, column 44)
                        _lhsOorderDepsCollect =
                            ({-# LINE 906 "src-ag/Transform.ag" #-}
                             Set.empty
                             {-# LINE 7778 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 875, column 46)
                        _lhsOpragmaNamesCollect =
                            ({-# LINE 875 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7784 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1072, column 40)
                        _lhsOruleInfos =
                            ({-# LINE 1072 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7790 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1074, column 40)
                        _lhsOuniqueInfos =
                            ({-# LINE 1074 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7796 "dist/build/Transform" #-}
                             )
                        ___node =
                            (Syn_SemDef _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos)
                    in  ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos))))
sem_SemDef_UniqueDef :: Identifier ->
                        Identifier ->
                        T_SemDef
sem_SemDef_UniqueDef ident_ ref_ =
    (T_SemDef (\ _lhsIoptions ->
                   (let _lhsOuniqueInfos :: ([UniqueInfo])
                        _lhsOaroundInfos :: ([AroundInfo])
                        _lhsOaugmentInfos :: ([AugmentInfo])
                        _lhsOdefinedInsts :: ([Identifier])
                        _lhsOerrors :: (Seq Error)
                        _lhsOmergeInfos :: ([MergeInfo])
                        _lhsOorderDepsCollect :: (Set Dependency)
                        _lhsOpragmaNamesCollect :: ([Identifier])
                        _lhsOruleInfos :: ([RuleInfo])
                        _lhsOsigInfos :: ([SigInfo])
                        -- "src-ag/Transform.ag"(line 1145, column 16)
                        _lhsOuniqueInfos =
                            ({-# LINE 1145 "src-ag/Transform.ag" #-}
                             [ (ident_, ref_) ]
                             {-# LINE 7820 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1076, column 40)
                        _lhsOaroundInfos =
                            ({-# LINE 1076 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7826 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1075, column 40)
                        _lhsOaugmentInfos =
                            ({-# LINE 1075 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7832 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1157, column 55)
                        _lhsOdefinedInsts =
                            ({-# LINE 1157 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7838 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 44, column 19)
                        _lhsOerrors =
                            ({-# LINE 44 "src-ag/Transform.ag" #-}
                             Seq.empty
                             {-# LINE 7844 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1077, column 40)
                        _lhsOmergeInfos =
                            ({-# LINE 1077 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7850 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 906, column 44)
                        _lhsOorderDepsCollect =
                            ({-# LINE 906 "src-ag/Transform.ag" #-}
                             Set.empty
                             {-# LINE 7856 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 875, column 46)
                        _lhsOpragmaNamesCollect =
                            ({-# LINE 875 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7862 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1072, column 40)
                        _lhsOruleInfos =
                            ({-# LINE 1072 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7868 "dist/build/Transform" #-}
                             )
                        -- use rule "src-ag/Transform.ag"(line 1073, column 40)
                        _lhsOsigInfos =
                            ({-# LINE 1073 "src-ag/Transform.ag" #-}
                             []
                             {-# LINE 7874 "dist/build/Transform" #-}
                             )
                        ___node =
                            (Syn_SemDef _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos)
                    in  ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos))))
-- SemDefs -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attributes:
         aroundInfos          : [AroundInfo]
         augmentInfos         : [AugmentInfo]
         definedInsts         : [Identifier]
         errors               : Seq Error
         mergeInfos           : [MergeInfo]
         orderDepsCollect     : Set Dependency
         pragmaNamesCollect   : [Identifier]
         ruleInfos            : [RuleInfo]
         sigInfos             : [SigInfo]
         uniqueInfos          : [UniqueInfo]
   alternatives:
      alternative Cons:
         child hd             : SemDef 
         child tl             : SemDefs 
      alternative Nil:
-}
-- cata
sem_SemDefs :: SemDefs ->
               T_SemDefs
sem_SemDefs list =
    (Prelude.foldr sem_SemDefs_Cons sem_SemDefs_Nil (Prelude.map sem_SemDef list))
-- semantic domain
newtype T_SemDefs = T_SemDefs (Options ->
                               ( ([AroundInfo]),([AugmentInfo]),([Identifier]),(Seq Error),([MergeInfo]),(Set Dependency),([Identifier]),([RuleInfo]),([SigInfo]),([UniqueInfo])))
data Inh_SemDefs = Inh_SemDefs {options_Inh_SemDefs :: !(Options)}
data Syn_SemDefs = Syn_SemDefs {aroundInfos_Syn_SemDefs :: !(([AroundInfo])),augmentInfos_Syn_SemDefs :: !(([AugmentInfo])),definedInsts_Syn_SemDefs :: !(([Identifier])),errors_Syn_SemDefs :: !((Seq Error)),mergeInfos_Syn_SemDefs :: !(([MergeInfo])),orderDepsCollect_Syn_SemDefs :: !((Set Dependency)),pragmaNamesCollect_Syn_SemDefs :: !(([Identifier])),ruleInfos_Syn_SemDefs :: !(([RuleInfo])),sigInfos_Syn_SemDefs :: !(([SigInfo])),uniqueInfos_Syn_SemDefs :: !(([UniqueInfo]))}
wrap_SemDefs :: T_SemDefs ->
                Inh_SemDefs ->
                Syn_SemDefs
wrap_SemDefs (T_SemDefs sem) (Inh_SemDefs _lhsIoptions) =
    (let ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos) = sem _lhsIoptions
     in  (Syn_SemDefs _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos))
sem_SemDefs_Cons :: T_SemDef ->
                    T_SemDefs ->
                    T_SemDefs
sem_SemDefs_Cons (T_SemDef hd_) (T_SemDefs tl_) =
    (T_SemDefs (\ _lhsIoptions ->
                    (let _lhsOaroundInfos :: ([AroundInfo])
                         _lhsOaugmentInfos :: ([AugmentInfo])
                         _lhsOdefinedInsts :: ([Identifier])
                         _lhsOerrors :: (Seq Error)
                         _lhsOmergeInfos :: ([MergeInfo])
                         _lhsOorderDepsCollect :: (Set Dependency)
                         _lhsOpragmaNamesCollect :: ([Identifier])
                         _lhsOruleInfos :: ([RuleInfo])
                         _lhsOsigInfos :: ([SigInfo])
                         _lhsOuniqueInfos :: ([UniqueInfo])
                         _hdOoptions :: Options
                         _tlOoptions :: Options
                         _hdIaroundInfos :: ([AroundInfo])
                         _hdIaugmentInfos :: ([AugmentInfo])
                         _hdIdefinedInsts :: ([Identifier])
                         _hdIerrors :: (Seq Error)
                         _hdImergeInfos :: ([MergeInfo])
                         _hdIorderDepsCollect :: (Set Dependency)
                         _hdIpragmaNamesCollect :: ([Identifier])
                         _hdIruleInfos :: ([RuleInfo])
                         _hdIsigInfos :: ([SigInfo])
                         _hdIuniqueInfos :: ([UniqueInfo])
                         _tlIaroundInfos :: ([AroundInfo])
                         _tlIaugmentInfos :: ([AugmentInfo])
                         _tlIdefinedInsts :: ([Identifier])
                         _tlIerrors :: (Seq Error)
                         _tlImergeInfos :: ([MergeInfo])
                         _tlIorderDepsCollect :: (Set Dependency)
                         _tlIpragmaNamesCollect :: ([Identifier])
                         _tlIruleInfos :: ([RuleInfo])
                         _tlIsigInfos :: ([SigInfo])
                         _tlIuniqueInfos :: ([UniqueInfo])
                         -- use rule "src-ag/Transform.ag"(line 1076, column 40)
                         _lhsOaroundInfos =
                             ({-# LINE 1076 "src-ag/Transform.ag" #-}
                              _hdIaroundInfos ++ _tlIaroundInfos
                              {-# LINE 7958 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1075, column 40)
                         _lhsOaugmentInfos =
                             ({-# LINE 1075 "src-ag/Transform.ag" #-}
                              _hdIaugmentInfos ++ _tlIaugmentInfos
                              {-# LINE 7964 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1157, column 55)
                         _lhsOdefinedInsts =
                             ({-# LINE 1157 "src-ag/Transform.ag" #-}
                              _hdIdefinedInsts ++ _tlIdefinedInsts
                              {-# LINE 7970 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 44, column 19)
                         _lhsOerrors =
                             ({-# LINE 44 "src-ag/Transform.ag" #-}
                              _hdIerrors Seq.>< _tlIerrors
                              {-# LINE 7976 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1077, column 40)
                         _lhsOmergeInfos =
                             ({-# LINE 1077 "src-ag/Transform.ag" #-}
                              _hdImergeInfos ++ _tlImergeInfos
                              {-# LINE 7982 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 906, column 44)
                         _lhsOorderDepsCollect =
                             ({-# LINE 906 "src-ag/Transform.ag" #-}
                              _hdIorderDepsCollect `Set.union` _tlIorderDepsCollect
                              {-# LINE 7988 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 875, column 46)
                         _lhsOpragmaNamesCollect =
                             ({-# LINE 875 "src-ag/Transform.ag" #-}
                              _hdIpragmaNamesCollect ++ _tlIpragmaNamesCollect
                              {-# LINE 7994 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1072, column 40)
                         _lhsOruleInfos =
                             ({-# LINE 1072 "src-ag/Transform.ag" #-}
                              _hdIruleInfos ++ _tlIruleInfos
                              {-# LINE 8000 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1073, column 40)
                         _lhsOsigInfos =
                             ({-# LINE 1073 "src-ag/Transform.ag" #-}
                              _hdIsigInfos ++ _tlIsigInfos
                              {-# LINE 8006 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1074, column 40)
                         _lhsOuniqueInfos =
                             ({-# LINE 1074 "src-ag/Transform.ag" #-}
                              _hdIuniqueInfos ++ _tlIuniqueInfos
                              {-# LINE 8012 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _hdOoptions =
                             ({-# LINE 40 "src-ag/Transform.ag" #-}
                              _lhsIoptions
                              {-# LINE 8018 "dist/build/Transform" #-}
                              )
                         -- copy rule (down)
                         _tlOoptions =
                             ({-# LINE 40 "src-ag/Transform.ag" #-}
                              _lhsIoptions
                              {-# LINE 8024 "dist/build/Transform" #-}
                              )
                         ( _hdIaroundInfos,_hdIaugmentInfos,_hdIdefinedInsts,_hdIerrors,_hdImergeInfos,_hdIorderDepsCollect,_hdIpragmaNamesCollect,_hdIruleInfos,_hdIsigInfos,_hdIuniqueInfos) =
                             hd_ _hdOoptions
                         ( _tlIaroundInfos,_tlIaugmentInfos,_tlIdefinedInsts,_tlIerrors,_tlImergeInfos,_tlIorderDepsCollect,_tlIpragmaNamesCollect,_tlIruleInfos,_tlIsigInfos,_tlIuniqueInfos) =
                             tl_ _tlOoptions
                         ___node =
                             (Syn_SemDefs _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos)
                     in  ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos))))
sem_SemDefs_Nil :: T_SemDefs
sem_SemDefs_Nil =
    (T_SemDefs (\ _lhsIoptions ->
                    (let _lhsOaroundInfos :: ([AroundInfo])
                         _lhsOaugmentInfos :: ([AugmentInfo])
                         _lhsOdefinedInsts :: ([Identifier])
                         _lhsOerrors :: (Seq Error)
                         _lhsOmergeInfos :: ([MergeInfo])
                         _lhsOorderDepsCollect :: (Set Dependency)
                         _lhsOpragmaNamesCollect :: ([Identifier])
                         _lhsOruleInfos :: ([RuleInfo])
                         _lhsOsigInfos :: ([SigInfo])
                         _lhsOuniqueInfos :: ([UniqueInfo])
                         -- use rule "src-ag/Transform.ag"(line 1076, column 40)
                         _lhsOaroundInfos =
                             ({-# LINE 1076 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 8050 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1075, column 40)
                         _lhsOaugmentInfos =
                             ({-# LINE 1075 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 8056 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1157, column 55)
                         _lhsOdefinedInsts =
                             ({-# LINE 1157 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 8062 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 44, column 19)
                         _lhsOerrors =
                             ({-# LINE 44 "src-ag/Transform.ag" #-}
                              Seq.empty
                              {-# LINE 8068 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1077, column 40)
                         _lhsOmergeInfos =
                             ({-# LINE 1077 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 8074 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 906, column 44)
                         _lhsOorderDepsCollect =
                             ({-# LINE 906 "src-ag/Transform.ag" #-}
                              Set.empty
                              {-# LINE 8080 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 875, column 46)
                         _lhsOpragmaNamesCollect =
                             ({-# LINE 875 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 8086 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1072, column 40)
                         _lhsOruleInfos =
                             ({-# LINE 1072 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 8092 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1073, column 40)
                         _lhsOsigInfos =
                             ({-# LINE 1073 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 8098 "dist/build/Transform" #-}
                              )
                         -- use rule "src-ag/Transform.ag"(line 1074, column 40)
                         _lhsOuniqueInfos =
                             ({-# LINE 1074 "src-ag/Transform.ag" #-}
                              []
                              {-# LINE 8104 "dist/build/Transform" #-}
                              )
                         ___node =
                             (Syn_SemDefs _lhsOaroundInfos _lhsOaugmentInfos _lhsOdefinedInsts _lhsOerrors _lhsOmergeInfos _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos)
                     in  ( _lhsOaroundInfos,_lhsOaugmentInfos,_lhsOdefinedInsts,_lhsOerrors,_lhsOmergeInfos,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos))))