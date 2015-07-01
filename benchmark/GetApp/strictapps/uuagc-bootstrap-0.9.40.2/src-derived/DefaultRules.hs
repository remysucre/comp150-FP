{-# OPTIONS_GHC -XBangPatterns #-}

-- UUAGC 0.9.40.1 (src-ag/DefaultRules.ag)
module DefaultRules where
{-# LINE 15 "src-ag/DefaultRules.ag" #-}

import qualified Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq,(><))
import UU.Scanner.Position(noPos)
import Pretty
import Data.Maybe
import HsToken
import HsTokenScanner
import Data.List(intersperse)

import AbstractSyntax
import ErrorMessages

import Options
{-# LINE 25 "dist/build/DefaultRules.hs" #-}

{-# LINE 2 "src-ag/AbstractSyntax.ag" #-}

-- AbstractSyntax.ag imports
import Data.Set(Set)
import Data.Map(Map)
import Patterns    (Pattern(..),Patterns)
import Expression  (Expression(..))
import Macro --marcos
import CommonTypes
import ErrorMessages
{-# LINE 37 "dist/build/DefaultRules.hs" #-}

{-# LINE 2 "src-ag/Patterns.ag" #-}

-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
{-# LINE 44 "dist/build/DefaultRules.hs" #-}
{-# LINE 77 "src-ag/DefaultRules.ag" #-}

fieldName n       = '@' : getName n

locName n         = "@loc." ++ getName n

attrName fld attr
 | fld == _LOC    = locName attr
 | fld == _FIELD  = fieldName attr
 | otherwise      = '@' : getName fld ++ "." ++ getName attr

_ACHILD = Ident "(" noPos -- hack

mkLocVar = AGField _LOC


getConName typeSyns rename nt con1
 | nt `elem` map fst typeSyns  =  synonym
 | otherwise                   =  normalName
 where con                            = getName con1
       normalName | rename            = getName nt++"_"++ con
                  | otherwise         =  con
       synonym    | con == "Cons"     = "(:)"
                  | con == "Nil"      = case lookup nt typeSyns of
                                          Just (Map _ _)  -> "Data.Map.empty"
                                          Just (IntMap _) -> "Data.IntMap.empty"
                                          Just (OrdSet _) -> "Data.Set.empty"
                                          Just IntSet     -> "Data.IntSet.empty"
                                          _               -> "[]"
                  | con == "Just"     = "Just"
                  | con == "Nothing"  = "Nothing"
                  | con == "Entry"    = case lookup nt typeSyns of
                                          Just (Map _ _)  -> "Data.Map.insert"
                                          Just (IntMap _) -> "Data.IntMap.insert"
                                          Just (OrdSet _) -> "Data.Set.insert"
                                          Just IntSet     -> "Data.IntSet.insert"
                  | otherwise         = normalName

concatSeq = foldr (Seq.><) Seq.empty

splitAttrs :: Map Identifier a -> [Identifier] -> ([(Identifier,a)],[Identifier])	  -- a used as (String,String)
splitAttrs _      []
  =  ([],[])
splitAttrs useMap (n:rest)
  =  let (uses,normals) = splitAttrs useMap rest
     in case Map.lookup n useMap of
          Just x  -> ((n,x):uses ,   normals )
          Nothing -> (      uses , n:normals )

removeDefined ::  Set (Identifier,Identifier) -> (Identifier,Attributes) -> (Identifier,[Identifier])
removeDefined defined (fld,as)
  = ( fld
    , [ a
      | a <- Map.keys as
      , not (Set.member (fld,a) defined)
      ]
    )

{-# LINE 103 "dist/build/DefaultRules.hs" #-}

{-# LINE 200 "src-ag/DefaultRules.ag" #-}




deprecatedCopyRuleError nt con fld a
 = let mesg =
                "In the definitions for alternative"
            >#< getName con
            >#< "of nonterminal"
            >#< getName nt
            >|< ","
            >-< "the value of field"
            >#< getName a
            >#< "is copied by a copy-rule."
            >-< "Copying the value of a field using a copy-rule is deprecated"
            >-< "Please add the following lines to your code:"
            >-< (    "SEM"
                >#< getName nt
                >-< indent 2 (      "|"
                             >#< getName con
                             >#< getName fld
                             >#< "."
                             >#< a
                             >#< "="
                             >#< "@"
                             >|< a
                             )
                )
    in  CustomError True (getPos a) mesg


missingRuleErrorExpr nt con fld a
 = "error \"missing rule: "
   ++ show nt  ++ "." ++ show con ++ "."
   ++ show fld ++ "." ++ show a   ++ "\""



makeRule :: (Identifier,Identifier) -> Expression -> String -> Bool -> Maybe Error -> Rule
makeRule (f1,a1) expr origin identity mbDelayedError
 = Rule Nothing
        (Alias f1 a1 (Underscore noPos))
        expr
        False
        origin
        False
        True
        identity
        mbDelayedError
        False


useRule :: Set Identifier -> [(Identifier,Attributes)] -> (Identifier,(String,String,String)) -> Rule
useRule locals ch_outs (n,(op,e,pos))
 =  let elems = [ fld
                | (fld,as) <- ch_outs
                , Map.member n as
                ]

        expr | Set.member n locals  =  attrName _LOC n
             | null elems           =  e
             | otherwise            =  foldr1 (\x y -> x ++ " " ++ op ++ " " ++ y)
                                              (map (flip attrName n) elems)

        tks | Set.member n locals  =  [mkLocVar n noPos Nothing]
            | null elems           =  lexTokens noPos e
            | otherwise            =  lexTokens noPos str
                                      where
                                        str = foldr1 (\x y -> x ++ " " ++ op ++ " " ++ y)
                                                (map (flip attrName n) elems)

    in makeRule (_LHS,n)
                (Expression noPos tks)
                ("use rule " ++ pos)
                False
                Nothing


selfRule :: Bool -> Identifier -> [HsToken] -> Rule
selfRule lhsNecLoc attr tks
 = makeRule (if lhsNecLoc then _LHS else _LOC,attr)
               (Expression noPos tks)
               "self rule"
               False
               Nothing




concatRE rsess = let (rss,ess) = unzip rsess
                 in (concat rss, concatSeq ess)


copyRule :: Options -> Set NontermIdent -> Identifier -> Identifier -> Bool -> Set Identifier -> (Map Identifier Identifier, (Identifier,[Identifier])) -> ([Rule], Seq Error)
copyRule options wrappers nt con modcopy locals (env,(fld,as))
 = concatRE (map copyRu as)

 where
       copyRu a
           = ( [ makeRule (fld,a)
                          (Expression noPos tks)
                          (cruletxt sel)
                          True
                          mbDelayedErr
               ]
             , err
             )

        where
              sel
               |    not modcopy
                 && Set.member a locals  =  Just _LOC
               | otherwise               =  Map.lookup a env

              (tks,err,mbDelayedErr)
               = case sel of
                  Nothing         -> let tks = [HsToken (missingRuleErrorExpr nt con fld a) noPos]
                                         err = MissingRule nt con fld a
                                     in if nt `Set.member` wrappers && kennedyWarren options
                                        then (tks, Seq.empty, Just err)  -- yield error only if the rule is actually scheduled; for kennedyWarren code gen only
                                        else (tks, Seq.singleton err, Nothing)
                  Just f
                   | f == _ACHILD -> ( [AGLocal a noPos Nothing]
                                     , Seq.singleton (deprecatedCopyRuleError nt con fld a)
                                     , Nothing
                                     )
                   | otherwise    -> ( [AGField f a noPos Nothing]
                                     , Seq.empty
                                     , Nothing
                                     )

              cruletxt sel
               | local                            = "copy rule (from local)"
               | deprChild                        = "deprecated child copy"
               | Set.member a locals && nonlocal  = "modified copy rule"
               | incoming && outgoing             = "copy rule (chain)"
               | incoming                         = "copy rule (down)"
               | outgoing                         = "copy rule (up)"
               | otherwise                        = "copy rule (chain)"
                where outgoing  =  fld == _LHS
                      incoming  =  maybe False (== _LHS)    sel
                      nonlocal  =  maybe False (/= _LOC)    sel
                      local     =  maybe False (== _LOC)    sel
                      deprChild =  maybe False (== _ACHILD) sel
{-# LINE 250 "dist/build/DefaultRules.hs" #-}

{-# LINE 431 "src-ag/DefaultRules.ag" #-}

addAugments :: (Identifier, [Expression]) -> [Rule] -> [Rule]
addAugments (_, exprs) rules
  | null exprs = rules
addAugments (syn, exprs) rules
  = [rule] ++ funRules ++ map modify rules
  where
    rule = Rule Nothing (Alias _LHS syn (Underscore noPos)) rhs False "augmented rule" False True False Nothing False
    rhs  = Expression noPos tks
    tks  = [ HsToken "foldr ($) " noPos, mkLocVar substSyn noPos Nothing, HsToken " [" noPos] ++ funs ++ [HsToken "]" noPos]
    funs = intersperse (HsToken ", " noPos) (map (\n -> mkLocVar n noPos Nothing) funNames)

    substSyn = Ident (show syn ++ "_augmented_syn") (getPos syn)
    funNames = zipWith (\i _ -> Ident (show syn ++ "_augmented_f" ++ show i) (getPos syn)) [1..] exprs
    funRules = zipWith (\name expr -> Rule Nothing (Alias _LOC name (Underscore noPos)) expr False "augment function" False True False Nothing False) funNames exprs

    modify (Rule mbNm pat rhs owrt origin expl pure identity mbErr eager)
      | containsSyn pat = Rule mbNm (modifyPat pat) rhs owrt origin expl pure identity mbErr eager
    modify r = r

    containsSyn (Constr _ pats)   = any containsSyn pats
    containsSyn (Product _ pats)  = any containsSyn pats
    containsSyn (Irrefutable pat) = containsSyn pat
    containsSyn (Alias field attr pat) = (field == _LHS && attr == syn) || containsSyn pat
    containsSyn _ = False

    modifyPat (Constr name pats) = Constr name (map modifyPat pats)
    modifyPat (Product pos pats) = Product pos (map modifyPat pats)
    modifyPat (Irrefutable pat)  = Irrefutable (modifyPat pat)
    modifyPat (Alias field attr pat)
      | field == _LHS && attr == syn = Alias _LOC substSyn (modifyPat pat)
      | otherwise                    = Alias field attr (modifyPat pat)
    modifyPat p = p

-- adds the additional rules needed for around, which creates a sequence of
-- rules that form a function that each transforms the semantics of a child
-- before attaching the child.
-- The rule defines a local attribute "<child>_around" and <child> is dependent
-- on this attribute.
addArounds :: (Identifier, [Expression]) -> [Rule] -> [Rule]
addArounds (_, exprs) rules | null exprs = rules
addArounds (child, exprs) rules
  = [rule] ++ funRules ++ rules
  where
    rule = Rule Nothing (Alias _LOC childLoc (Underscore noPos)) rhs False "around rule" False True False Nothing False
    rhs  = Expression noPos tks
    tks  = [ HsToken "\\s -> foldr ($) s " noPos, HsToken " [" noPos] ++ funs ++ [HsToken "]" noPos]
    funs = intersperse (HsToken ", " noPos) (map (\n -> mkLocVar n noPos Nothing) funNames)

    childLoc = Ident (show child ++ "_around") (getPos child)
    funNames = zipWith (\i _ -> Ident (show child ++ "_around_f" ++ show i) (getPos child)) [1..] exprs
    funRules = zipWith (\name expr -> Rule Nothing (Alias _LOC name (Underscore noPos)) expr False "around function" False True False Nothing False) funNames exprs

-- adds the additional rules needed for merging.
-- It produces for each merging child a rule with local attribute: "<child>_merged".
-- this rules takes the semantics of the first children and feeds it to the function
-- represented by this attribute. This attribute then defines the semantics for
-- the merging child.
addMerges :: (Identifier, (Identifier,[Identifier],Expression)) -> [Rule] -> [Rule]
addMerges (target,(_,_,expr)) rules
  = rule : rules
  where
    rule = Rule Nothing (Alias _LOC childLoc (Underscore noPos)) expr False "merge rule" False True False Nothing False
    childLoc = Ident (show target ++ "_merge") (getPos target)
{-# LINE 317 "dist/build/DefaultRules.hs" #-}

{-# LINE 547 "src-ag/DefaultRules.ag" #-}

elimSelfId :: NontermIdent -> [Identifier] -> Type -> Type
elimSelfId nt args Self = NT nt (map locname args) False
elimSelfId _ _ tp = tp

elimSelfStr :: NontermIdent -> [String] -> Type -> Type
elimSelfStr nt args Self = NT nt args False
elimSelfStr _ _ tp = tp
{-# LINE 328 "dist/build/DefaultRules.hs" #-}

{-# LINE 599 "src-ag/DefaultRules.ag" #-}

-- When a rule has a name, create an alias for a rule
-- and a modified rule that refers to the alias
-- Thus it removes rule names from rules
mkRuleAlias :: Rule -> (Rule, Maybe Rule)
mkRuleAlias r@(Rule Nothing _ _ _ _ _ _ _ _ _) = (r, Nothing)
mkRuleAlias (Rule (Just nm) pat expr owrt origin expl pure identity mbErr eager) = (r', Just alias) where
  alias = Rule Nothing (Alias _LOC (Ident ("_rule_" ++ show nm) pos) (Underscore pos)) expr owrt origin expl pure identity mbErr eager
  pos   = getPos nm
  expr' = Expression pos tks
  tks   = [mkLocVar (Ident ("_rule_" ++ show nm) pos) pos (Just ("Indirection to rule " ++ show nm))]
  r'    = Rule Nothing pat expr' owrt origin False True identity Nothing False
{-# LINE 343 "dist/build/DefaultRules.hs" #-}

{-# LINE 616 "src-ag/DefaultRules.ag" #-}

needsMultiRules :: Options -> Bool
needsMultiRules opts = (visit opts || withCycle opts) && not (kennedyWarren opts)
{-# LINE 349 "dist/build/DefaultRules.hs" #-}

{-# LINE 621 "src-ag/DefaultRules.ag" #-}

{-
multiRule replaces
  loc.(a,b) = e
by
  loc.tup1  = e
  loc.(a,_) = @loc.tup1
  loc.(_,b) = @loc.tup1
It needs to thread a unique number for inventing names for the tuples.

It also works for nested tuples:
  loc.(a,(b,c)) = e
becomes
  loc.tup1      = e
  loc.(a,_)     = @loc.tup1
  loc.(_,tup2)  = @loc.tup1
  loc.(b,_)     = @loc.tup2
  loc.(_,c)     = @loc.tup2
-}

multiRule :: Rule -> Int -> ([Rule], Int)
multiRule (Rule _ pat expr owrt origin expl pure identity mbErr eager) uniq
  =  let f :: Bool -> (Pattern->Pattern) -> Expression -> Pattern -> Int -> (Pattern, ([Rule], Int))
         f expl' w e (Product pos pats) n
           = let freshName = Ident ("_tup" ++ show n) pos
                 freshExpr = Expression pos freshTks
                 freshTks  = [AGField _LOC freshName pos Nothing]
                 freshPat  = Alias _LOC freshName (Underscore pos)
                 a = length pats - 1
                 us b p = Product pos (replicate (a-b) (Underscore pos) ++ [p] ++ replicate b (Underscore pos))
                 g :: Pattern -> ([Pattern],[Rule],Int) -> ([Pattern],[Rule],Int)
                 g p (xs1,rs1,n1)   = let (x2,(rs2,n2)) = f False (us (length xs1)) freshExpr p n1
                                      in  (x2:xs1, rs2++rs1, n2)
                 (xs9,rs9,n9) = foldr g ([], [], n+1) pats
             in  ( freshPat
                 , ( Rule Nothing (w freshPat) e owrt origin expl' True False mbErr eager : rs9
                   , n9
                   )
                 )
         f expl' w e p n
           = ( p
             , ( [Rule Nothing (w p) e owrt origin expl' True False mbErr eager]
               , n
               )
             )
     in snd (f expl id expr pat uniq)

{-# LINE 399 "dist/build/DefaultRules.hs" #-}
-- Child -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         con                  : ConstructorIdent
         cr                   : Bool
         inhMap               : Map Identifier Attributes
         merged               : Set Identifier
         nt                   : NontermIdent
         params               : [Identifier]
         synMap               : Map Identifier Attributes
      synthesized attributes:
         errors               : Seq Error
         field                :  (Identifier,Type,ChildKind) 
         inherited            : Attributes
         name                 : Identifier
         output               : Child 
         synthesized          : Attributes
   alternatives:
      alternative Child:
         child name           : {Identifier}
         child tp             : {Type}
         child kind           : {ChildKind}
         visit 0:
            local chnt        : _
            local inh         : _
            local _tup1       : _
            local params      : _
            local nt          : _
            local inh1        : _
            local syn         : _
            local syn1        : _
-}
-- cata
sem_Child :: Child ->
             T_Child
sem_Child !(Child _name _tp _kind) =
    (sem_Child_Child _name _tp _kind)
-- semantic domain
newtype T_Child = T_Child (ConstructorIdent ->
                           Bool ->
                           (Map Identifier Attributes) ->
                           (Set Identifier) ->
                           NontermIdent ->
                           ([Identifier]) ->
                           (Map Identifier Attributes) ->
                           ( (Seq Error),( (Identifier,Type,ChildKind) ),Attributes,Identifier,Child,Attributes))
data Inh_Child = Inh_Child {con_Inh_Child :: !(ConstructorIdent),cr_Inh_Child :: !(Bool),inhMap_Inh_Child :: !((Map Identifier Attributes)),merged_Inh_Child :: !((Set Identifier)),nt_Inh_Child :: !(NontermIdent),params_Inh_Child :: !(([Identifier])),synMap_Inh_Child :: !((Map Identifier Attributes))}
data Syn_Child = Syn_Child {errors_Syn_Child :: !((Seq Error)),field_Syn_Child :: !(( (Identifier,Type,ChildKind) )),inherited_Syn_Child :: !(Attributes),name_Syn_Child :: !(Identifier),output_Syn_Child :: !(Child),synthesized_Syn_Child :: !(Attributes)}
wrap_Child :: T_Child ->
              Inh_Child ->
              Syn_Child
wrap_Child !(T_Child sem) !(Inh_Child _lhsIcon _lhsIcr _lhsIinhMap _lhsImerged _lhsInt _lhsIparams _lhsIsynMap) =
    (let ( !_lhsOerrors,!_lhsOfield,!_lhsOinherited,!_lhsOname,!_lhsOoutput,!_lhsOsynthesized) = sem _lhsIcon _lhsIcr _lhsIinhMap _lhsImerged _lhsInt _lhsIparams _lhsIsynMap
     in  (Syn_Child _lhsOerrors _lhsOfield _lhsOinherited _lhsOname _lhsOoutput _lhsOsynthesized))
sem_Child_Child :: Identifier ->
                   Type ->
                   ChildKind ->
                   T_Child
sem_Child_Child !name_ !tp_ !kind_ =
    (T_Child (\ (!_lhsIcon)
                (!_lhsIcr)
                (!_lhsIinhMap)
                (!_lhsImerged)
                (!_lhsInt)
                (!_lhsIparams)
                (!_lhsIsynMap) ->
                  (case (({-# LINE 142 "src-ag/DefaultRules.ag" #-}
                          Seq.empty
                          {-# LINE 469 "dist/build/DefaultRules" #-}
                          )) of
                   { !_lhsOerrors ->
                   (case (({-# LINE 515 "src-ag/DefaultRules.ag" #-}
                           (name_,tp_,kind_)
                           {-# LINE 474 "dist/build/DefaultRules" #-}
                           )) of
                    { !_lhsOfield ->
                    (case (({-# LINE 19 "src-ag/DistChildAttr.ag" #-}
                            case tp_ of
                              NT nt _ _ -> nt
                              Self      -> error ("The type of child " ++ show name_ ++ " should not be a Self type.")
                              Haskell t -> identifier ""
                            {-# LINE 482 "dist/build/DefaultRules" #-}
                            )) of
                     { !_chnt ->
                     (case (({-# LINE 23 "src-ag/DistChildAttr.ag" #-}
                             Map.findWithDefault Map.empty _chnt     _lhsIinhMap
                             {-# LINE 487 "dist/build/DefaultRules" #-}
                             )) of
                      { !_inh ->
                      (case (({-# LINE 537 "src-ag/DefaultRules.ag" #-}
                              case tp_ of
                                NT nt params _ -> (nt, params)
                                Self           -> error ("The type of child " ++ show name_ ++ " should not be a Self type.")
                                Haskell t      -> (identifier t, [])
                              {-# LINE 495 "dist/build/DefaultRules" #-}
                              )) of
                       { !__tup1 ->
                       (case (({-# LINE 537 "src-ag/DefaultRules.ag" #-}
                               __tup1
                               {-# LINE 500 "dist/build/DefaultRules" #-}
                               )) of
                        { !(_,!_params) ->
                        (case (({-# LINE 537 "src-ag/DefaultRules.ag" #-}
                                __tup1
                                {-# LINE 505 "dist/build/DefaultRules" #-}
                                )) of
                         { !(!_nt,_) ->
                         (case (({-# LINE 541 "src-ag/DefaultRules.ag" #-}
                                 Map.map (elimSelfStr _nt     _params    ) _inh
                                 {-# LINE 510 "dist/build/DefaultRules" #-}
                                 )) of
                          { !_inh1 ->
                          (case (({-# LINE 183 "src-ag/DefaultRules.ag" #-}
                                  _inh1
                                  {-# LINE 515 "dist/build/DefaultRules" #-}
                                  )) of
                           { !_lhsOinherited ->
                           (case (({-# LINE 174 "src-ag/DefaultRules.ag" #-}
                                   name_
                                   {-# LINE 520 "dist/build/DefaultRules" #-}
                                   )) of
                            { !_lhsOname ->
                            (case (({-# LINE 583 "src-ag/DefaultRules.ag" #-}
                                    Child name_ tp_ kind_
                                    {-# LINE 525 "dist/build/DefaultRules" #-}
                                    )) of
                             { !_lhsOoutput ->
                             (case (({-# LINE 24 "src-ag/DistChildAttr.ag" #-}
                                     Map.findWithDefault Map.empty _chnt     _lhsIsynMap
                                     {-# LINE 530 "dist/build/DefaultRules" #-}
                                     )) of
                              { !_syn ->
                              (case (({-# LINE 542 "src-ag/DefaultRules.ag" #-}
                                      Map.map (elimSelfStr _nt     _params    ) _syn
                                      {-# LINE 535 "dist/build/DefaultRules" #-}
                                      )) of
                               { !_syn1 ->
                               (case (({-# LINE 184 "src-ag/DefaultRules.ag" #-}
                                       if name_ `Set.member` _lhsImerged
                                       then Map.empty
                                       else _syn1
                                       {-# LINE 542 "dist/build/DefaultRules" #-}
                                       )) of
                                { !_lhsOsynthesized ->
                                (case ((Syn_Child _lhsOerrors _lhsOfield _lhsOinherited _lhsOname _lhsOoutput _lhsOsynthesized)) of
                                 { ___node ->
                                 ( _lhsOerrors,_lhsOfield,_lhsOinherited,_lhsOname,_lhsOoutput,_lhsOsynthesized) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
-- Children ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         con                  : ConstructorIdent
         cr                   : Bool
         inhMap               : Map Identifier Attributes
         merged               : Set Identifier
         nt                   : NontermIdent
         params               : [Identifier]
         synMap               : Map Identifier Attributes
      synthesized attributes:
         errors               : Seq Error
         fields               : [(Identifier,Type,ChildKind)]
         inputs               : [(Identifier, Attributes)]
         output               : Children 
         outputs              : [(Identifier, Attributes)]
   alternatives:
      alternative Cons:
         child hd             : Child 
         child tl             : Children 
         visit 0:
            local output      : _
      alternative Nil:
         visit 0:
            local output      : _
-}
-- cata
sem_Children :: Children ->
                T_Children
sem_Children !list =
    (Prelude.foldr sem_Children_Cons sem_Children_Nil (Prelude.map sem_Child list))
-- semantic domain
newtype T_Children = T_Children (ConstructorIdent ->
                                 Bool ->
                                 (Map Identifier Attributes) ->
                                 (Set Identifier) ->
                                 NontermIdent ->
                                 ([Identifier]) ->
                                 (Map Identifier Attributes) ->
                                 ( (Seq Error),([(Identifier,Type,ChildKind)]),([(Identifier, Attributes)]),Children,([(Identifier, Attributes)])))
data Inh_Children = Inh_Children {con_Inh_Children :: !(ConstructorIdent),cr_Inh_Children :: !(Bool),inhMap_Inh_Children :: !((Map Identifier Attributes)),merged_Inh_Children :: !((Set Identifier)),nt_Inh_Children :: !(NontermIdent),params_Inh_Children :: !(([Identifier])),synMap_Inh_Children :: !((Map Identifier Attributes))}
data Syn_Children = Syn_Children {errors_Syn_Children :: !((Seq Error)),fields_Syn_Children :: !(([(Identifier,Type,ChildKind)])),inputs_Syn_Children :: !(([(Identifier, Attributes)])),output_Syn_Children :: !(Children),outputs_Syn_Children :: !(([(Identifier, Attributes)]))}
wrap_Children :: T_Children ->
                 Inh_Children ->
                 Syn_Children
wrap_Children !(T_Children sem) !(Inh_Children _lhsIcon _lhsIcr _lhsIinhMap _lhsImerged _lhsInt _lhsIparams _lhsIsynMap) =
    (let ( !_lhsOerrors,!_lhsOfields,!_lhsOinputs,!_lhsOoutput,!_lhsOoutputs) = sem _lhsIcon _lhsIcr _lhsIinhMap _lhsImerged _lhsInt _lhsIparams _lhsIsynMap
     in  (Syn_Children _lhsOerrors _lhsOfields _lhsOinputs _lhsOoutput _lhsOoutputs))
sem_Children_Cons :: T_Child ->
                     T_Children ->
                     T_Children
sem_Children_Cons !(T_Child hd_) !(T_Children tl_) =
    (T_Children (\ (!_lhsIcon)
                   (!_lhsIcr)
                   (!_lhsIinhMap)
                   (!_lhsImerged)
                   (!_lhsInt)
                   (!_lhsIparams)
                   (!_lhsIsynMap) ->
                     (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                             _lhsIsynMap
                             {-# LINE 610 "dist/build/DefaultRules" #-}
                             )) of
                      { !_tlOsynMap ->
                      (case (({-# LINE 41 "src-ag/DefaultRules.ag" #-}
                              _lhsIparams
                              {-# LINE 615 "dist/build/DefaultRules" #-}
                              )) of
                       { !_tlOparams ->
                       (case (({-# LINE 38 "src-ag/DefaultRules.ag" #-}
                               _lhsInt
                               {-# LINE 620 "dist/build/DefaultRules" #-}
                               )) of
                        { !_tlOnt ->
                        (case (({-# LINE 757 "src-ag/DefaultRules.ag" #-}
                                _lhsImerged
                                {-# LINE 625 "dist/build/DefaultRules" #-}
                                )) of
                         { !_tlOmerged ->
                         (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                 _lhsIinhMap
                                 {-# LINE 630 "dist/build/DefaultRules" #-}
                                 )) of
                          { !_tlOinhMap ->
                          (case (({-# LINE 55 "src-ag/DefaultRules.ag" #-}
                                  _lhsIcr
                                  {-# LINE 635 "dist/build/DefaultRules" #-}
                                  )) of
                           { !_tlOcr ->
                           (case (({-# LINE 39 "src-ag/DefaultRules.ag" #-}
                                   _lhsIcon
                                   {-# LINE 640 "dist/build/DefaultRules" #-}
                                   )) of
                            { !_tlOcon ->
                            (case (tl_ _tlOcon _tlOcr _tlOinhMap _tlOmerged _tlOnt _tlOparams _tlOsynMap) of
                             { ( !_tlIerrors,!_tlIfields,!_tlIinputs,!_tlIoutput,!_tlIoutputs) ->
                                 (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                         _lhsIsynMap
                                         {-# LINE 647 "dist/build/DefaultRules" #-}
                                         )) of
                                  { !_hdOsynMap ->
                                  (case (({-# LINE 41 "src-ag/DefaultRules.ag" #-}
                                          _lhsIparams
                                          {-# LINE 652 "dist/build/DefaultRules" #-}
                                          )) of
                                   { !_hdOparams ->
                                   (case (({-# LINE 38 "src-ag/DefaultRules.ag" #-}
                                           _lhsInt
                                           {-# LINE 657 "dist/build/DefaultRules" #-}
                                           )) of
                                    { !_hdOnt ->
                                    (case (({-# LINE 757 "src-ag/DefaultRules.ag" #-}
                                            _lhsImerged
                                            {-# LINE 662 "dist/build/DefaultRules" #-}
                                            )) of
                                     { !_hdOmerged ->
                                     (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                             _lhsIinhMap
                                             {-# LINE 667 "dist/build/DefaultRules" #-}
                                             )) of
                                      { !_hdOinhMap ->
                                      (case (({-# LINE 55 "src-ag/DefaultRules.ag" #-}
                                              _lhsIcr
                                              {-# LINE 672 "dist/build/DefaultRules" #-}
                                              )) of
                                       { !_hdOcr ->
                                       (case (({-# LINE 39 "src-ag/DefaultRules.ag" #-}
                                               _lhsIcon
                                               {-# LINE 677 "dist/build/DefaultRules" #-}
                                               )) of
                                        { !_hdOcon ->
                                        (case (hd_ _hdOcon _hdOcr _hdOinhMap _hdOmerged _hdOnt _hdOparams _hdOsynMap) of
                                         { ( !_hdIerrors,!_hdIfield,!_hdIinherited,!_hdIname,!_hdIoutput,!_hdIsynthesized) ->
                                             (case (({-# LINE 142 "src-ag/DefaultRules.ag" #-}
                                                     _hdIerrors Seq.>< _tlIerrors
                                                     {-# LINE 684 "dist/build/DefaultRules" #-}
                                                     )) of
                                              { !_lhsOerrors ->
                                              (case (({-# LINE 511 "src-ag/DefaultRules.ag" #-}
                                                      _hdIfield : _tlIfields
                                                      {-# LINE 689 "dist/build/DefaultRules" #-}
                                                      )) of
                                               { !_lhsOfields ->
                                               (case (({-# LINE 189 "src-ag/DefaultRules.ag" #-}
                                                       (_hdIname, _hdIinherited) : _tlIinputs
                                                       {-# LINE 694 "dist/build/DefaultRules" #-}
                                                       )) of
                                                { !_lhsOinputs ->
                                                (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                                        (:) _hdIoutput _tlIoutput
                                                        {-# LINE 699 "dist/build/DefaultRules" #-}
                                                        )) of
                                                 { !_output ->
                                                 (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                                         _output
                                                         {-# LINE 704 "dist/build/DefaultRules" #-}
                                                         )) of
                                                  { !_lhsOoutput ->
                                                  (case (({-# LINE 190 "src-ag/DefaultRules.ag" #-}
                                                          (_hdIname, _hdIsynthesized) : _tlIoutputs
                                                          {-# LINE 709 "dist/build/DefaultRules" #-}
                                                          )) of
                                                   { !_lhsOoutputs ->
                                                   (case ((Syn_Children _lhsOerrors _lhsOfields _lhsOinputs _lhsOoutput _lhsOoutputs)) of
                                                    { ___node ->
                                                    ( _lhsOerrors,_lhsOfields,_lhsOinputs,_lhsOoutput,_lhsOoutputs) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
sem_Children_Nil :: T_Children
sem_Children_Nil =
    (T_Children (\ (!_lhsIcon)
                   (!_lhsIcr)
                   (!_lhsIinhMap)
                   (!_lhsImerged)
                   (!_lhsInt)
                   (!_lhsIparams)
                   (!_lhsIsynMap) ->
                     (case (({-# LINE 142 "src-ag/DefaultRules.ag" #-}
                             Seq.empty
                             {-# LINE 726 "dist/build/DefaultRules" #-}
                             )) of
                      { !_lhsOerrors ->
                      (case (({-# LINE 512 "src-ag/DefaultRules.ag" #-}
                              []
                              {-# LINE 731 "dist/build/DefaultRules" #-}
                              )) of
                       { !_lhsOfields ->
                       (case (({-# LINE 191 "src-ag/DefaultRules.ag" #-}
                               []
                               {-# LINE 736 "dist/build/DefaultRules" #-}
                               )) of
                        { !_lhsOinputs ->
                        (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                []
                                {-# LINE 741 "dist/build/DefaultRules" #-}
                                )) of
                         { !_output ->
                         (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                 _output
                                 {-# LINE 746 "dist/build/DefaultRules" #-}
                                 )) of
                          { !_lhsOoutput ->
                          (case (({-# LINE 192 "src-ag/DefaultRules.ag" #-}
                                  []
                                  {-# LINE 751 "dist/build/DefaultRules" #-}
                                  )) of
                           { !_lhsOoutputs ->
                           (case ((Syn_Children _lhsOerrors _lhsOfields _lhsOinputs _lhsOoutput _lhsOoutputs)) of
                            { ___node ->
                            ( _lhsOerrors,_lhsOfields,_lhsOinputs,_lhsOoutput,_lhsOoutputs) }) }) }) }) }) }) })))
-- Grammar -----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attributes:
         errors               : Seq Error
         output               : Grammar 
   alternatives:
      alternative Grammar:
         child typeSyns       : {TypeSyns}
         child useMap         : {UseMap}
         child derivings      : {Derivings}
         child wrappers       : {Set NontermIdent}
         child nonts          : Nonterminals 
         child pragmas        : {PragmaMap}
         child manualAttrOrderMap : {AttrOrderMap}
         child paramMap       : {ParamMap}
         child contextMap     : {ContextMap}
         child quantMap       : {QuantMap}
         child uniqueMap      : {UniqueMap}
         child augmentsMap    : {Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))}
         child aroundsMap     : {Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))}
         child mergeMap       : {Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))}
         visit 0:
            local output      : _
-}
-- cata
sem_Grammar :: Grammar ->
               T_Grammar
sem_Grammar !(Grammar _typeSyns _useMap _derivings _wrappers _nonts _pragmas _manualAttrOrderMap _paramMap _contextMap _quantMap _uniqueMap _augmentsMap _aroundsMap _mergeMap) =
    (sem_Grammar_Grammar _typeSyns _useMap _derivings _wrappers (sem_Nonterminals _nonts) _pragmas _manualAttrOrderMap _paramMap _contextMap _quantMap _uniqueMap _augmentsMap _aroundsMap _mergeMap)
-- semantic domain
newtype T_Grammar = T_Grammar (Options ->
                               ( (Seq Error),Grammar))
data Inh_Grammar = Inh_Grammar {options_Inh_Grammar :: !(Options)}
data Syn_Grammar = Syn_Grammar {errors_Syn_Grammar :: !((Seq Error)),output_Syn_Grammar :: !(Grammar)}
wrap_Grammar :: T_Grammar ->
                Inh_Grammar ->
                Syn_Grammar
wrap_Grammar !(T_Grammar sem) !(Inh_Grammar _lhsIoptions) =
    (let ( !_lhsOerrors,!_lhsOoutput) = sem _lhsIoptions
     in  (Syn_Grammar _lhsOerrors _lhsOoutput))
sem_Grammar_Grammar :: TypeSyns ->
                       UseMap ->
                       Derivings ->
                       (Set NontermIdent) ->
                       T_Nonterminals ->
                       PragmaMap ->
                       AttrOrderMap ->
                       ParamMap ->
                       ContextMap ->
                       QuantMap ->
                       UniqueMap ->
                       (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                       (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                       (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier, [Identifier], Expression)))) ->
                       T_Grammar
sem_Grammar_Grammar !typeSyns_ !useMap_ !derivings_ !wrappers_ !(T_Nonterminals nonts_) !pragmas_ !manualAttrOrderMap_ !paramMap_ !contextMap_ !quantMap_ !uniqueMap_ !augmentsMap_ !aroundsMap_ !mergeMap_ =
    (T_Grammar (\ (!_lhsIoptions) ->
                    (case (({-# LINE 50 "src-ag/DefaultRules.ag" #-}
                            _lhsIoptions
                            {-# LINE 819 "dist/build/DefaultRules" #-}
                            )) of
                     { !_nontsOoptions ->
                     (case (nonts_) of
                      { ( !_nontsIcollect_nts,!_nontsIinhMap',!_nontsIsynMap',!T_Nonterminals_1 nonts_1) ->
                          (case (({-# LINE 16 "src-ag/DistChildAttr.ag" #-}
                                  _nontsIsynMap'
                                  {-# LINE 826 "dist/build/DefaultRules" #-}
                                  )) of
                           { !_nontsOsynMap ->
                           (case (({-# LINE 15 "src-ag/DistChildAttr.ag" #-}
                                   _nontsIinhMap'
                                   {-# LINE 831 "dist/build/DefaultRules" #-}
                                   )) of
                            { !_nontsOinhMap ->
                            (case (({-# LINE 759 "src-ag/DefaultRules.ag" #-}
                                    mergeMap_
                                    {-# LINE 836 "dist/build/DefaultRules" #-}
                                    )) of
                             { !_nontsOmergesIn ->
                             (case (({-# LINE 678 "src-ag/DefaultRules.ag" #-}
                                     manualAttrOrderMap_
                                     {-# LINE 841 "dist/build/DefaultRules" #-}
                                     )) of
                              { !_nontsOmanualAttrOrderMap ->
                              (case (({-# LINE 178 "src-ag/DefaultRules.ag" #-}
                                      typeSyns_
                                      {-# LINE 846 "dist/build/DefaultRules" #-}
                                      )) of
                               { !_nontsOtypeSyns ->
                               (case (({-# LINE 176 "src-ag/DefaultRules.ag" #-}
                                       useMap_
                                       {-# LINE 851 "dist/build/DefaultRules" #-}
                                       )) of
                                { !_nontsOuseMap ->
                                (case (({-# LINE 66 "src-ag/DefaultRules.ag" #-}
                                        wrappers_
                                        {-# LINE 856 "dist/build/DefaultRules" #-}
                                        )) of
                                 { !_nontsOwrappers ->
                                 (case (({-# LINE 58 "src-ag/DefaultRules.ag" #-}
                                         modcopy   _lhsIoptions
                                         {-# LINE 861 "dist/build/DefaultRules" #-}
                                         )) of
                                  { !_nontsOcr ->
                                  (case (({-# LINE 57 "src-ag/DefaultRules.ag" #-}
                                          rename    _lhsIoptions
                                          {-# LINE 866 "dist/build/DefaultRules" #-}
                                          )) of
                                   { !_nontsOo_rename ->
                                   (case (({-# LINE 751 "src-ag/DefaultRules.ag" #-}
                                           aroundsMap_
                                           {-# LINE 871 "dist/build/DefaultRules" #-}
                                           )) of
                                    { !_nontsOaroundsIn ->
                                    (case (({-# LINE 744 "src-ag/DefaultRules.ag" #-}
                                            augmentsMap_
                                            {-# LINE 876 "dist/build/DefaultRules" #-}
                                            )) of
                                     { !_nontsOaugmentsIn ->
                                     (case (({-# LINE 564 "src-ag/DefaultRules.ag" #-}
                                             1
                                             {-# LINE 881 "dist/build/DefaultRules" #-}
                                             )) of
                                      { !_nontsOuniq ->
                                      (case (({-# LINE 154 "src-ag/DefaultRules.ag" #-}
                                              _nontsIcollect_nts
                                              {-# LINE 886 "dist/build/DefaultRules" #-}
                                              )) of
                                       { !_nontsOnonterminals ->
                                       (case (nonts_1 _nontsOaroundsIn _nontsOaugmentsIn _nontsOcr _nontsOinhMap _nontsOmanualAttrOrderMap _nontsOmergesIn _nontsOnonterminals _nontsOo_rename _nontsOoptions _nontsOsynMap _nontsOtypeSyns _nontsOuniq _nontsOuseMap _nontsOwrappers) of
                                        { ( !_nontsIerrors,!_nontsIoutput,!_nontsIuniq) ->
                                            (case (({-# LINE 142 "src-ag/DefaultRules.ag" #-}
                                                    _nontsIerrors
                                                    {-# LINE 893 "dist/build/DefaultRules" #-}
                                                    )) of
                                             { !_lhsOerrors ->
                                             (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                                     Grammar typeSyns_ useMap_ derivings_ wrappers_ _nontsIoutput pragmas_ manualAttrOrderMap_ paramMap_ contextMap_ quantMap_ uniqueMap_ augmentsMap_ aroundsMap_ mergeMap_
                                                     {-# LINE 898 "dist/build/DefaultRules" #-}
                                                     )) of
                                              { !_output ->
                                              (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                                      _output
                                                      {-# LINE 903 "dist/build/DefaultRules" #-}
                                                      )) of
                                               { !_lhsOoutput ->
                                               (case ((Syn_Grammar _lhsOerrors _lhsOoutput)) of
                                                { ___node ->
                                                ( _lhsOerrors,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
-- Nonterminal -------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         collect_nts          : Set NontermIdent
         inhMap'              : Map Identifier Attributes
         synMap'              : Map Identifier Attributes
   visit 1:
      inherited attributes:
         aroundsIn            : Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         augmentsIn           : Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         cr                   : Bool
         inhMap               : Map Identifier Attributes
         manualAttrOrderMap   : AttrOrderMap
         mergesIn             : Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))
         nonterminals         : Set NontermIdent
         o_rename             : Bool
         options              : Options
         synMap               : Map Identifier Attributes
         typeSyns             : TypeSyns
         useMap               : UseMap
         wrappers             : Set NontermIdent
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         errors               : Seq Error
         output               : Nonterminal 
   alternatives:
      alternative Nonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child prods          : Productions 
         visit 1:
            local mergesIn    : _
            local syn1        : _
            local inh1        : _
            local augmentsIn  : _
            local aroundsIn   : _
-}
-- cata
sem_Nonterminal :: Nonterminal ->
                   T_Nonterminal
sem_Nonterminal !(Nonterminal _nt _params _inh _syn _prods) =
    (sem_Nonterminal_Nonterminal _nt _params _inh _syn (sem_Productions _prods))
-- semantic domain
newtype T_Nonterminal = T_Nonterminal (( (Set NontermIdent),(Map Identifier Attributes),(Map Identifier Attributes),T_Nonterminal_1))
newtype T_Nonterminal_1 = T_Nonterminal_1 ((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                                           (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                                           Bool ->
                                           (Map Identifier Attributes) ->
                                           AttrOrderMap ->
                                           (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))) ->
                                           (Set NontermIdent) ->
                                           Bool ->
                                           Options ->
                                           (Map Identifier Attributes) ->
                                           TypeSyns ->
                                           Int ->
                                           UseMap ->
                                           (Set NontermIdent) ->
                                           ( (Seq Error),Nonterminal,Int))
data Inh_Nonterminal = Inh_Nonterminal {aroundsIn_Inh_Nonterminal :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))),augmentsIn_Inh_Nonterminal :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))),cr_Inh_Nonterminal :: !(Bool),inhMap_Inh_Nonterminal :: !((Map Identifier Attributes)),manualAttrOrderMap_Inh_Nonterminal :: !(AttrOrderMap),mergesIn_Inh_Nonterminal :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))))),nonterminals_Inh_Nonterminal :: !((Set NontermIdent)),o_rename_Inh_Nonterminal :: !(Bool),options_Inh_Nonterminal :: !(Options),synMap_Inh_Nonterminal :: !((Map Identifier Attributes)),typeSyns_Inh_Nonterminal :: !(TypeSyns),uniq_Inh_Nonterminal :: !(Int),useMap_Inh_Nonterminal :: !(UseMap),wrappers_Inh_Nonterminal :: !((Set NontermIdent))}
data Syn_Nonterminal = Syn_Nonterminal {collect_nts_Syn_Nonterminal :: !((Set NontermIdent)),errors_Syn_Nonterminal :: !((Seq Error)),inhMap'_Syn_Nonterminal :: !((Map Identifier Attributes)),output_Syn_Nonterminal :: !(Nonterminal),synMap'_Syn_Nonterminal :: !((Map Identifier Attributes)),uniq_Syn_Nonterminal :: !(Int)}
wrap_Nonterminal :: T_Nonterminal ->
                    Inh_Nonterminal ->
                    Syn_Nonterminal
wrap_Nonterminal !(T_Nonterminal sem) !(Inh_Nonterminal _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) =
    (let ( !_lhsOcollect_nts,!_lhsOinhMap',!_lhsOsynMap',!T_Nonterminal_1 sem_1) = sem
         ( !_lhsOerrors,!_lhsOoutput,!_lhsOuniq) = sem_1 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers
     in  (Syn_Nonterminal _lhsOcollect_nts _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap' _lhsOuniq))
sem_Nonterminal_Nonterminal :: NontermIdent ->
                               ([Identifier]) ->
                               Attributes ->
                               Attributes ->
                               T_Productions ->
                               T_Nonterminal
sem_Nonterminal_Nonterminal !nt_ !params_ !inh_ !syn_ !(T_Productions prods_) =
    (T_Nonterminal (case (({-# LINE 150 "src-ag/DefaultRules.ag" #-}
                           Set.singleton nt_
                           {-# LINE 990 "dist/build/DefaultRules" #-}
                           )) of
                    { !_lhsOcollect_nts ->
                    (case (({-# LINE 7 "src-ag/DistChildAttr.ag" #-}
                            Map.singleton nt_ inh_
                            {-# LINE 995 "dist/build/DefaultRules" #-}
                            )) of
                     { !_lhsOinhMap' ->
                     (case (({-# LINE 8 "src-ag/DistChildAttr.ag" #-}
                             Map.singleton nt_ syn_
                             {-# LINE 1000 "dist/build/DefaultRules" #-}
                             )) of
                      { !_lhsOsynMap' ->
                      (case ((let sem_Nonterminal_Nonterminal_1 :: T_Nonterminal_1
                                  sem_Nonterminal_Nonterminal_1 =
                                      (T_Nonterminal_1 (\ (!_lhsIaroundsIn)
                                                          (!_lhsIaugmentsIn)
                                                          (!_lhsIcr)
                                                          (!_lhsIinhMap)
                                                          (!_lhsImanualAttrOrderMap)
                                                          (!_lhsImergesIn)
                                                          (!_lhsInonterminals)
                                                          (!_lhsIo_rename)
                                                          (!_lhsIoptions)
                                                          (!_lhsIsynMap)
                                                          (!_lhsItypeSyns)
                                                          (!_lhsIuniq)
                                                          (!_lhsIuseMap)
                                                          (!_lhsIwrappers) ->
                                                            (case (({-# LINE 64 "src-ag/DefaultRules.ag" #-}
                                                                    _lhsIwrappers
                                                                    {-# LINE 1021 "dist/build/DefaultRules" #-}
                                                                    )) of
                                                             { !_prodsOwrappers ->
                                                             (case (({-# LINE 71 "src-ag/DefaultRules.ag" #-}
                                                                     _lhsItypeSyns
                                                                     {-# LINE 1026 "dist/build/DefaultRules" #-}
                                                                     )) of
                                                              { !_prodsOtypeSyns ->
                                                              (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                                                      _lhsIsynMap
                                                                      {-# LINE 1031 "dist/build/DefaultRules" #-}
                                                                      )) of
                                                               { !_prodsOsynMap ->
                                                               (case (({-# LINE 50 "src-ag/DefaultRules.ag" #-}
                                                                       _lhsIoptions
                                                                       {-# LINE 1036 "dist/build/DefaultRules" #-}
                                                                       )) of
                                                                { !_prodsOoptions ->
                                                                (case (({-# LINE 51 "src-ag/DefaultRules.ag" #-}
                                                                        _lhsIo_rename
                                                                        {-# LINE 1041 "dist/build/DefaultRules" #-}
                                                                        )) of
                                                                 { !_prodsOo_rename ->
                                                                 (case (({-# LINE 760 "src-ag/DefaultRules.ag" #-}
                                                                         Map.findWithDefault Map.empty nt_ _lhsImergesIn
                                                                         {-# LINE 1046 "dist/build/DefaultRules" #-}
                                                                         )) of
                                                                  { !_mergesIn ->
                                                                  (case (({-# LINE 756 "src-ag/DefaultRules.ag" #-}
                                                                          _mergesIn
                                                                          {-# LINE 1051 "dist/build/DefaultRules" #-}
                                                                          )) of
                                                                   { !_prodsOmergesIn ->
                                                                   (case (({-# LINE 674 "src-ag/DefaultRules.ag" #-}
                                                                           _lhsImanualAttrOrderMap
                                                                           {-# LINE 1056 "dist/build/DefaultRules" #-}
                                                                           )) of
                                                                    { !_prodsOmanualAttrOrderMap ->
                                                                    (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                                                            _lhsIinhMap
                                                                            {-# LINE 1061 "dist/build/DefaultRules" #-}
                                                                            )) of
                                                                     { !_prodsOinhMap ->
                                                                     (case (({-# LINE 52 "src-ag/DefaultRules.ag" #-}
                                                                             _lhsIcr
                                                                             {-# LINE 1066 "dist/build/DefaultRules" #-}
                                                                             )) of
                                                                      { !_prodsOcr ->
                                                                      (case (({-# LINE 534 "src-ag/DefaultRules.ag" #-}
                                                                              Map.map (elimSelfId nt_ params_) syn_
                                                                              {-# LINE 1071 "dist/build/DefaultRules" #-}
                                                                              )) of
                                                                       { !_syn1 ->
                                                                       (case (({-# LINE 533 "src-ag/DefaultRules.ag" #-}
                                                                               Map.map (elimSelfId nt_ params_) inh_
                                                                               {-# LINE 1076 "dist/build/DefaultRules" #-}
                                                                               )) of
                                                                        { !_inh1 ->
                                                                        (case (({-# LINE 180 "src-ag/DefaultRules.ag" #-}
                                                                                nt_
                                                                                {-# LINE 1081 "dist/build/DefaultRules" #-}
                                                                                )) of
                                                                         { !_prodsOnt ->
                                                                         (case (({-# LINE 168 "src-ag/DefaultRules.ag" #-}
                                                                                 Map.findWithDefault Map.empty nt_ _lhsIuseMap
                                                                                 {-# LINE 1086 "dist/build/DefaultRules" #-}
                                                                                 )) of
                                                                          { !_prodsOuseMap ->
                                                                          (case (({-# LINE 167 "src-ag/DefaultRules.ag" #-}
                                                                                  syn_
                                                                                  {-# LINE 1091 "dist/build/DefaultRules" #-}
                                                                                  )) of
                                                                           { !_prodsOsynOrig ->
                                                                           (case (({-# LINE 165 "src-ag/DefaultRules.ag" #-}
                                                                                   _syn1
                                                                                   {-# LINE 1096 "dist/build/DefaultRules" #-}
                                                                                   )) of
                                                                            { !_prodsOsyn ->
                                                                            (case (({-# LINE 164 "src-ag/DefaultRules.ag" #-}
                                                                                    _inh1
                                                                                    {-# LINE 1101 "dist/build/DefaultRules" #-}
                                                                                    )) of
                                                                             { !_prodsOinh ->
                                                                             (case (({-# LINE 561 "src-ag/DefaultRules.ag" #-}
                                                                                     _lhsIuniq
                                                                                     {-# LINE 1106 "dist/build/DefaultRules" #-}
                                                                                     )) of
                                                                              { !_prodsOuniq ->
                                                                              (case (({-# LINE 152 "src-ag/DefaultRules.ag" #-}
                                                                                      _lhsInonterminals
                                                                                      {-# LINE 1111 "dist/build/DefaultRules" #-}
                                                                                      )) of
                                                                               { !_prodsOnonterminals ->
                                                                               (case (({-# LINE 745 "src-ag/DefaultRules.ag" #-}
                                                                                       Map.findWithDefault Map.empty nt_ _lhsIaugmentsIn
                                                                                       {-# LINE 1116 "dist/build/DefaultRules" #-}
                                                                                       )) of
                                                                                { !_augmentsIn ->
                                                                                (case (({-# LINE 742 "src-ag/DefaultRules.ag" #-}
                                                                                        _augmentsIn
                                                                                        {-# LINE 1121 "dist/build/DefaultRules" #-}
                                                                                        )) of
                                                                                 { !_prodsOaugmentsIn ->
                                                                                 (case (({-# LINE 752 "src-ag/DefaultRules.ag" #-}
                                                                                         Map.findWithDefault Map.empty nt_ _lhsIaroundsIn
                                                                                         {-# LINE 1126 "dist/build/DefaultRules" #-}
                                                                                         )) of
                                                                                  { !_aroundsIn ->
                                                                                  (case (({-# LINE 749 "src-ag/DefaultRules.ag" #-}
                                                                                          _aroundsIn
                                                                                          {-# LINE 1131 "dist/build/DefaultRules" #-}
                                                                                          )) of
                                                                                   { !_prodsOaroundsIn ->
                                                                                   (case (({-# LINE 166 "src-ag/DefaultRules.ag" #-}
                                                                                           inh_
                                                                                           {-# LINE 1136 "dist/build/DefaultRules" #-}
                                                                                           )) of
                                                                                    { !_prodsOinhOrig ->
                                                                                    (case (({-# LINE 43 "src-ag/DefaultRules.ag" #-}
                                                                                            params_
                                                                                            {-# LINE 1141 "dist/build/DefaultRules" #-}
                                                                                            )) of
                                                                                     { !_prodsOparams ->
                                                                                     (case (prods_ _prodsOaroundsIn _prodsOaugmentsIn _prodsOcr _prodsOinh _prodsOinhMap _prodsOinhOrig _prodsOmanualAttrOrderMap _prodsOmergesIn _prodsOnonterminals _prodsOnt _prodsOo_rename _prodsOoptions _prodsOparams _prodsOsyn _prodsOsynMap _prodsOsynOrig _prodsOtypeSyns _prodsOuniq _prodsOuseMap _prodsOwrappers) of
                                                                                      { ( !_prodsIerrors,!_prodsIoutput,!_prodsIuniq) ->
                                                                                          (case (({-# LINE 142 "src-ag/DefaultRules.ag" #-}
                                                                                                  _prodsIerrors
                                                                                                  {-# LINE 1148 "dist/build/DefaultRules" #-}
                                                                                                  )) of
                                                                                           { !_lhsOerrors ->
                                                                                           (case (({-# LINE 573 "src-ag/DefaultRules.ag" #-}
                                                                                                   Nonterminal nt_ params_ _inh1     _syn1     _prodsIoutput
                                                                                                   {-# LINE 1153 "dist/build/DefaultRules" #-}
                                                                                                   )) of
                                                                                            { !_lhsOoutput ->
                                                                                            (case (({-# LINE 561 "src-ag/DefaultRules.ag" #-}
                                                                                                    _prodsIuniq
                                                                                                    {-# LINE 1158 "dist/build/DefaultRules" #-}
                                                                                                    )) of
                                                                                             { !_lhsOuniq ->
                                                                                             ( _lhsOerrors,_lhsOoutput,_lhsOuniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
                              in  sem_Nonterminal_Nonterminal_1)) of
                       { ( !sem_Nonterminal_1) ->
                       ( _lhsOcollect_nts,_lhsOinhMap',_lhsOsynMap',sem_Nonterminal_1) }) }) }) }))
-- Nonterminals ------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         collect_nts          : Set NontermIdent
         inhMap'              : Map Identifier Attributes
         synMap'              : Map Identifier Attributes
   visit 1:
      inherited attributes:
         aroundsIn            : Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         augmentsIn           : Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))
         cr                   : Bool
         inhMap               : Map Identifier Attributes
         manualAttrOrderMap   : AttrOrderMap
         mergesIn             : Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))
         nonterminals         : Set NontermIdent
         o_rename             : Bool
         options              : Options
         synMap               : Map Identifier Attributes
         typeSyns             : TypeSyns
         useMap               : UseMap
         wrappers             : Set NontermIdent
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         errors               : Seq Error
         output               : Nonterminals 
   alternatives:
      alternative Cons:
         child hd             : Nonterminal 
         child tl             : Nonterminals 
         visit 1:
            local output      : _
      alternative Nil:
         visit 1:
            local output      : _
-}
-- cata
sem_Nonterminals :: Nonterminals ->
                    T_Nonterminals
sem_Nonterminals !list =
    (Prelude.foldr sem_Nonterminals_Cons sem_Nonterminals_Nil (Prelude.map sem_Nonterminal list))
-- semantic domain
newtype T_Nonterminals = T_Nonterminals (( (Set NontermIdent),(Map Identifier Attributes),(Map Identifier Attributes),T_Nonterminals_1))
newtype T_Nonterminals_1 = T_Nonterminals_1 ((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                                             (Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression]))) ->
                                             Bool ->
                                             (Map Identifier Attributes) ->
                                             AttrOrderMap ->
                                             (Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))) ->
                                             (Set NontermIdent) ->
                                             Bool ->
                                             Options ->
                                             (Map Identifier Attributes) ->
                                             TypeSyns ->
                                             Int ->
                                             UseMap ->
                                             (Set NontermIdent) ->
                                             ( (Seq Error),Nonterminals,Int))
data Inh_Nonterminals = Inh_Nonterminals {aroundsIn_Inh_Nonterminals :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))),augmentsIn_Inh_Nonterminals :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier [Expression])))),cr_Inh_Nonterminals :: !(Bool),inhMap_Inh_Nonterminals :: !((Map Identifier Attributes)),manualAttrOrderMap_Inh_Nonterminals :: !(AttrOrderMap),mergesIn_Inh_Nonterminals :: !((Map NontermIdent (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))))),nonterminals_Inh_Nonterminals :: !((Set NontermIdent)),o_rename_Inh_Nonterminals :: !(Bool),options_Inh_Nonterminals :: !(Options),synMap_Inh_Nonterminals :: !((Map Identifier Attributes)),typeSyns_Inh_Nonterminals :: !(TypeSyns),uniq_Inh_Nonterminals :: !(Int),useMap_Inh_Nonterminals :: !(UseMap),wrappers_Inh_Nonterminals :: !((Set NontermIdent))}
data Syn_Nonterminals = Syn_Nonterminals {collect_nts_Syn_Nonterminals :: !((Set NontermIdent)),errors_Syn_Nonterminals :: !((Seq Error)),inhMap'_Syn_Nonterminals :: !((Map Identifier Attributes)),output_Syn_Nonterminals :: !(Nonterminals),synMap'_Syn_Nonterminals :: !((Map Identifier Attributes)),uniq_Syn_Nonterminals :: !(Int)}
wrap_Nonterminals :: T_Nonterminals ->
                     Inh_Nonterminals ->
                     Syn_Nonterminals
wrap_Nonterminals !(T_Nonterminals sem) !(Inh_Nonterminals _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) =
    (let ( !_lhsOcollect_nts,!_lhsOinhMap',!_lhsOsynMap',!T_Nonterminals_1 sem_1) = sem
         ( !_lhsOerrors,!_lhsOoutput,!_lhsOuniq) = sem_1 _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinhMap _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsIo_rename _lhsIoptions _lhsIsynMap _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers
     in  (Syn_Nonterminals _lhsOcollect_nts _lhsOerrors _lhsOinhMap' _lhsOoutput _lhsOsynMap' _lhsOuniq))
sem_Nonterminals_Cons :: T_Nonterminal ->
                         T_Nonterminals ->
                         T_Nonterminals
sem_Nonterminals_Cons !(T_Nonterminal hd_) !(T_Nonterminals tl_) =
    (T_Nonterminals (case (tl_) of
                     { ( !_tlIcollect_nts,!_tlIinhMap',!_tlIsynMap',!T_Nonterminals_1 tl_1) ->
                         (case (hd_) of
                          { ( !_hdIcollect_nts,!_hdIinhMap',!_hdIsynMap',!T_Nonterminal_1 hd_1) ->
                              (case (({-# LINE 148 "src-ag/DefaultRules.ag" #-}
                                      _hdIcollect_nts `Set.union` _tlIcollect_nts
                                      {-# LINE 1243 "dist/build/DefaultRules" #-}
                                      )) of
                               { !_lhsOcollect_nts ->
                               (case (({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                       _hdIinhMap' `Map.union` _tlIinhMap'
                                       {-# LINE 1248 "dist/build/DefaultRules" #-}
                                       )) of
                                { !_lhsOinhMap' ->
                                (case (({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                                        _hdIsynMap' `Map.union` _tlIsynMap'
                                        {-# LINE 1253 "dist/build/DefaultRules" #-}
                                        )) of
                                 { !_lhsOsynMap' ->
                                 (case ((let sem_Nonterminals_Cons_1 :: T_Nonterminals_1
                                             sem_Nonterminals_Cons_1 =
                                                 (T_Nonterminals_1 (\ (!_lhsIaroundsIn)
                                                                      (!_lhsIaugmentsIn)
                                                                      (!_lhsIcr)
                                                                      (!_lhsIinhMap)
                                                                      (!_lhsImanualAttrOrderMap)
                                                                      (!_lhsImergesIn)
                                                                      (!_lhsInonterminals)
                                                                      (!_lhsIo_rename)
                                                                      (!_lhsIoptions)
                                                                      (!_lhsIsynMap)
                                                                      (!_lhsItypeSyns)
                                                                      (!_lhsIuniq)
                                                                      (!_lhsIuseMap)
                                                                      (!_lhsIwrappers) ->
                                                                        (case (({-# LINE 64 "src-ag/DefaultRules.ag" #-}
                                                                                _lhsIwrappers
                                                                                {-# LINE 1274 "dist/build/DefaultRules" #-}
                                                                                )) of
                                                                         { !_tlOwrappers ->
                                                                         (case (({-# LINE 160 "src-ag/DefaultRules.ag" #-}
                                                                                 _lhsIuseMap
                                                                                 {-# LINE 1279 "dist/build/DefaultRules" #-}
                                                                                 )) of
                                                                          { !_tlOuseMap ->
                                                                          (case (({-# LINE 71 "src-ag/DefaultRules.ag" #-}
                                                                                  _lhsItypeSyns
                                                                                  {-# LINE 1284 "dist/build/DefaultRules" #-}
                                                                                  )) of
                                                                           { !_tlOtypeSyns ->
                                                                           (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                                                                   _lhsIsynMap
                                                                                   {-# LINE 1289 "dist/build/DefaultRules" #-}
                                                                                   )) of
                                                                            { !_tlOsynMap ->
                                                                            (case (({-# LINE 50 "src-ag/DefaultRules.ag" #-}
                                                                                    _lhsIoptions
                                                                                    {-# LINE 1294 "dist/build/DefaultRules" #-}
                                                                                    )) of
                                                                             { !_tlOoptions ->
                                                                             (case (({-# LINE 51 "src-ag/DefaultRules.ag" #-}
                                                                                     _lhsIo_rename
                                                                                     {-# LINE 1299 "dist/build/DefaultRules" #-}
                                                                                     )) of
                                                                              { !_tlOo_rename ->
                                                                              (case (({-# LINE 755 "src-ag/DefaultRules.ag" #-}
                                                                                      _lhsImergesIn
                                                                                      {-# LINE 1304 "dist/build/DefaultRules" #-}
                                                                                      )) of
                                                                               { !_tlOmergesIn ->
                                                                               (case (({-# LINE 674 "src-ag/DefaultRules.ag" #-}
                                                                                       _lhsImanualAttrOrderMap
                                                                                       {-# LINE 1309 "dist/build/DefaultRules" #-}
                                                                                       )) of
                                                                                { !_tlOmanualAttrOrderMap ->
                                                                                (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                                                                        _lhsIinhMap
                                                                                        {-# LINE 1314 "dist/build/DefaultRules" #-}
                                                                                        )) of
                                                                                 { !_tlOinhMap ->
                                                                                 (case (({-# LINE 52 "src-ag/DefaultRules.ag" #-}
                                                                                         _lhsIcr
                                                                                         {-# LINE 1319 "dist/build/DefaultRules" #-}
                                                                                         )) of
                                                                                  { !_tlOcr ->
                                                                                  (case (({-# LINE 64 "src-ag/DefaultRules.ag" #-}
                                                                                          _lhsIwrappers
                                                                                          {-# LINE 1324 "dist/build/DefaultRules" #-}
                                                                                          )) of
                                                                                   { !_hdOwrappers ->
                                                                                   (case (({-# LINE 160 "src-ag/DefaultRules.ag" #-}
                                                                                           _lhsIuseMap
                                                                                           {-# LINE 1329 "dist/build/DefaultRules" #-}
                                                                                           )) of
                                                                                    { !_hdOuseMap ->
                                                                                    (case (({-# LINE 71 "src-ag/DefaultRules.ag" #-}
                                                                                            _lhsItypeSyns
                                                                                            {-# LINE 1334 "dist/build/DefaultRules" #-}
                                                                                            )) of
                                                                                     { !_hdOtypeSyns ->
                                                                                     (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                                                                             _lhsIsynMap
                                                                                             {-# LINE 1339 "dist/build/DefaultRules" #-}
                                                                                             )) of
                                                                                      { !_hdOsynMap ->
                                                                                      (case (({-# LINE 50 "src-ag/DefaultRules.ag" #-}
                                                                                              _lhsIoptions
                                                                                              {-# LINE 1344 "dist/build/DefaultRules" #-}
                                                                                              )) of
                                                                                       { !_hdOoptions ->
                                                                                       (case (({-# LINE 51 "src-ag/DefaultRules.ag" #-}
                                                                                               _lhsIo_rename
                                                                                               {-# LINE 1349 "dist/build/DefaultRules" #-}
                                                                                               )) of
                                                                                        { !_hdOo_rename ->
                                                                                        (case (({-# LINE 755 "src-ag/DefaultRules.ag" #-}
                                                                                                _lhsImergesIn
                                                                                                {-# LINE 1354 "dist/build/DefaultRules" #-}
                                                                                                )) of
                                                                                         { !_hdOmergesIn ->
                                                                                         (case (({-# LINE 674 "src-ag/DefaultRules.ag" #-}
                                                                                                 _lhsImanualAttrOrderMap
                                                                                                 {-# LINE 1359 "dist/build/DefaultRules" #-}
                                                                                                 )) of
                                                                                          { !_hdOmanualAttrOrderMap ->
                                                                                          (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                                                                                  _lhsIinhMap
                                                                                                  {-# LINE 1364 "dist/build/DefaultRules" #-}
                                                                                                  )) of
                                                                                           { !_hdOinhMap ->
                                                                                           (case (({-# LINE 52 "src-ag/DefaultRules.ag" #-}
                                                                                                   _lhsIcr
                                                                                                   {-# LINE 1369 "dist/build/DefaultRules" #-}
                                                                                                   )) of
                                                                                            { !_hdOcr ->
                                                                                            (case (({-# LINE 561 "src-ag/DefaultRules.ag" #-}
                                                                                                    _lhsIuniq
                                                                                                    {-# LINE 1374 "dist/build/DefaultRules" #-}
                                                                                                    )) of
                                                                                             { !_hdOuniq ->
                                                                                             (case (({-# LINE 152 "src-ag/DefaultRules.ag" #-}
                                                                                                     _lhsInonterminals
                                                                                                     {-# LINE 1379 "dist/build/DefaultRules" #-}
                                                                                                     )) of
                                                                                              { !_hdOnonterminals ->
                                                                                              (case (({-# LINE 741 "src-ag/DefaultRules.ag" #-}
                                                                                                      _lhsIaugmentsIn
                                                                                                      {-# LINE 1384 "dist/build/DefaultRules" #-}
                                                                                                      )) of
                                                                                               { !_hdOaugmentsIn ->
                                                                                               (case (({-# LINE 748 "src-ag/DefaultRules.ag" #-}
                                                                                                       _lhsIaroundsIn
                                                                                                       {-# LINE 1389 "dist/build/DefaultRules" #-}
                                                                                                       )) of
                                                                                                { !_hdOaroundsIn ->
                                                                                                (case (hd_1 _hdOaroundsIn _hdOaugmentsIn _hdOcr _hdOinhMap _hdOmanualAttrOrderMap _hdOmergesIn _hdOnonterminals _hdOo_rename _hdOoptions _hdOsynMap _hdOtypeSyns _hdOuniq _hdOuseMap _hdOwrappers) of
                                                                                                 { ( !_hdIerrors,!_hdIoutput,!_hdIuniq) ->
                                                                                                     (case (({-# LINE 561 "src-ag/DefaultRules.ag" #-}
                                                                                                             _hdIuniq
                                                                                                             {-# LINE 1396 "dist/build/DefaultRules" #-}
                                                                                                             )) of
                                                                                                      { !_tlOuniq ->
                                                                                                      (case (({-# LINE 152 "src-ag/DefaultRules.ag" #-}
                                                                                                              _lhsInonterminals
                                                                                                              {-# LINE 1401 "dist/build/DefaultRules" #-}
                                                                                                              )) of
                                                                                                       { !_tlOnonterminals ->
                                                                                                       (case (({-# LINE 741 "src-ag/DefaultRules.ag" #-}
                                                                                                               _lhsIaugmentsIn
                                                                                                               {-# LINE 1406 "dist/build/DefaultRules" #-}
                                                                                                               )) of
                                                                                                        { !_tlOaugmentsIn ->
                                                                                                        (case (({-# LINE 748 "src-ag/DefaultRules.ag" #-}
                                                                                                                _lhsIaroundsIn
                                                                                                                {-# LINE 1411 "dist/build/DefaultRules" #-}
                                                                                                                )) of
                                                                                                         { !_tlOaroundsIn ->
                                                                                                         (case (tl_1 _tlOaroundsIn _tlOaugmentsIn _tlOcr _tlOinhMap _tlOmanualAttrOrderMap _tlOmergesIn _tlOnonterminals _tlOo_rename _tlOoptions _tlOsynMap _tlOtypeSyns _tlOuniq _tlOuseMap _tlOwrappers) of
                                                                                                          { ( !_tlIerrors,!_tlIoutput,!_tlIuniq) ->
                                                                                                              (case (({-# LINE 142 "src-ag/DefaultRules.ag" #-}
                                                                                                                      _hdIerrors Seq.>< _tlIerrors
                                                                                                                      {-# LINE 1418 "dist/build/DefaultRules" #-}
                                                                                                                      )) of
                                                                                                               { !_lhsOerrors ->
                                                                                                               (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                                                                                                       (:) _hdIoutput _tlIoutput
                                                                                                                       {-# LINE 1423 "dist/build/DefaultRules" #-}
                                                                                                                       )) of
                                                                                                                { !_output ->
                                                                                                                (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                                                                                                        _output
                                                                                                                        {-# LINE 1428 "dist/build/DefaultRules" #-}
                                                                                                                        )) of
                                                                                                                 { !_lhsOoutput ->
                                                                                                                 (case (({-# LINE 561 "src-ag/DefaultRules.ag" #-}
                                                                                                                         _tlIuniq
                                                                                                                         {-# LINE 1433 "dist/build/DefaultRules" #-}
                                                                                                                         )) of
                                                                                                                  { !_lhsOuniq ->
                                                                                                                  ( _lhsOerrors,_lhsOoutput,_lhsOuniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
                                         in  sem_Nonterminals_Cons_1)) of
                                  { ( !sem_Nonterminals_1) ->
                                  ( _lhsOcollect_nts,_lhsOinhMap',_lhsOsynMap',sem_Nonterminals_1) }) }) }) }) }) }))
sem_Nonterminals_Nil :: T_Nonterminals
sem_Nonterminals_Nil =
    (T_Nonterminals (case (({-# LINE 148 "src-ag/DefaultRules.ag" #-}
                            Set.empty
                            {-# LINE 1444 "dist/build/DefaultRules" #-}
                            )) of
                     { !_lhsOcollect_nts ->
                     (case (({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                             Map.empty
                             {-# LINE 1449 "dist/build/DefaultRules" #-}
                             )) of
                      { !_lhsOinhMap' ->
                      (case (({-# LINE 4 "src-ag/DistChildAttr.ag" #-}
                              Map.empty
                              {-# LINE 1454 "dist/build/DefaultRules" #-}
                              )) of
                       { !_lhsOsynMap' ->
                       (case ((let sem_Nonterminals_Nil_1 :: T_Nonterminals_1
                                   sem_Nonterminals_Nil_1 =
                                       (T_Nonterminals_1 (\ (!_lhsIaroundsIn)
                                                            (!_lhsIaugmentsIn)
                                                            (!_lhsIcr)
                                                            (!_lhsIinhMap)
                                                            (!_lhsImanualAttrOrderMap)
                                                            (!_lhsImergesIn)
                                                            (!_lhsInonterminals)
                                                            (!_lhsIo_rename)
                                                            (!_lhsIoptions)
                                                            (!_lhsIsynMap)
                                                            (!_lhsItypeSyns)
                                                            (!_lhsIuniq)
                                                            (!_lhsIuseMap)
                                                            (!_lhsIwrappers) ->
                                                              (case (({-# LINE 142 "src-ag/DefaultRules.ag" #-}
                                                                      Seq.empty
                                                                      {-# LINE 1475 "dist/build/DefaultRules" #-}
                                                                      )) of
                                                               { !_lhsOerrors ->
                                                               (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                                                       []
                                                                       {-# LINE 1480 "dist/build/DefaultRules" #-}
                                                                       )) of
                                                                { !_output ->
                                                                (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                                                        _output
                                                                        {-# LINE 1485 "dist/build/DefaultRules" #-}
                                                                        )) of
                                                                 { !_lhsOoutput ->
                                                                 (case (({-# LINE 561 "src-ag/DefaultRules.ag" #-}
                                                                         _lhsIuniq
                                                                         {-# LINE 1490 "dist/build/DefaultRules" #-}
                                                                         )) of
                                                                  { !_lhsOuniq ->
                                                                  ( _lhsOerrors,_lhsOoutput,_lhsOuniq) }) }) }) })))
                               in  sem_Nonterminals_Nil_1)) of
                        { ( !sem_Nonterminals_1) ->
                        ( _lhsOcollect_nts,_lhsOinhMap',_lhsOsynMap',sem_Nonterminals_1) }) }) }) }))
-- Pattern -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         con                  : ConstructorIdent
         nt                   : NontermIdent
      synthesized attributes:
         containsVars         : Bool
         copy                 : Pattern 
         definedAttrs         : Set (Identifier,Identifier)
         errors               : Seq Error
         locals               : Set Identifier
         output               : Pattern 
   alternatives:
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         visit 0:
            local copy        : _
            local output      : _
      alternative Constr:
         child name           : {ConstructorIdent}
         child pats           : Patterns 
         visit 0:
            local copy        : _
            local output      : _
      alternative Irrefutable:
         child pat            : Pattern 
         visit 0:
            local copy        : _
            local output      : _
      alternative Product:
         child pos            : {Pos}
         child pats           : Patterns 
         visit 0:
            local copy        : _
            local output      : _
      alternative Underscore:
         child pos            : {Pos}
         visit 0:
            local copy        : _
            local output      : _
-}
-- cata
sem_Pattern :: Pattern ->
               T_Pattern
sem_Pattern !(Alias _field _attr _pat) =
    (sem_Pattern_Alias _field _attr (sem_Pattern _pat))
sem_Pattern !(Constr _name _pats) =
    (sem_Pattern_Constr _name (sem_Patterns _pats))
sem_Pattern !(Irrefutable _pat) =
    (sem_Pattern_Irrefutable (sem_Pattern _pat))
sem_Pattern !(Product _pos _pats) =
    (sem_Pattern_Product _pos (sem_Patterns _pats))
sem_Pattern !(Underscore _pos) =
    (sem_Pattern_Underscore _pos)
-- semantic domain
newtype T_Pattern = T_Pattern (ConstructorIdent ->
                               NontermIdent ->
                               ( Bool,Pattern,(Set (Identifier,Identifier)),(Seq Error),(Set Identifier),Pattern))
data Inh_Pattern = Inh_Pattern {con_Inh_Pattern :: !(ConstructorIdent),nt_Inh_Pattern :: !(NontermIdent)}
data Syn_Pattern = Syn_Pattern {containsVars_Syn_Pattern :: !(Bool),copy_Syn_Pattern :: !(Pattern),definedAttrs_Syn_Pattern :: !((Set (Identifier,Identifier))),errors_Syn_Pattern :: !((Seq Error)),locals_Syn_Pattern :: !((Set Identifier)),output_Syn_Pattern :: !(Pattern)}
wrap_Pattern :: T_Pattern ->
                Inh_Pattern ->
                Syn_Pattern
wrap_Pattern !(T_Pattern sem) !(Inh_Pattern _lhsIcon _lhsInt) =
    (let ( !_lhsOcontainsVars,!_lhsOcopy,!_lhsOdefinedAttrs,!_lhsOerrors,!_lhsOlocals,!_lhsOoutput) = sem _lhsIcon _lhsInt
     in  (Syn_Pattern _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern ->
                     T_Pattern
sem_Pattern_Alias !field_ !attr_ !(T_Pattern pat_) =
    (T_Pattern (\ (!_lhsIcon)
                  (!_lhsInt) ->
                    (case (({-# LINE 523 "src-ag/DefaultRules.ag" #-}
                            True
                            {-# LINE 1575 "dist/build/DefaultRules" #-}
                            )) of
                     { !_lhsOcontainsVars ->
                     (case (({-# LINE 38 "src-ag/DefaultRules.ag" #-}
                             _lhsInt
                             {-# LINE 1580 "dist/build/DefaultRules" #-}
                             )) of
                      { !_patOnt ->
                      (case (({-# LINE 39 "src-ag/DefaultRules.ag" #-}
                              _lhsIcon
                              {-# LINE 1585 "dist/build/DefaultRules" #-}
                              )) of
                       { !_patOcon ->
                       (case (pat_ _patOcon _patOnt) of
                        { ( !_patIcontainsVars,!_patIcopy,!_patIdefinedAttrs,!_patIerrors,!_patIlocals,!_patIoutput) ->
                            (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                    Alias field_ attr_ _patIcopy
                                    {-# LINE 1592 "dist/build/DefaultRules" #-}
                                    )) of
                             { !_copy ->
                             (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                     _copy
                                     {-# LINE 1597 "dist/build/DefaultRules" #-}
                                     )) of
                              { !_lhsOcopy ->
                              (case (({-# LINE 505 "src-ag/DefaultRules.ag" #-}
                                      Set.insert (field_,attr_) _patIdefinedAttrs
                                      {-# LINE 1602 "dist/build/DefaultRules" #-}
                                      )) of
                               { !_lhsOdefinedAttrs ->
                               (case (({-# LINE 142 "src-ag/DefaultRules.ag" #-}
                                       _patIerrors
                                       {-# LINE 1607 "dist/build/DefaultRules" #-}
                                       )) of
                                { !_lhsOerrors ->
                                (case (({-# LINE 506 "src-ag/DefaultRules.ag" #-}
                                        if field_ == _LOC
                                           then Set.insert attr_ _patIlocals
                                           else _patIlocals
                                        {-# LINE 1614 "dist/build/DefaultRules" #-}
                                        )) of
                                 { !_lhsOlocals ->
                                 (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                         Alias field_ attr_ _patIoutput
                                         {-# LINE 1619 "dist/build/DefaultRules" #-}
                                         )) of
                                  { !_output ->
                                  (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                          _output
                                          {-# LINE 1624 "dist/build/DefaultRules" #-}
                                          )) of
                                   { !_lhsOoutput ->
                                   (case ((Syn_Pattern _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput)) of
                                    { ___node ->
                                    ( _lhsOcontainsVars,_lhsOcopy,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) })))
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns ->
                      T_Pattern
sem_Pattern_Constr !name_ !(T_Patterns pats_) =
    (T_Pattern (\ (!_lhsIcon)
                  (!_lhsInt) ->
                    (case (({-# LINE 38 "src-ag/DefaultRules.ag" #-}
                            _lhsInt
                            {-# LINE 1638 "dist/build/DefaultRules" #-}
                            )) of
                     { !_patsOnt ->
                     (case (({-# LINE 39 "src-ag/DefaultRules.ag" #-}
                             _lhsIcon
                             {-# LINE 1643 "dist/build/DefaultRules" #-}
                             )) of
                      { !_patsOcon ->
                      (case (pats_ _patsOcon _patsOnt) of
                       { ( !_patsIcontainsVars,!_patsIcopy,!_patsIdefinedAttrs,!_patsIerrors,!_patsIlocals,!_patsIoutput) ->
                           (case (({-# LINE 520 "src-ag/DefaultRules.ag" #-}
                                   _patsIcontainsVars
                                   {-# LINE 1650 "dist/build/DefaultRules" #-}
                                   )) of
                            { !_lhsOcontainsVars ->
                            (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                    Constr name_ _patsIcopy
                                    {-# LINE 1655 "dist/build/DefaultRules" #-}
                                    )) of
                             { !_copy ->
                             (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                     _copy
                                     {-# LINE 1660 "dist/build/DefaultRules" #-}
                                     )) of
                              { !_lhsOcopy ->
                              (case (({-# LINE 500 "src-ag/DefaultRules.ag" #-}
                                      _patsIdefinedAttrs
                                      {-# LINE 1665 "dist/build/DefaultRules" #-}
                                      )) of
                               { !_lhsOdefinedAttrs ->
                               (case (({-# LINE 142 "src-ag/DefaultRules.ag" #-}
                                       _patsIerrors
                                       {-# LINE 1670 "dist/build/DefaultRules" #-}
                                       )) of
                                { !_lhsOerrors ->
                                (case (({-# LINE 499 "src-ag/DefaultRules.ag" #-}
                                        _patsIlocals
                                        {-# LINE 1675 "dist/build/DefaultRules" #-}
                                        )) of
                                 { !_lhsOlocals ->
                                 (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                         Constr name_ _patsIoutput
                                         {-# LINE 1680 "dist/build/DefaultRules" #-}
                                         )) of
                                  { !_output ->
                                  (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                          _output
                                          {-# LINE 1685 "dist/build/DefaultRules" #-}
                                          )) of
                                   { !_lhsOoutput ->
                                   (case ((Syn_Pattern _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput)) of
                                    { ___node ->
                                    ( _lhsOcontainsVars,_lhsOcopy,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) })))
sem_Pattern_Irrefutable :: T_Pattern ->
                           T_Pattern
sem_Pattern_Irrefutable !(T_Pattern pat_) =
    (T_Pattern (\ (!_lhsIcon)
                  (!_lhsInt) ->
                    (case (({-# LINE 38 "src-ag/DefaultRules.ag" #-}
                            _lhsInt
                            {-# LINE 1698 "dist/build/DefaultRules" #-}
                            )) of
                     { !_patOnt ->
                     (case (({-# LINE 39 "src-ag/DefaultRules.ag" #-}
                             _lhsIcon
                             {-# LINE 1703 "dist/build/DefaultRules" #-}
                             )) of
                      { !_patOcon ->
                      (case (pat_ _patOcon _patOnt) of
                       { ( !_patIcontainsVars,!_patIcopy,!_patIdefinedAttrs,!_patIerrors,!_patIlocals,!_patIoutput) ->
                           (case (({-# LINE 520 "src-ag/DefaultRules.ag" #-}
                                   _patIcontainsVars
                                   {-# LINE 1710 "dist/build/DefaultRules" #-}
                                   )) of
                            { !_lhsOcontainsVars ->
                            (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                    Irrefutable _patIcopy
                                    {-# LINE 1715 "dist/build/DefaultRules" #-}
                                    )) of
                             { !_copy ->
                             (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                     _copy
                                     {-# LINE 1720 "dist/build/DefaultRules" #-}
                                     )) of
                              { !_lhsOcopy ->
                              (case (({-# LINE 500 "src-ag/DefaultRules.ag" #-}
                                      _patIdefinedAttrs
                                      {-# LINE 1725 "dist/build/DefaultRules" #-}
                                      )) of
                               { !_lhsOdefinedAttrs ->
                               (case (({-# LINE 142 "src-ag/DefaultRules.ag" #-}
                                       _patIerrors
                                       {-# LINE 1730 "dist/build/DefaultRules" #-}
                                       )) of
                                { !_lhsOerrors ->
                                (case (({-# LINE 499 "src-ag/DefaultRules.ag" #-}
                                        _patIlocals
                                        {-# LINE 1735 "dist/build/DefaultRules" #-}
                                        )) of
                                 { !_lhsOlocals ->
                                 (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                         Irrefutable _patIoutput
                                         {-# LINE 1740 "dist/build/DefaultRules" #-}
                                         )) of
                                  { !_output ->
                                  (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                          _output
                                          {-# LINE 1745 "dist/build/DefaultRules" #-}
                                          )) of
                                   { !_lhsOoutput ->
                                   (case ((Syn_Pattern _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput)) of
                                    { ___node ->
                                    ( _lhsOcontainsVars,_lhsOcopy,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) })))
sem_Pattern_Product :: Pos ->
                       T_Patterns ->
                       T_Pattern
sem_Pattern_Product !pos_ !(T_Patterns pats_) =
    (T_Pattern (\ (!_lhsIcon)
                  (!_lhsInt) ->
                    (case (({-# LINE 38 "src-ag/DefaultRules.ag" #-}
                            _lhsInt
                            {-# LINE 1759 "dist/build/DefaultRules" #-}
                            )) of
                     { !_patsOnt ->
                     (case (({-# LINE 39 "src-ag/DefaultRules.ag" #-}
                             _lhsIcon
                             {-# LINE 1764 "dist/build/DefaultRules" #-}
                             )) of
                      { !_patsOcon ->
                      (case (pats_ _patsOcon _patsOnt) of
                       { ( !_patsIcontainsVars,!_patsIcopy,!_patsIdefinedAttrs,!_patsIerrors,!_patsIlocals,!_patsIoutput) ->
                           (case (({-# LINE 520 "src-ag/DefaultRules.ag" #-}
                                   _patsIcontainsVars
                                   {-# LINE 1771 "dist/build/DefaultRules" #-}
                                   )) of
                            { !_lhsOcontainsVars ->
                            (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                    Product pos_ _patsIcopy
                                    {-# LINE 1776 "dist/build/DefaultRules" #-}
                                    )) of
                             { !_copy ->
                             (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                     _copy
                                     {-# LINE 1781 "dist/build/DefaultRules" #-}
                                     )) of
                              { !_lhsOcopy ->
                              (case (({-# LINE 500 "src-ag/DefaultRules.ag" #-}
                                      _patsIdefinedAttrs
                                      {-# LINE 1786 "dist/build/DefaultRules" #-}
                                      )) of
                               { !_lhsOdefinedAttrs ->
                               (case (({-# LINE 142 "src-ag/DefaultRules.ag" #-}
                                       _patsIerrors
                                       {-# LINE 1791 "dist/build/DefaultRules" #-}
                                       )) of
                                { !_lhsOerrors ->
                                (case (({-# LINE 499 "src-ag/DefaultRules.ag" #-}
                                        _patsIlocals
                                        {-# LINE 1796 "dist/build/DefaultRules" #-}
                                        )) of
                                 { !_lhsOlocals ->
                                 (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                         Product pos_ _patsIoutput
                                         {-# LINE 1801 "dist/build/DefaultRules" #-}
                                         )) of
                                  { !_output ->
                                  (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                          _output
                                          {-# LINE 1806 "dist/build/DefaultRules" #-}
                                          )) of
                                   { !_lhsOoutput ->
                                   (case ((Syn_Pattern _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput)) of
                                    { ___node ->
                                    ( _lhsOcontainsVars,_lhsOcopy,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) })))
sem_Pattern_Underscore :: Pos ->
                          T_Pattern
sem_Pattern_Underscore !pos_ =
    (T_Pattern (\ (!_lhsIcon)
                  (!_lhsInt) ->
                    (case (({-# LINE 520 "src-ag/DefaultRules.ag" #-}
                            False
                            {-# LINE 1819 "dist/build/DefaultRules" #-}
                            )) of
                     { !_lhsOcontainsVars ->
                     (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                             Underscore pos_
                             {-# LINE 1824 "dist/build/DefaultRules" #-}
                             )) of
                      { !_copy ->
                      (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                              _copy
                              {-# LINE 1829 "dist/build/DefaultRules" #-}
                              )) of
                       { !_lhsOcopy ->
                       (case (({-# LINE 500 "src-ag/DefaultRules.ag" #-}
                               Set.empty
                               {-# LINE 1834 "dist/build/DefaultRules" #-}
                               )) of
                        { !_lhsOdefinedAttrs ->
                        (case (({-# LINE 142 "src-ag/DefaultRules.ag" #-}
                                Seq.empty
                                {-# LINE 1839 "dist/build/DefaultRules" #-}
                                )) of
                         { !_lhsOerrors ->
                         (case (({-# LINE 499 "src-ag/DefaultRules.ag" #-}
                                 Set.empty
                                 {-# LINE 1844 "dist/build/DefaultRules" #-}
                                 )) of
                          { !_lhsOlocals ->
                          (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                  Underscore pos_
                                  {-# LINE 1849 "dist/build/DefaultRules" #-}
                                  )) of
                           { !_output ->
                           (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                   _output
                                   {-# LINE 1854 "dist/build/DefaultRules" #-}
                                   )) of
                            { !_lhsOoutput ->
                            (case ((Syn_Pattern _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput)) of
                             { ___node ->
                             ( _lhsOcontainsVars,_lhsOcopy,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput) }) }) }) }) }) }) }) }) })))
-- Patterns ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         con                  : ConstructorIdent
         nt                   : NontermIdent
      synthesized attributes:
         containsVars         : Bool
         copy                 : Patterns 
         definedAttrs         : Set (Identifier,Identifier)
         errors               : Seq Error
         locals               : Set Identifier
         output               : Patterns 
   alternatives:
      alternative Cons:
         child hd             : Pattern 
         child tl             : Patterns 
         visit 0:
            local copy        : _
            local output      : _
      alternative Nil:
         visit 0:
            local copy        : _
            local output      : _
-}
-- cata
sem_Patterns :: Patterns ->
                T_Patterns
sem_Patterns !list =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list))
-- semantic domain
newtype T_Patterns = T_Patterns (ConstructorIdent ->
                                 NontermIdent ->
                                 ( Bool,Patterns,(Set (Identifier,Identifier)),(Seq Error),(Set Identifier),Patterns))
data Inh_Patterns = Inh_Patterns {con_Inh_Patterns :: !(ConstructorIdent),nt_Inh_Patterns :: !(NontermIdent)}
data Syn_Patterns = Syn_Patterns {containsVars_Syn_Patterns :: !(Bool),copy_Syn_Patterns :: !(Patterns),definedAttrs_Syn_Patterns :: !((Set (Identifier,Identifier))),errors_Syn_Patterns :: !((Seq Error)),locals_Syn_Patterns :: !((Set Identifier)),output_Syn_Patterns :: !(Patterns)}
wrap_Patterns :: T_Patterns ->
                 Inh_Patterns ->
                 Syn_Patterns
wrap_Patterns !(T_Patterns sem) !(Inh_Patterns _lhsIcon _lhsInt) =
    (let ( !_lhsOcontainsVars,!_lhsOcopy,!_lhsOdefinedAttrs,!_lhsOerrors,!_lhsOlocals,!_lhsOoutput) = sem _lhsIcon _lhsInt
     in  (Syn_Patterns _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput))
sem_Patterns_Cons :: T_Pattern ->
                     T_Patterns ->
                     T_Patterns
sem_Patterns_Cons !(T_Pattern hd_) !(T_Patterns tl_) =
    (T_Patterns (\ (!_lhsIcon)
                   (!_lhsInt) ->
                     (case (({-# LINE 38 "src-ag/DefaultRules.ag" #-}
                             _lhsInt
                             {-# LINE 1910 "dist/build/DefaultRules" #-}
                             )) of
                      { !_tlOnt ->
                      (case (({-# LINE 39 "src-ag/DefaultRules.ag" #-}
                              _lhsIcon
                              {-# LINE 1915 "dist/build/DefaultRules" #-}
                              )) of
                       { !_tlOcon ->
                       (case (tl_ _tlOcon _tlOnt) of
                        { ( !_tlIcontainsVars,!_tlIcopy,!_tlIdefinedAttrs,!_tlIerrors,!_tlIlocals,!_tlIoutput) ->
                            (case (({-# LINE 38 "src-ag/DefaultRules.ag" #-}
                                    _lhsInt
                                    {-# LINE 1922 "dist/build/DefaultRules" #-}
                                    )) of
                             { !_hdOnt ->
                             (case (({-# LINE 39 "src-ag/DefaultRules.ag" #-}
                                     _lhsIcon
                                     {-# LINE 1927 "dist/build/DefaultRules" #-}
                                     )) of
                              { !_hdOcon ->
                              (case (hd_ _hdOcon _hdOnt) of
                               { ( !_hdIcontainsVars,!_hdIcopy,!_hdIdefinedAttrs,!_hdIerrors,!_hdIlocals,!_hdIoutput) ->
                                   (case (({-# LINE 520 "src-ag/DefaultRules.ag" #-}
                                           _hdIcontainsVars || _tlIcontainsVars
                                           {-# LINE 1934 "dist/build/DefaultRules" #-}
                                           )) of
                                    { !_lhsOcontainsVars ->
                                    (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                            (:) _hdIcopy _tlIcopy
                                            {-# LINE 1939 "dist/build/DefaultRules" #-}
                                            )) of
                                     { !_copy ->
                                     (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                                             _copy
                                             {-# LINE 1944 "dist/build/DefaultRules" #-}
                                             )) of
                                      { !_lhsOcopy ->
                                      (case (({-# LINE 500 "src-ag/DefaultRules.ag" #-}
                                              _hdIdefinedAttrs `Set.union` _tlIdefinedAttrs
                                              {-# LINE 1949 "dist/build/DefaultRules" #-}
                                              )) of
                                       { !_lhsOdefinedAttrs ->
                                       (case (({-# LINE 142 "src-ag/DefaultRules.ag" #-}
                                               _hdIerrors Seq.>< _tlIerrors
                                               {-# LINE 1954 "dist/build/DefaultRules" #-}
                                               )) of
                                        { !_lhsOerrors ->
                                        (case (({-# LINE 499 "src-ag/DefaultRules.ag" #-}
                                                _hdIlocals `Set.union` _tlIlocals
                                                {-# LINE 1959 "dist/build/DefaultRules" #-}
                                                )) of
                                         { !_lhsOlocals ->
                                         (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                                 (:) _hdIoutput _tlIoutput
                                                 {-# LINE 1964 "dist/build/DefaultRules" #-}
                                                 )) of
                                          { !_output ->
                                          (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                                  _output
                                                  {-# LINE 1969 "dist/build/DefaultRules" #-}
                                                  )) of
                                           { !_lhsOoutput ->
                                           (case ((Syn_Patterns _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput)) of
                                            { ___node ->
                                            ( _lhsOcontainsVars,_lhsOcopy,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
sem_Patterns_Nil :: T_Patterns
sem_Patterns_Nil =
    (T_Patterns (\ (!_lhsIcon)
                   (!_lhsInt) ->
                     (case (({-# LINE 520 "src-ag/DefaultRules.ag" #-}
                             False
                             {-# LINE 1981 "dist/build/DefaultRules" #-}
                             )) of
                      { !_lhsOcontainsVars ->
                      (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                              []
                              {-# LINE 1986 "dist/build/DefaultRules" #-}
                              )) of
                       { !_copy ->
                       (case (({-# LINE 22 "src-ag/Patterns.ag" #-}
                               _copy
                               {-# LINE 1991 "dist/build/DefaultRules" #-}
                               )) of
                        { !_lhsOcopy ->
                        (case (({-# LINE 500 "src-ag/DefaultRules.ag" #-}
                                Set.empty
                                {-# LINE 1996 "dist/build/DefaultRules" #-}
                                )) of
                         { !_lhsOdefinedAttrs ->
                         (case (({-# LINE 142 "src-ag/DefaultRules.ag" #-}
                                 Seq.empty
                                 {-# LINE 2001 "dist/build/DefaultRules" #-}
                                 )) of
                          { !_lhsOerrors ->
                          (case (({-# LINE 499 "src-ag/DefaultRules.ag" #-}
                                  Set.empty
                                  {-# LINE 2006 "dist/build/DefaultRules" #-}
                                  )) of
                           { !_lhsOlocals ->
                           (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                   []
                                   {-# LINE 2011 "dist/build/DefaultRules" #-}
                                   )) of
                            { !_output ->
                            (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                    _output
                                    {-# LINE 2016 "dist/build/DefaultRules" #-}
                                    )) of
                             { !_lhsOoutput ->
                             (case ((Syn_Patterns _lhsOcontainsVars _lhsOcopy _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput)) of
                              { ___node ->
                              ( _lhsOcontainsVars,_lhsOcopy,_lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput) }) }) }) }) }) }) }) }) })))
-- Production --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         aroundsIn            : Map ConstructorIdent (Map Identifier [Expression])
         augmentsIn           : Map ConstructorIdent (Map Identifier [Expression])
         cr                   : Bool
         inh                  : Attributes
         inhMap               : Map Identifier Attributes
         inhOrig              : Attributes
         manualAttrOrderMap   : AttrOrderMap
         mergesIn             : Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))
         nonterminals         : Set NontermIdent
         nt                   : NontermIdent
         o_rename             : Bool
         options              : Options
         params               : [Identifier]
         syn                  : Attributes
         synMap               : Map Identifier Attributes
         synOrig              : Attributes
         typeSyns             : TypeSyns
         useMap               : Map Identifier (String,String,String)
         wrappers             : Set NontermIdent
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         errors               : Seq Error
         output               : Production 
   alternatives:
      alternative Production:
         child con            : {ConstructorIdent}
         child params         : {[Identifier]}
         child constraints    : {[Type]}
         child children       : Children 
         child rules          : Rules 
         child typeSigs       : TypeSigs 
         child macro          : {MaybeMacro}
         visit 0:
            local mergesIn    : _
            local merged      : _
            local orderDeps   : _
            local orderErrs   : _
            local _tup2       : _
            local errs        : _
            local aroundsIn   : _
            local augmentsIn  : _
            local newRls      : _
            local extra1      : _
            local extra2      : _
            local extra3      : _
-}
-- cata
sem_Production :: Production ->
                  T_Production
sem_Production !(Production _con _params _constraints _children _rules _typeSigs _macro) =
    (sem_Production_Production _con _params _constraints (sem_Children _children) (sem_Rules _rules) (sem_TypeSigs _typeSigs) _macro)
-- semantic domain
newtype T_Production = T_Production ((Map ConstructorIdent (Map Identifier [Expression])) ->
                                     (Map ConstructorIdent (Map Identifier [Expression])) ->
                                     Bool ->
                                     Attributes ->
                                     (Map Identifier Attributes) ->
                                     Attributes ->
                                     AttrOrderMap ->
                                     (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))) ->
                                     (Set NontermIdent) ->
                                     NontermIdent ->
                                     Bool ->
                                     Options ->
                                     ([Identifier]) ->
                                     Attributes ->
                                     (Map Identifier Attributes) ->
                                     Attributes ->
                                     TypeSyns ->
                                     Int ->
                                     (Map Identifier (String,String,String)) ->
                                     (Set NontermIdent) ->
                                     ( (Seq Error),Production,Int))
data Inh_Production = Inh_Production {aroundsIn_Inh_Production :: !((Map ConstructorIdent (Map Identifier [Expression]))),augmentsIn_Inh_Production :: !((Map ConstructorIdent (Map Identifier [Expression]))),cr_Inh_Production :: !(Bool),inh_Inh_Production :: !(Attributes),inhMap_Inh_Production :: !((Map Identifier Attributes)),inhOrig_Inh_Production :: !(Attributes),manualAttrOrderMap_Inh_Production :: !(AttrOrderMap),mergesIn_Inh_Production :: !((Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))),nonterminals_Inh_Production :: !((Set NontermIdent)),nt_Inh_Production :: !(NontermIdent),o_rename_Inh_Production :: !(Bool),options_Inh_Production :: !(Options),params_Inh_Production :: !(([Identifier])),syn_Inh_Production :: !(Attributes),synMap_Inh_Production :: !((Map Identifier Attributes)),synOrig_Inh_Production :: !(Attributes),typeSyns_Inh_Production :: !(TypeSyns),uniq_Inh_Production :: !(Int),useMap_Inh_Production :: !((Map Identifier (String,String,String))),wrappers_Inh_Production :: !((Set NontermIdent))}
data Syn_Production = Syn_Production {errors_Syn_Production :: !((Seq Error)),output_Syn_Production :: !(Production),uniq_Syn_Production :: !(Int)}
wrap_Production :: T_Production ->
                   Inh_Production ->
                   Syn_Production
wrap_Production !(T_Production sem) !(Inh_Production _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsIinhMap _lhsIinhOrig _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsInt _lhsIo_rename _lhsIoptions _lhsIparams _lhsIsyn _lhsIsynMap _lhsIsynOrig _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) =
    (let ( !_lhsOerrors,!_lhsOoutput,!_lhsOuniq) = sem _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsIinhMap _lhsIinhOrig _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsInt _lhsIo_rename _lhsIoptions _lhsIparams _lhsIsyn _lhsIsynMap _lhsIsynOrig _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers
     in  (Syn_Production _lhsOerrors _lhsOoutput _lhsOuniq))
sem_Production_Production :: ConstructorIdent ->
                             ([Identifier]) ->
                             ([Type]) ->
                             T_Children ->
                             T_Rules ->
                             T_TypeSigs ->
                             MaybeMacro ->
                             T_Production
sem_Production_Production !con_ !params_ !constraints_ !(T_Children children_) !(T_Rules rules_) !(T_TypeSigs typeSigs_) !macro_ =
    (T_Production (\ (!_lhsIaroundsIn)
                     (!_lhsIaugmentsIn)
                     (!_lhsIcr)
                     (!_lhsIinh)
                     (!_lhsIinhMap)
                     (!_lhsIinhOrig)
                     (!_lhsImanualAttrOrderMap)
                     (!_lhsImergesIn)
                     (!_lhsInonterminals)
                     (!_lhsInt)
                     (!_lhsIo_rename)
                     (!_lhsIoptions)
                     (!_lhsIparams)
                     (!_lhsIsyn)
                     (!_lhsIsynMap)
                     (!_lhsIsynOrig)
                     (!_lhsItypeSyns)
                     (!_lhsIuniq)
                     (!_lhsIuseMap)
                     (!_lhsIwrappers) ->
                       (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                               _lhsIsynMap
                               {-# LINE 2139 "dist/build/DefaultRules" #-}
                               )) of
                        { !_childrenOsynMap ->
                        (case (({-# LINE 761 "src-ag/DefaultRules.ag" #-}
                                Map.findWithDefault Map.empty con_ _lhsImergesIn
                                {-# LINE 2144 "dist/build/DefaultRules" #-}
                                )) of
                         { !_mergesIn ->
                         (case (({-# LINE 762 "src-ag/DefaultRules.ag" #-}
                                 Set.fromList [ c | (_,cs,_) <- Map.elems _mergesIn    , c <- cs ]
                                 {-# LINE 2149 "dist/build/DefaultRules" #-}
                                 )) of
                          { !_merged ->
                          (case (({-# LINE 757 "src-ag/DefaultRules.ag" #-}
                                  _merged
                                  {-# LINE 2154 "dist/build/DefaultRules" #-}
                                  )) of
                           { !_childrenOmerged ->
                           (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                   _lhsIinhMap
                                   {-# LINE 2159 "dist/build/DefaultRules" #-}
                                   )) of
                            { !_childrenOinhMap ->
                            (case (({-# LINE 688 "src-ag/DefaultRules.ag" #-}
                                    Set.toList $ Map.findWithDefault Set.empty con_ $ Map.findWithDefault Map.empty _lhsInt _lhsImanualAttrOrderMap
                                    {-# LINE 2164 "dist/build/DefaultRules" #-}
                                    )) of
                             { !_orderDeps ->
                             (case (({-# LINE 561 "src-ag/DefaultRules.ag" #-}
                                     _lhsIuniq
                                     {-# LINE 2169 "dist/build/DefaultRules" #-}
                                     )) of
                              { !_rulesOuniq ->
                              (case (({-# LINE 50 "src-ag/DefaultRules.ag" #-}
                                      _lhsIoptions
                                      {-# LINE 2174 "dist/build/DefaultRules" #-}
                                      )) of
                               { !_rulesOoptions ->
                               (case (({-# LINE 38 "src-ag/DefaultRules.ag" #-}
                                       _lhsInt
                                       {-# LINE 2179 "dist/build/DefaultRules" #-}
                                       )) of
                                { !_rulesOnt ->
                                (case (({-# LINE 171 "src-ag/DefaultRules.ag" #-}
                                        con_
                                        {-# LINE 2184 "dist/build/DefaultRules" #-}
                                        )) of
                                 { !_rulesOcon ->
                                 (case (rules_ _rulesOcon _rulesOnt _rulesOoptions _rulesOuniq) of
                                  { ( !_rulesIdefinedAttrs,!_rulesIerrors,!_rulesIlocals,!_rulesIoutput,!_rulesIruleNames,!_rulesIuniq) ->
                                      (case (({-# LINE 41 "src-ag/DefaultRules.ag" #-}
                                              _lhsIparams
                                              {-# LINE 2191 "dist/build/DefaultRules" #-}
                                              )) of
                                       { !_childrenOparams ->
                                       (case (({-# LINE 38 "src-ag/DefaultRules.ag" #-}
                                               _lhsInt
                                               {-# LINE 2196 "dist/build/DefaultRules" #-}
                                               )) of
                                        { !_childrenOnt ->
                                        (case (({-# LINE 55 "src-ag/DefaultRules.ag" #-}
                                                _lhsIcr
                                                {-# LINE 2201 "dist/build/DefaultRules" #-}
                                                )) of
                                         { !_childrenOcr ->
                                         (case (({-# LINE 172 "src-ag/DefaultRules.ag" #-}
                                                 con_
                                                 {-# LINE 2206 "dist/build/DefaultRules" #-}
                                                 )) of
                                          { !_childrenOcon ->
                                          (case (children_ _childrenOcon _childrenOcr _childrenOinhMap _childrenOmerged _childrenOnt _childrenOparams _childrenOsynMap) of
                                           { ( !_childrenIerrors,!_childrenIfields,!_childrenIinputs,!_childrenIoutput,!_childrenIoutputs) ->
                                               (case (({-# LINE 690 "src-ag/DefaultRules.ag" #-}
                                                       let chldOutMap = Map.fromList [ (k, Map.keysSet s) | (k,s) <- _childrenIoutputs ]
                                                           chldInMap  = Map.fromList [ (k, Map.keysSet s) | (k,s) <- _childrenIinputs ]
                                                           isInAttribute :: Identifier -> Identifier -> [Error]
                                                           isInAttribute fld nm
                                                              | fld == _LOC = if nm `Set.member` _rulesIlocals
                                                                              then []
                                                                              else [UndefAttr _lhsInt con_ fld nm False]
                                                              | fld == _LHS = if nm `Map.member` _lhsIinh
                                                                              then []
                                                                              else [UndefAttr _lhsInt con_ fld nm False]
                                                              | otherwise   = if nm `Set.member` (Map.findWithDefault Set.empty fld chldOutMap)
                                                                              then []
                                                                              else [UndefAttr _lhsInt con_ fld nm False]
                                                           isOutAttribute :: Identifier -> Identifier -> [Error]
                                                           isOutAttribute fld nm
                                                              | fld == _LOC = if nm `Set.member` _rulesIlocals
                                                                              then []
                                                                              else [UndefAttr _lhsInt con_ fld nm True]
                                                              | fld == _LHS = if nm `Map.member` _lhsIsyn
                                                                              then []
                                                                              else [UndefAttr _lhsInt con_ fld nm True]
                                                              | otherwise   = if nm `Set.member` (Map.findWithDefault Set.empty fld chldInMap)
                                                                              then []
                                                                              else [UndefAttr _lhsInt con_ fld nm True]
                                                           existsRule nm = if nm `Set.member` _rulesIruleNames
                                                                           then []
                                                                           else [MissingNamedRule _lhsInt con_ nm]
                                                           checkIn (OccAttr fld nm)  = isInAttribute fld nm
                                                           checkIn (OccRule nm)      = existsRule nm
                                                           checkOut (OccAttr fld nm) = isOutAttribute fld nm
                                                           checkOut (OccRule nm)     = existsRule nm
                                                       in Seq.fromList . concat $
                                                          [ checkIn occA ++ checkOut occB
                                                          | (Dependency occA occB) <- _orderDeps
                                                          ]
                                                       {-# LINE 2247 "dist/build/DefaultRules" #-}
                                                       )) of
                                                { !_orderErrs ->
                                                (case (({-# LINE 354 "src-ag/DefaultRules.ag" #-}
                                                        let locals       = _rulesIlocals
                                                            initenv      = Map.fromList (  [ (a,_ACHILD)
                                                                                           | (a,_,_) <- _childrenIfields
                                                                                           ]
                                                                                        ++ attrs(_LHS, _lhsIinh)
                                                                                        ++ [ (a,_LOC)
                                                                                           |  a <- Set.toList locals
                                                                                           ]
                                                                                        )
                                                            attrs (n,as) = [ (a,n) | a <- Map.keys as ]
                                                            envs       = scanl (flip Map.union)
                                                                               initenv
                                                                               (map (Map.fromList . attrs ) _childrenIoutputs)
                                                            child_envs = init envs
                                                            lhs_env    = last envs
                                                            (selfAttrs, normalAttrs)
                                                              = Map.partitionWithKey (\k _ -> maybe False isSELFNonterminal $ Map.lookup k _lhsIsynOrig) _lhsIsyn
                                                            (_,undefAttrs)
                                                              = removeDefined _rulesIdefinedAttrs (_LHS, normalAttrs)
                                                            (useAttrs,others)
                                                              = splitAttrs _lhsIuseMap undefAttrs
                                                            (rules1, errors1)
                                                              = concatRE $ map (copyRule _lhsIoptions _lhsIwrappers _lhsInt con_ _lhsIcr locals)
                                                                               (zip envs (map (removeDefined _rulesIdefinedAttrs) _childrenIinputs))
                                                            uRules
                                                              = map (useRule locals _childrenIoutputs) useAttrs
                                                            selfLocRules
                                                              =  [ selfRule False attr $
                                                                     lexTokens noPos $
                                                                     constructor [childSelf attr nm tp | (nm,tp,virt) <- _childrenIfields, childExists virt]
                                                                 | attr <- Map.keys selfAttrs
                                                                 , not (Set.member attr locals)
                                                                 ]
                                                                 where
                                                                   childSelf self nm tp
                                                                     = case tp of NT nt _ _                       -> attrName nm self
                                                                                  _      | nm `Set.member` locals -> locName nm
                                                                                         | otherwise              -> fieldName nm
                                                                   constructor fs
                                                                    | getName con_ == "Tuple" && _lhsInt `elem` map fst _lhsItypeSyns
                                                                      = "(" ++ concat (intersperse "," fs) ++ ")"
                                                                    | otherwise
                                                                      = getConName _lhsItypeSyns _lhsIo_rename _lhsInt con_ ++ " " ++ unwords fs
                                                                   childExists ChildAttr = False
                                                                   childExists _         = True
                                                            selfRules
                                                              = [ selfRule True attr [mkLocVar attr noPos Nothing]
                                                                | attr <- Map.keys selfAttrs
                                                                , not (Set.member (_LHS,attr) _rulesIdefinedAttrs)
                                                                ]
                                                            (rules5, errs5)
                                                              = copyRule _lhsIoptions
                                                                         _lhsIwrappers
                                                                         _lhsInt
                                                                         con_
                                                                         _lhsIcr
                                                                         locals
                                                                         (lhs_env, (_LHS, others))
                                                        in (uRules++selfLocRules++selfRules++rules5++rules1, errors1><errs5)
                                                        {-# LINE 2310 "dist/build/DefaultRules" #-}
                                                        )) of
                                                 { !__tup2 ->
                                                 (case (({-# LINE 354 "src-ag/DefaultRules.ag" #-}
                                                         __tup2
                                                         {-# LINE 2315 "dist/build/DefaultRules" #-}
                                                         )) of
                                                  { !(_,!_errs) ->
                                                  (case (({-# LINE 352 "src-ag/DefaultRules.ag" #-}
                                                          _childrenIerrors >< _errs >< _rulesIerrors >< _orderErrs
                                                          {-# LINE 2320 "dist/build/DefaultRules" #-}
                                                          )) of
                                                   { !_lhsOerrors ->
                                                   (case (({-# LINE 41 "src-ag/DefaultRules.ag" #-}
                                                           _lhsIparams
                                                           {-# LINE 2325 "dist/build/DefaultRules" #-}
                                                           )) of
                                                    { !_typeSigsOparams ->
                                                    (case (({-# LINE 38 "src-ag/DefaultRules.ag" #-}
                                                            _lhsInt
                                                            {-# LINE 2330 "dist/build/DefaultRules" #-}
                                                            )) of
                                                     { !_typeSigsOnt ->
                                                     (case (({-# LINE 753 "src-ag/DefaultRules.ag" #-}
                                                             Map.findWithDefault Map.empty con_ _lhsIaroundsIn
                                                             {-# LINE 2335 "dist/build/DefaultRules" #-}
                                                             )) of
                                                      { !_aroundsIn ->
                                                      (case (({-# LINE 746 "src-ag/DefaultRules.ag" #-}
                                                              Map.findWithDefault Map.empty con_ _lhsIaugmentsIn
                                                              {-# LINE 2340 "dist/build/DefaultRules" #-}
                                                              )) of
                                                       { !_augmentsIn ->
                                                       (case (({-# LINE 354 "src-ag/DefaultRules.ag" #-}
                                                               __tup2
                                                               {-# LINE 2345 "dist/build/DefaultRules" #-}
                                                               )) of
                                                        { !(!_newRls,_) ->
                                                        (case (({-# LINE 577 "src-ag/DefaultRules.ag" #-}
                                                                foldr addAugments (_rulesIoutput ++ _newRls) (Map.assocs _augmentsIn    )
                                                                {-# LINE 2350 "dist/build/DefaultRules" #-}
                                                                )) of
                                                         { !_extra1 ->
                                                         (case (({-# LINE 578 "src-ag/DefaultRules.ag" #-}
                                                                 foldr addArounds _extra1     (Map.assocs _aroundsIn    )
                                                                 {-# LINE 2355 "dist/build/DefaultRules" #-}
                                                                 )) of
                                                          { !_extra2 ->
                                                          (case (({-# LINE 579 "src-ag/DefaultRules.ag" #-}
                                                                  foldr addMerges _extra2     (Map.assocs _mergesIn    )
                                                                  {-# LINE 2360 "dist/build/DefaultRules" #-}
                                                                  )) of
                                                           { !_extra3 ->
                                                           (case (typeSigs_ _typeSigsOnt _typeSigsOparams) of
                                                            { ( !_typeSigsIoutput) ->
                                                                (case (({-# LINE 580 "src-ag/DefaultRules.ag" #-}
                                                                        Production con_ params_ constraints_ _childrenIoutput _extra3     _typeSigsIoutput macro_
                                                                        {-# LINE 2367 "dist/build/DefaultRules" #-}
                                                                        )) of
                                                                 { !_lhsOoutput ->
                                                                 (case (({-# LINE 561 "src-ag/DefaultRules.ag" #-}
                                                                         _rulesIuniq
                                                                         {-# LINE 2372 "dist/build/DefaultRules" #-}
                                                                         )) of
                                                                  { !_lhsOuniq ->
                                                                  (case ((Syn_Production _lhsOerrors _lhsOoutput _lhsOuniq)) of
                                                                   { ___node ->
                                                                   ( _lhsOerrors,_lhsOoutput,_lhsOuniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
-- Productions -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         aroundsIn            : Map ConstructorIdent (Map Identifier [Expression])
         augmentsIn           : Map ConstructorIdent (Map Identifier [Expression])
         cr                   : Bool
         inh                  : Attributes
         inhMap               : Map Identifier Attributes
         inhOrig              : Attributes
         manualAttrOrderMap   : AttrOrderMap
         mergesIn             : Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))
         nonterminals         : Set NontermIdent
         nt                   : NontermIdent
         o_rename             : Bool
         options              : Options
         params               : [Identifier]
         syn                  : Attributes
         synMap               : Map Identifier Attributes
         synOrig              : Attributes
         typeSyns             : TypeSyns
         useMap               : Map Identifier (String,String,String)
         wrappers             : Set NontermIdent
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         errors               : Seq Error
         output               : Productions 
   alternatives:
      alternative Cons:
         child hd             : Production 
         child tl             : Productions 
         visit 0:
            local output      : _
      alternative Nil:
         visit 0:
            local output      : _
-}
-- cata
sem_Productions :: Productions ->
                   T_Productions
sem_Productions !list =
    (Prelude.foldr sem_Productions_Cons sem_Productions_Nil (Prelude.map sem_Production list))
-- semantic domain
newtype T_Productions = T_Productions ((Map ConstructorIdent (Map Identifier [Expression])) ->
                                       (Map ConstructorIdent (Map Identifier [Expression])) ->
                                       Bool ->
                                       Attributes ->
                                       (Map Identifier Attributes) ->
                                       Attributes ->
                                       AttrOrderMap ->
                                       (Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression))) ->
                                       (Set NontermIdent) ->
                                       NontermIdent ->
                                       Bool ->
                                       Options ->
                                       ([Identifier]) ->
                                       Attributes ->
                                       (Map Identifier Attributes) ->
                                       Attributes ->
                                       TypeSyns ->
                                       Int ->
                                       (Map Identifier (String,String,String)) ->
                                       (Set NontermIdent) ->
                                       ( (Seq Error),Productions,Int))
data Inh_Productions = Inh_Productions {aroundsIn_Inh_Productions :: !((Map ConstructorIdent (Map Identifier [Expression]))),augmentsIn_Inh_Productions :: !((Map ConstructorIdent (Map Identifier [Expression]))),cr_Inh_Productions :: !(Bool),inh_Inh_Productions :: !(Attributes),inhMap_Inh_Productions :: !((Map Identifier Attributes)),inhOrig_Inh_Productions :: !(Attributes),manualAttrOrderMap_Inh_Productions :: !(AttrOrderMap),mergesIn_Inh_Productions :: !((Map ConstructorIdent (Map Identifier (Identifier,[Identifier],Expression)))),nonterminals_Inh_Productions :: !((Set NontermIdent)),nt_Inh_Productions :: !(NontermIdent),o_rename_Inh_Productions :: !(Bool),options_Inh_Productions :: !(Options),params_Inh_Productions :: !(([Identifier])),syn_Inh_Productions :: !(Attributes),synMap_Inh_Productions :: !((Map Identifier Attributes)),synOrig_Inh_Productions :: !(Attributes),typeSyns_Inh_Productions :: !(TypeSyns),uniq_Inh_Productions :: !(Int),useMap_Inh_Productions :: !((Map Identifier (String,String,String))),wrappers_Inh_Productions :: !((Set NontermIdent))}
data Syn_Productions = Syn_Productions {errors_Syn_Productions :: !((Seq Error)),output_Syn_Productions :: !(Productions),uniq_Syn_Productions :: !(Int)}
wrap_Productions :: T_Productions ->
                    Inh_Productions ->
                    Syn_Productions
wrap_Productions !(T_Productions sem) !(Inh_Productions _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsIinhMap _lhsIinhOrig _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsInt _lhsIo_rename _lhsIoptions _lhsIparams _lhsIsyn _lhsIsynMap _lhsIsynOrig _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers) =
    (let ( !_lhsOerrors,!_lhsOoutput,!_lhsOuniq) = sem _lhsIaroundsIn _lhsIaugmentsIn _lhsIcr _lhsIinh _lhsIinhMap _lhsIinhOrig _lhsImanualAttrOrderMap _lhsImergesIn _lhsInonterminals _lhsInt _lhsIo_rename _lhsIoptions _lhsIparams _lhsIsyn _lhsIsynMap _lhsIsynOrig _lhsItypeSyns _lhsIuniq _lhsIuseMap _lhsIwrappers
     in  (Syn_Productions _lhsOerrors _lhsOoutput _lhsOuniq))
sem_Productions_Cons :: T_Production ->
                        T_Productions ->
                        T_Productions
sem_Productions_Cons !(T_Production hd_) !(T_Productions tl_) =
    (T_Productions (\ (!_lhsIaroundsIn)
                      (!_lhsIaugmentsIn)
                      (!_lhsIcr)
                      (!_lhsIinh)
                      (!_lhsIinhMap)
                      (!_lhsIinhOrig)
                      (!_lhsImanualAttrOrderMap)
                      (!_lhsImergesIn)
                      (!_lhsInonterminals)
                      (!_lhsInt)
                      (!_lhsIo_rename)
                      (!_lhsIoptions)
                      (!_lhsIparams)
                      (!_lhsIsyn)
                      (!_lhsIsynMap)
                      (!_lhsIsynOrig)
                      (!_lhsItypeSyns)
                      (!_lhsIuniq)
                      (!_lhsIuseMap)
                      (!_lhsIwrappers) ->
                        (case (({-# LINE 64 "src-ag/DefaultRules.ag" #-}
                                _lhsIwrappers
                                {-# LINE 2477 "dist/build/DefaultRules" #-}
                                )) of
                         { !_tlOwrappers ->
                         (case (({-# LINE 161 "src-ag/DefaultRules.ag" #-}
                                 _lhsIuseMap
                                 {-# LINE 2482 "dist/build/DefaultRules" #-}
                                 )) of
                          { !_tlOuseMap ->
                          (case (({-# LINE 71 "src-ag/DefaultRules.ag" #-}
                                  _lhsItypeSyns
                                  {-# LINE 2487 "dist/build/DefaultRules" #-}
                                  )) of
                           { !_tlOtypeSyns ->
                           (case (({-# LINE 161 "src-ag/DefaultRules.ag" #-}
                                   _lhsIsynOrig
                                   {-# LINE 2492 "dist/build/DefaultRules" #-}
                                   )) of
                            { !_tlOsynOrig ->
                            (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                    _lhsIsynMap
                                    {-# LINE 2497 "dist/build/DefaultRules" #-}
                                    )) of
                             { !_tlOsynMap ->
                             (case (({-# LINE 161 "src-ag/DefaultRules.ag" #-}
                                     _lhsIsyn
                                     {-# LINE 2502 "dist/build/DefaultRules" #-}
                                     )) of
                              { !_tlOsyn ->
                              (case (({-# LINE 50 "src-ag/DefaultRules.ag" #-}
                                      _lhsIoptions
                                      {-# LINE 2507 "dist/build/DefaultRules" #-}
                                      )) of
                               { !_tlOoptions ->
                               (case (({-# LINE 51 "src-ag/DefaultRules.ag" #-}
                                       _lhsIo_rename
                                       {-# LINE 2512 "dist/build/DefaultRules" #-}
                                       )) of
                                { !_tlOo_rename ->
                                (case (({-# LINE 38 "src-ag/DefaultRules.ag" #-}
                                        _lhsInt
                                        {-# LINE 2517 "dist/build/DefaultRules" #-}
                                        )) of
                                 { !_tlOnt ->
                                 (case (({-# LINE 756 "src-ag/DefaultRules.ag" #-}
                                         _lhsImergesIn
                                         {-# LINE 2522 "dist/build/DefaultRules" #-}
                                         )) of
                                  { !_tlOmergesIn ->
                                  (case (({-# LINE 674 "src-ag/DefaultRules.ag" #-}
                                          _lhsImanualAttrOrderMap
                                          {-# LINE 2527 "dist/build/DefaultRules" #-}
                                          )) of
                                   { !_tlOmanualAttrOrderMap ->
                                   (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                           _lhsIinhMap
                                           {-# LINE 2532 "dist/build/DefaultRules" #-}
                                           )) of
                                    { !_tlOinhMap ->
                                    (case (({-# LINE 161 "src-ag/DefaultRules.ag" #-}
                                            _lhsIinh
                                            {-# LINE 2537 "dist/build/DefaultRules" #-}
                                            )) of
                                     { !_tlOinh ->
                                     (case (({-# LINE 52 "src-ag/DefaultRules.ag" #-}
                                             _lhsIcr
                                             {-# LINE 2542 "dist/build/DefaultRules" #-}
                                             )) of
                                      { !_tlOcr ->
                                      (case (({-# LINE 64 "src-ag/DefaultRules.ag" #-}
                                              _lhsIwrappers
                                              {-# LINE 2547 "dist/build/DefaultRules" #-}
                                              )) of
                                       { !_hdOwrappers ->
                                       (case (({-# LINE 161 "src-ag/DefaultRules.ag" #-}
                                               _lhsIuseMap
                                               {-# LINE 2552 "dist/build/DefaultRules" #-}
                                               )) of
                                        { !_hdOuseMap ->
                                        (case (({-# LINE 71 "src-ag/DefaultRules.ag" #-}
                                                _lhsItypeSyns
                                                {-# LINE 2557 "dist/build/DefaultRules" #-}
                                                )) of
                                         { !_hdOtypeSyns ->
                                         (case (({-# LINE 161 "src-ag/DefaultRules.ag" #-}
                                                 _lhsIsynOrig
                                                 {-# LINE 2562 "dist/build/DefaultRules" #-}
                                                 )) of
                                          { !_hdOsynOrig ->
                                          (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                                  _lhsIsynMap
                                                  {-# LINE 2567 "dist/build/DefaultRules" #-}
                                                  )) of
                                           { !_hdOsynMap ->
                                           (case (({-# LINE 161 "src-ag/DefaultRules.ag" #-}
                                                   _lhsIsyn
                                                   {-# LINE 2572 "dist/build/DefaultRules" #-}
                                                   )) of
                                            { !_hdOsyn ->
                                            (case (({-# LINE 50 "src-ag/DefaultRules.ag" #-}
                                                    _lhsIoptions
                                                    {-# LINE 2577 "dist/build/DefaultRules" #-}
                                                    )) of
                                             { !_hdOoptions ->
                                             (case (({-# LINE 51 "src-ag/DefaultRules.ag" #-}
                                                     _lhsIo_rename
                                                     {-# LINE 2582 "dist/build/DefaultRules" #-}
                                                     )) of
                                              { !_hdOo_rename ->
                                              (case (({-# LINE 38 "src-ag/DefaultRules.ag" #-}
                                                      _lhsInt
                                                      {-# LINE 2587 "dist/build/DefaultRules" #-}
                                                      )) of
                                               { !_hdOnt ->
                                               (case (({-# LINE 756 "src-ag/DefaultRules.ag" #-}
                                                       _lhsImergesIn
                                                       {-# LINE 2592 "dist/build/DefaultRules" #-}
                                                       )) of
                                                { !_hdOmergesIn ->
                                                (case (({-# LINE 674 "src-ag/DefaultRules.ag" #-}
                                                        _lhsImanualAttrOrderMap
                                                        {-# LINE 2597 "dist/build/DefaultRules" #-}
                                                        )) of
                                                 { !_hdOmanualAttrOrderMap ->
                                                 (case (({-# LINE 12 "src-ag/DistChildAttr.ag" #-}
                                                         _lhsIinhMap
                                                         {-# LINE 2602 "dist/build/DefaultRules" #-}
                                                         )) of
                                                  { !_hdOinhMap ->
                                                  (case (({-# LINE 161 "src-ag/DefaultRules.ag" #-}
                                                          _lhsIinh
                                                          {-# LINE 2607 "dist/build/DefaultRules" #-}
                                                          )) of
                                                   { !_hdOinh ->
                                                   (case (({-# LINE 52 "src-ag/DefaultRules.ag" #-}
                                                           _lhsIcr
                                                           {-# LINE 2612 "dist/build/DefaultRules" #-}
                                                           )) of
                                                    { !_hdOcr ->
                                                    (case (({-# LINE 561 "src-ag/DefaultRules.ag" #-}
                                                            _lhsIuniq
                                                            {-# LINE 2617 "dist/build/DefaultRules" #-}
                                                            )) of
                                                     { !_hdOuniq ->
                                                     (case (({-# LINE 41 "src-ag/DefaultRules.ag" #-}
                                                             _lhsIparams
                                                             {-# LINE 2622 "dist/build/DefaultRules" #-}
                                                             )) of
                                                      { !_hdOparams ->
                                                      (case (({-# LINE 152 "src-ag/DefaultRules.ag" #-}
                                                              _lhsInonterminals
                                                              {-# LINE 2627 "dist/build/DefaultRules" #-}
                                                              )) of
                                                       { !_hdOnonterminals ->
                                                       (case (({-# LINE 161 "src-ag/DefaultRules.ag" #-}
                                                               _lhsIinhOrig
                                                               {-# LINE 2632 "dist/build/DefaultRules" #-}
                                                               )) of
                                                        { !_hdOinhOrig ->
                                                        (case (({-# LINE 742 "src-ag/DefaultRules.ag" #-}
                                                                _lhsIaugmentsIn
                                                                {-# LINE 2637 "dist/build/DefaultRules" #-}
                                                                )) of
                                                         { !_hdOaugmentsIn ->
                                                         (case (({-# LINE 749 "src-ag/DefaultRules.ag" #-}
                                                                 _lhsIaroundsIn
                                                                 {-# LINE 2642 "dist/build/DefaultRules" #-}
                                                                 )) of
                                                          { !_hdOaroundsIn ->
                                                          (case (hd_ _hdOaroundsIn _hdOaugmentsIn _hdOcr _hdOinh _hdOinhMap _hdOinhOrig _hdOmanualAttrOrderMap _hdOmergesIn _hdOnonterminals _hdOnt _hdOo_rename _hdOoptions _hdOparams _hdOsyn _hdOsynMap _hdOsynOrig _hdOtypeSyns _hdOuniq _hdOuseMap _hdOwrappers) of
                                                           { ( !_hdIerrors,!_hdIoutput,!_hdIuniq) ->
                                                               (case (({-# LINE 561 "src-ag/DefaultRules.ag" #-}
                                                                       _hdIuniq
                                                                       {-# LINE 2649 "dist/build/DefaultRules" #-}
                                                                       )) of
                                                                { !_tlOuniq ->
                                                                (case (({-# LINE 41 "src-ag/DefaultRules.ag" #-}
                                                                        _lhsIparams
                                                                        {-# LINE 2654 "dist/build/DefaultRules" #-}
                                                                        )) of
                                                                 { !_tlOparams ->
                                                                 (case (({-# LINE 152 "src-ag/DefaultRules.ag" #-}
                                                                         _lhsInonterminals
                                                                         {-# LINE 2659 "dist/build/DefaultRules" #-}
                                                                         )) of
                                                                  { !_tlOnonterminals ->
                                                                  (case (({-# LINE 161 "src-ag/DefaultRules.ag" #-}
                                                                          _lhsIinhOrig
                                                                          {-# LINE 2664 "dist/build/DefaultRules" #-}
                                                                          )) of
                                                                   { !_tlOinhOrig ->
                                                                   (case (({-# LINE 742 "src-ag/DefaultRules.ag" #-}
                                                                           _lhsIaugmentsIn
                                                                           {-# LINE 2669 "dist/build/DefaultRules" #-}
                                                                           )) of
                                                                    { !_tlOaugmentsIn ->
                                                                    (case (({-# LINE 749 "src-ag/DefaultRules.ag" #-}
                                                                            _lhsIaroundsIn
                                                                            {-# LINE 2674 "dist/build/DefaultRules" #-}
                                                                            )) of
                                                                     { !_tlOaroundsIn ->
                                                                     (case (tl_ _tlOaroundsIn _tlOaugmentsIn _tlOcr _tlOinh _tlOinhMap _tlOinhOrig _tlOmanualAttrOrderMap _tlOmergesIn _tlOnonterminals _tlOnt _tlOo_rename _tlOoptions _tlOparams _tlOsyn _tlOsynMap _tlOsynOrig _tlOtypeSyns _tlOuniq _tlOuseMap _tlOwrappers) of
                                                                      { ( !_tlIerrors,!_tlIoutput,!_tlIuniq) ->
                                                                          (case (({-# LINE 142 "src-ag/DefaultRules.ag" #-}
                                                                                  _hdIerrors Seq.>< _tlIerrors
                                                                                  {-# LINE 2681 "dist/build/DefaultRules" #-}
                                                                                  )) of
                                                                           { !_lhsOerrors ->
                                                                           (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                                                                   (:) _hdIoutput _tlIoutput
                                                                                   {-# LINE 2686 "dist/build/DefaultRules" #-}
                                                                                   )) of
                                                                            { !_output ->
                                                                            (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                                                                    _output
                                                                                    {-# LINE 2691 "dist/build/DefaultRules" #-}
                                                                                    )) of
                                                                             { !_lhsOoutput ->
                                                                             (case (({-# LINE 561 "src-ag/DefaultRules.ag" #-}
                                                                                     _tlIuniq
                                                                                     {-# LINE 2696 "dist/build/DefaultRules" #-}
                                                                                     )) of
                                                                              { !_lhsOuniq ->
                                                                              (case ((Syn_Productions _lhsOerrors _lhsOoutput _lhsOuniq)) of
                                                                               { ___node ->
                                                                               ( _lhsOerrors,_lhsOoutput,_lhsOuniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
sem_Productions_Nil :: T_Productions
sem_Productions_Nil =
    (T_Productions (\ (!_lhsIaroundsIn)
                      (!_lhsIaugmentsIn)
                      (!_lhsIcr)
                      (!_lhsIinh)
                      (!_lhsIinhMap)
                      (!_lhsIinhOrig)
                      (!_lhsImanualAttrOrderMap)
                      (!_lhsImergesIn)
                      (!_lhsInonterminals)
                      (!_lhsInt)
                      (!_lhsIo_rename)
                      (!_lhsIoptions)
                      (!_lhsIparams)
                      (!_lhsIsyn)
                      (!_lhsIsynMap)
                      (!_lhsIsynOrig)
                      (!_lhsItypeSyns)
                      (!_lhsIuniq)
                      (!_lhsIuseMap)
                      (!_lhsIwrappers) ->
                        (case (({-# LINE 142 "src-ag/DefaultRules.ag" #-}
                                Seq.empty
                                {-# LINE 2726 "dist/build/DefaultRules" #-}
                                )) of
                         { !_lhsOerrors ->
                         (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                 []
                                 {-# LINE 2731 "dist/build/DefaultRules" #-}
                                 )) of
                          { !_output ->
                          (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                  _output
                                  {-# LINE 2736 "dist/build/DefaultRules" #-}
                                  )) of
                           { !_lhsOoutput ->
                           (case (({-# LINE 561 "src-ag/DefaultRules.ag" #-}
                                   _lhsIuniq
                                   {-# LINE 2741 "dist/build/DefaultRules" #-}
                                   )) of
                            { !_lhsOuniq ->
                            (case ((Syn_Productions _lhsOerrors _lhsOoutput _lhsOuniq)) of
                             { ___node ->
                             ( _lhsOerrors,_lhsOoutput,_lhsOuniq) }) }) }) }) })))
-- Rule --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         con                  : ConstructorIdent
         nt                   : NontermIdent
         options              : Options
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         containsVars         : Bool
         definedAttrs         : Set (Identifier,Identifier)
         errors               : Seq Error
         isPure               : Bool
         locals               : Set Identifier
         output               : Rule 
         outputs              : Rules 
         ruleNames            : Set Identifier
   alternatives:
      alternative Rule:
         child mbName         : {Maybe Identifier}
         child pattern        : Pattern 
         child rhs            : {Expression}
         child owrt           : {Bool}
         child origin         : {String}
         child explicit       : {Bool}
         child pure           : {Bool}
         child identity       : {Bool}
         child mbError        : {Maybe Error}
         child eager          : {Bool}
         visit 0:
            local output      : _
            local _tup3       : _
            local output1     : _
            local _tup4       : _
            local outputs     : _
            local mbAlias     : _
-}
-- cata
sem_Rule :: Rule ->
            T_Rule
sem_Rule !(Rule _mbName _pattern _rhs _owrt _origin _explicit _pure _identity _mbError _eager) =
    (sem_Rule_Rule _mbName (sem_Pattern _pattern) _rhs _owrt _origin _explicit _pure _identity _mbError _eager)
-- semantic domain
newtype T_Rule = T_Rule (ConstructorIdent ->
                         NontermIdent ->
                         Options ->
                         Int ->
                         ( Bool,(Set (Identifier,Identifier)),(Seq Error),Bool,(Set Identifier),Rule,Rules,(Set Identifier),Int))
data Inh_Rule = Inh_Rule {con_Inh_Rule :: !(ConstructorIdent),nt_Inh_Rule :: !(NontermIdent),options_Inh_Rule :: !(Options),uniq_Inh_Rule :: !(Int)}
data Syn_Rule = Syn_Rule {containsVars_Syn_Rule :: !(Bool),definedAttrs_Syn_Rule :: !((Set (Identifier,Identifier))),errors_Syn_Rule :: !((Seq Error)),isPure_Syn_Rule :: !(Bool),locals_Syn_Rule :: !((Set Identifier)),output_Syn_Rule :: !(Rule),outputs_Syn_Rule :: !(Rules),ruleNames_Syn_Rule :: !((Set Identifier)),uniq_Syn_Rule :: !(Int)}
wrap_Rule :: T_Rule ->
             Inh_Rule ->
             Syn_Rule
wrap_Rule !(T_Rule sem) !(Inh_Rule _lhsIcon _lhsInt _lhsIoptions _lhsIuniq) =
    (let ( !_lhsOcontainsVars,!_lhsOdefinedAttrs,!_lhsOerrors,!_lhsOisPure,!_lhsOlocals,!_lhsOoutput,!_lhsOoutputs,!_lhsOruleNames,!_lhsOuniq) = sem _lhsIcon _lhsInt _lhsIoptions _lhsIuniq
     in  (Syn_Rule _lhsOcontainsVars _lhsOdefinedAttrs _lhsOerrors _lhsOisPure _lhsOlocals _lhsOoutput _lhsOoutputs _lhsOruleNames _lhsOuniq))
sem_Rule_Rule :: (Maybe Identifier) ->
                 T_Pattern ->
                 Expression ->
                 Bool ->
                 String ->
                 Bool ->
                 Bool ->
                 Bool ->
                 (Maybe Error) ->
                 Bool ->
                 T_Rule
sem_Rule_Rule !mbName_ !(T_Pattern pattern_) !rhs_ !owrt_ !origin_ !explicit_ !pure_ !identity_ !mbError_ !eager_ =
    (T_Rule (\ (!_lhsIcon)
               (!_lhsInt)
               (!_lhsIoptions)
               (!_lhsIuniq) ->
                 (case (({-# LINE 38 "src-ag/DefaultRules.ag" #-}
                         _lhsInt
                         {-# LINE 2822 "dist/build/DefaultRules" #-}
                         )) of
                  { !_patternOnt ->
                  (case (({-# LINE 39 "src-ag/DefaultRules.ag" #-}
                          _lhsIcon
                          {-# LINE 2827 "dist/build/DefaultRules" #-}
                          )) of
                   { !_patternOcon ->
                   (case (pattern_ _patternOcon _patternOnt) of
                    { ( !_patternIcontainsVars,!_patternIcopy,!_patternIdefinedAttrs,!_patternIerrors,!_patternIlocals,!_patternIoutput) ->
                        (case (({-# LINE 520 "src-ag/DefaultRules.ag" #-}
                                _patternIcontainsVars
                                {-# LINE 2834 "dist/build/DefaultRules" #-}
                                )) of
                         { !_lhsOcontainsVars ->
                         (case (({-# LINE 500 "src-ag/DefaultRules.ag" #-}
                                 _patternIdefinedAttrs
                                 {-# LINE 2839 "dist/build/DefaultRules" #-}
                                 )) of
                          { !_lhsOdefinedAttrs ->
                          (case (({-# LINE 142 "src-ag/DefaultRules.ag" #-}
                                  _patternIerrors
                                  {-# LINE 2844 "dist/build/DefaultRules" #-}
                                  )) of
                           { !_lhsOerrors ->
                           (case (({-# LINE 526 "src-ag/DefaultRules.ag" #-}
                                   pure_
                                   {-# LINE 2849 "dist/build/DefaultRules" #-}
                                   )) of
                            { !_lhsOisPure ->
                            (case (({-# LINE 499 "src-ag/DefaultRules.ag" #-}
                                    _patternIlocals
                                    {-# LINE 2854 "dist/build/DefaultRules" #-}
                                    )) of
                             { !_lhsOlocals ->
                             (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                     Rule mbName_ _patternIoutput rhs_ owrt_ origin_ explicit_ pure_ identity_ mbError_ eager_
                                     {-# LINE 2859 "dist/build/DefaultRules" #-}
                                     )) of
                              { !_output ->
                              (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                      _output
                                      {-# LINE 2864 "dist/build/DefaultRules" #-}
                                      )) of
                               { !_lhsOoutput ->
                               (case (({-# LINE 593 "src-ag/DefaultRules.ag" #-}
                                       mkRuleAlias _output
                                       {-# LINE 2869 "dist/build/DefaultRules" #-}
                                       )) of
                                { !__tup3 ->
                                (case (({-# LINE 593 "src-ag/DefaultRules.ag" #-}
                                        __tup3
                                        {-# LINE 2874 "dist/build/DefaultRules" #-}
                                        )) of
                                 { !(!_output1,_) ->
                                 (case (({-# LINE 594 "src-ag/DefaultRules.ag" #-}
                                         if needsMultiRules _lhsIoptions
                                         then multiRule _output1     _lhsIuniq
                                         else ([_output1    ], _lhsIuniq)
                                         {-# LINE 2881 "dist/build/DefaultRules" #-}
                                         )) of
                                  { !__tup4 ->
                                  (case (({-# LINE 594 "src-ag/DefaultRules.ag" #-}
                                          __tup4
                                          {-# LINE 2886 "dist/build/DefaultRules" #-}
                                          )) of
                                   { !(!_outputs,_) ->
                                   (case (({-# LINE 593 "src-ag/DefaultRules.ag" #-}
                                           __tup3
                                           {-# LINE 2891 "dist/build/DefaultRules" #-}
                                           )) of
                                    { !(_,!_mbAlias) ->
                                    (case (({-# LINE 597 "src-ag/DefaultRules.ag" #-}
                                            maybe [] return _mbAlias     ++ _outputs
                                            {-# LINE 2896 "dist/build/DefaultRules" #-}
                                            )) of
                                     { !_lhsOoutputs ->
                                     (case (({-# LINE 682 "src-ag/DefaultRules.ag" #-}
                                             case mbName_ of
                                               Nothing -> Set.empty
                                               Just nm -> Set.singleton nm
                                             {-# LINE 2903 "dist/build/DefaultRules" #-}
                                             )) of
                                      { !_lhsOruleNames ->
                                      (case (({-# LINE 594 "src-ag/DefaultRules.ag" #-}
                                              __tup4
                                              {-# LINE 2908 "dist/build/DefaultRules" #-}
                                              )) of
                                       { !(_,!_lhsOuniq) ->
                                       (case ((Syn_Rule _lhsOcontainsVars _lhsOdefinedAttrs _lhsOerrors _lhsOisPure _lhsOlocals _lhsOoutput _lhsOoutputs _lhsOruleNames _lhsOuniq)) of
                                        { ___node ->
                                        ( _lhsOcontainsVars,_lhsOdefinedAttrs,_lhsOerrors,_lhsOisPure,_lhsOlocals,_lhsOoutput,_lhsOoutputs,_lhsOruleNames,_lhsOuniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
-- Rules -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         con                  : ConstructorIdent
         nt                   : NontermIdent
         options              : Options
      chained attribute:
         uniq                 : Int
      synthesized attributes:
         definedAttrs         : Set (Identifier,Identifier)
         errors               : Seq Error
         locals               : Set Identifier
         output               : Rules 
         ruleNames            : Set Identifier
   alternatives:
      alternative Cons:
         child hd             : Rule 
         child tl             : Rules 
      alternative Nil:
         visit 0:
            local output      : _
-}
-- cata
sem_Rules :: Rules ->
             T_Rules
sem_Rules !list =
    (Prelude.foldr sem_Rules_Cons sem_Rules_Nil (Prelude.map sem_Rule list))
-- semantic domain
newtype T_Rules = T_Rules (ConstructorIdent ->
                           NontermIdent ->
                           Options ->
                           Int ->
                           ( (Set (Identifier,Identifier)),(Seq Error),(Set Identifier),Rules,(Set Identifier),Int))
data Inh_Rules = Inh_Rules {con_Inh_Rules :: !(ConstructorIdent),nt_Inh_Rules :: !(NontermIdent),options_Inh_Rules :: !(Options),uniq_Inh_Rules :: !(Int)}
data Syn_Rules = Syn_Rules {definedAttrs_Syn_Rules :: !((Set (Identifier,Identifier))),errors_Syn_Rules :: !((Seq Error)),locals_Syn_Rules :: !((Set Identifier)),output_Syn_Rules :: !(Rules),ruleNames_Syn_Rules :: !((Set Identifier)),uniq_Syn_Rules :: !(Int)}
wrap_Rules :: T_Rules ->
              Inh_Rules ->
              Syn_Rules
wrap_Rules !(T_Rules sem) !(Inh_Rules _lhsIcon _lhsInt _lhsIoptions _lhsIuniq) =
    (let ( !_lhsOdefinedAttrs,!_lhsOerrors,!_lhsOlocals,!_lhsOoutput,!_lhsOruleNames,!_lhsOuniq) = sem _lhsIcon _lhsInt _lhsIoptions _lhsIuniq
     in  (Syn_Rules _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput _lhsOruleNames _lhsOuniq))
sem_Rules_Cons :: T_Rule ->
                  T_Rules ->
                  T_Rules
sem_Rules_Cons !(T_Rule hd_) !(T_Rules tl_) =
    (T_Rules (\ (!_lhsIcon)
                (!_lhsInt)
                (!_lhsIoptions)
                (!_lhsIuniq) ->
                  (case (({-# LINE 561 "src-ag/DefaultRules.ag" #-}
                          _lhsIuniq
                          {-# LINE 2966 "dist/build/DefaultRules" #-}
                          )) of
                   { !_hdOuniq ->
                   (case (({-# LINE 50 "src-ag/DefaultRules.ag" #-}
                           _lhsIoptions
                           {-# LINE 2971 "dist/build/DefaultRules" #-}
                           )) of
                    { !_hdOoptions ->
                    (case (({-# LINE 38 "src-ag/DefaultRules.ag" #-}
                            _lhsInt
                            {-# LINE 2976 "dist/build/DefaultRules" #-}
                            )) of
                     { !_hdOnt ->
                     (case (({-# LINE 39 "src-ag/DefaultRules.ag" #-}
                             _lhsIcon
                             {-# LINE 2981 "dist/build/DefaultRules" #-}
                             )) of
                      { !_hdOcon ->
                      (case (hd_ _hdOcon _hdOnt _hdOoptions _hdOuniq) of
                       { ( !_hdIcontainsVars,!_hdIdefinedAttrs,!_hdIerrors,!_hdIisPure,!_hdIlocals,!_hdIoutput,!_hdIoutputs,!_hdIruleNames,!_hdIuniq) ->
                           (case (({-# LINE 561 "src-ag/DefaultRules.ag" #-}
                                   _hdIuniq
                                   {-# LINE 2988 "dist/build/DefaultRules" #-}
                                   )) of
                            { !_tlOuniq ->
                            (case (({-# LINE 50 "src-ag/DefaultRules.ag" #-}
                                    _lhsIoptions
                                    {-# LINE 2993 "dist/build/DefaultRules" #-}
                                    )) of
                             { !_tlOoptions ->
                             (case (({-# LINE 38 "src-ag/DefaultRules.ag" #-}
                                     _lhsInt
                                     {-# LINE 2998 "dist/build/DefaultRules" #-}
                                     )) of
                              { !_tlOnt ->
                              (case (({-# LINE 39 "src-ag/DefaultRules.ag" #-}
                                      _lhsIcon
                                      {-# LINE 3003 "dist/build/DefaultRules" #-}
                                      )) of
                               { !_tlOcon ->
                               (case (tl_ _tlOcon _tlOnt _tlOoptions _tlOuniq) of
                                { ( !_tlIdefinedAttrs,!_tlIerrors,!_tlIlocals,!_tlIoutput,!_tlIruleNames,!_tlIuniq) ->
                                    (case (({-# LINE 500 "src-ag/DefaultRules.ag" #-}
                                            _hdIdefinedAttrs `Set.union` _tlIdefinedAttrs
                                            {-# LINE 3010 "dist/build/DefaultRules" #-}
                                            )) of
                                     { !_lhsOdefinedAttrs ->
                                     (case (({-# LINE 142 "src-ag/DefaultRules.ag" #-}
                                             _hdIerrors Seq.>< _tlIerrors
                                             {-# LINE 3015 "dist/build/DefaultRules" #-}
                                             )) of
                                      { !_lhsOerrors ->
                                      (case (({-# LINE 499 "src-ag/DefaultRules.ag" #-}
                                              _hdIlocals `Set.union` _tlIlocals
                                              {-# LINE 3020 "dist/build/DefaultRules" #-}
                                              )) of
                                       { !_lhsOlocals ->
                                       (case (({-# LINE 589 "src-ag/DefaultRules.ag" #-}
                                               if _hdIcontainsVars && _hdIisPure then _hdIoutputs ++ _tlIoutput else _tlIoutput
                                               {-# LINE 3025 "dist/build/DefaultRules" #-}
                                               )) of
                                        { !_lhsOoutput ->
                                        (case (({-# LINE 680 "src-ag/DefaultRules.ag" #-}
                                                _hdIruleNames `Set.union` _tlIruleNames
                                                {-# LINE 3030 "dist/build/DefaultRules" #-}
                                                )) of
                                         { !_lhsOruleNames ->
                                         (case (({-# LINE 561 "src-ag/DefaultRules.ag" #-}
                                                 _tlIuniq
                                                 {-# LINE 3035 "dist/build/DefaultRules" #-}
                                                 )) of
                                          { !_lhsOuniq ->
                                          (case ((Syn_Rules _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput _lhsOruleNames _lhsOuniq)) of
                                           { ___node ->
                                           ( _lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput,_lhsOruleNames,_lhsOuniq) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })))
sem_Rules_Nil :: T_Rules
sem_Rules_Nil =
    (T_Rules (\ (!_lhsIcon)
                (!_lhsInt)
                (!_lhsIoptions)
                (!_lhsIuniq) ->
                  (case (({-# LINE 500 "src-ag/DefaultRules.ag" #-}
                          Set.empty
                          {-# LINE 3049 "dist/build/DefaultRules" #-}
                          )) of
                   { !_lhsOdefinedAttrs ->
                   (case (({-# LINE 142 "src-ag/DefaultRules.ag" #-}
                           Seq.empty
                           {-# LINE 3054 "dist/build/DefaultRules" #-}
                           )) of
                    { !_lhsOerrors ->
                    (case (({-# LINE 499 "src-ag/DefaultRules.ag" #-}
                            Set.empty
                            {-# LINE 3059 "dist/build/DefaultRules" #-}
                            )) of
                     { !_lhsOlocals ->
                     (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                             []
                             {-# LINE 3064 "dist/build/DefaultRules" #-}
                             )) of
                      { !_output ->
                      (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                              _output
                              {-# LINE 3069 "dist/build/DefaultRules" #-}
                              )) of
                       { !_lhsOoutput ->
                       (case (({-# LINE 680 "src-ag/DefaultRules.ag" #-}
                               Set.empty
                               {-# LINE 3074 "dist/build/DefaultRules" #-}
                               )) of
                        { !_lhsOruleNames ->
                        (case (({-# LINE 561 "src-ag/DefaultRules.ag" #-}
                                _lhsIuniq
                                {-# LINE 3079 "dist/build/DefaultRules" #-}
                                )) of
                         { !_lhsOuniq ->
                         (case ((Syn_Rules _lhsOdefinedAttrs _lhsOerrors _lhsOlocals _lhsOoutput _lhsOruleNames _lhsOuniq)) of
                          { ___node ->
                          ( _lhsOdefinedAttrs,_lhsOerrors,_lhsOlocals,_lhsOoutput,_lhsOruleNames,_lhsOuniq) }) }) }) }) }) }) }) })))
-- TypeSig -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         nt                   : NontermIdent
         params               : [Identifier]
      synthesized attribute:
         output               : TypeSig 
   alternatives:
      alternative TypeSig:
         child name           : {Identifier}
         child tp             : {Type}
         visit 0:
            local tp1         : _
-}
-- cata
sem_TypeSig :: TypeSig ->
               T_TypeSig
sem_TypeSig !(TypeSig _name _tp) =
    (sem_TypeSig_TypeSig _name _tp)
-- semantic domain
newtype T_TypeSig = T_TypeSig (NontermIdent ->
                               ([Identifier]) ->
                               ( TypeSig))
data Inh_TypeSig = Inh_TypeSig {nt_Inh_TypeSig :: !(NontermIdent),params_Inh_TypeSig :: !(([Identifier]))}
data Syn_TypeSig = Syn_TypeSig {output_Syn_TypeSig :: !(TypeSig)}
wrap_TypeSig :: T_TypeSig ->
                Inh_TypeSig ->
                Syn_TypeSig
wrap_TypeSig !(T_TypeSig sem) !(Inh_TypeSig _lhsInt _lhsIparams) =
    (let ( !_lhsOoutput) = sem _lhsInt _lhsIparams
     in  (Syn_TypeSig _lhsOoutput))
sem_TypeSig_TypeSig :: Identifier ->
                       Type ->
                       T_TypeSig
sem_TypeSig_TypeSig !name_ !tp_ =
    (T_TypeSig (\ (!_lhsInt)
                  (!_lhsIparams) ->
                    (case (({-# LINE 545 "src-ag/DefaultRules.ag" #-}
                            elimSelfId _lhsInt _lhsIparams tp_
                            {-# LINE 3125 "dist/build/DefaultRules" #-}
                            )) of
                     { !_tp1 ->
                     (case (({-# LINE 586 "src-ag/DefaultRules.ag" #-}
                             TypeSig name_ _tp1
                             {-# LINE 3130 "dist/build/DefaultRules" #-}
                             )) of
                      { !_lhsOoutput ->
                      (case ((Syn_TypeSig _lhsOoutput)) of
                       { ___node ->
                       ( _lhsOoutput) }) }) })))
-- TypeSigs ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         nt                   : NontermIdent
         params               : [Identifier]
      synthesized attribute:
         output               : TypeSigs 
   alternatives:
      alternative Cons:
         child hd             : TypeSig 
         child tl             : TypeSigs 
         visit 0:
            local output      : _
      alternative Nil:
         visit 0:
            local output      : _
-}
-- cata
sem_TypeSigs :: TypeSigs ->
                T_TypeSigs
sem_TypeSigs !list =
    (Prelude.foldr sem_TypeSigs_Cons sem_TypeSigs_Nil (Prelude.map sem_TypeSig list))
-- semantic domain
newtype T_TypeSigs = T_TypeSigs (NontermIdent ->
                                 ([Identifier]) ->
                                 ( TypeSigs))
data Inh_TypeSigs = Inh_TypeSigs {nt_Inh_TypeSigs :: !(NontermIdent),params_Inh_TypeSigs :: !(([Identifier]))}
data Syn_TypeSigs = Syn_TypeSigs {output_Syn_TypeSigs :: !(TypeSigs)}
wrap_TypeSigs :: T_TypeSigs ->
                 Inh_TypeSigs ->
                 Syn_TypeSigs
wrap_TypeSigs !(T_TypeSigs sem) !(Inh_TypeSigs _lhsInt _lhsIparams) =
    (let ( !_lhsOoutput) = sem _lhsInt _lhsIparams
     in  (Syn_TypeSigs _lhsOoutput))
sem_TypeSigs_Cons :: T_TypeSig ->
                     T_TypeSigs ->
                     T_TypeSigs
sem_TypeSigs_Cons !(T_TypeSig hd_) !(T_TypeSigs tl_) =
    (T_TypeSigs (\ (!_lhsInt)
                   (!_lhsIparams) ->
                     (case (({-# LINE 41 "src-ag/DefaultRules.ag" #-}
                             _lhsIparams
                             {-# LINE 3179 "dist/build/DefaultRules" #-}
                             )) of
                      { !_tlOparams ->
                      (case (({-# LINE 38 "src-ag/DefaultRules.ag" #-}
                              _lhsInt
                              {-# LINE 3184 "dist/build/DefaultRules" #-}
                              )) of
                       { !_tlOnt ->
                       (case (({-# LINE 41 "src-ag/DefaultRules.ag" #-}
                               _lhsIparams
                               {-# LINE 3189 "dist/build/DefaultRules" #-}
                               )) of
                        { !_hdOparams ->
                        (case (({-# LINE 38 "src-ag/DefaultRules.ag" #-}
                                _lhsInt
                                {-# LINE 3194 "dist/build/DefaultRules" #-}
                                )) of
                         { !_hdOnt ->
                         (case (tl_ _tlOnt _tlOparams) of
                          { ( !_tlIoutput) ->
                              (case (hd_ _hdOnt _hdOparams) of
                               { ( !_hdIoutput) ->
                                   (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                           (:) _hdIoutput _tlIoutput
                                           {-# LINE 3203 "dist/build/DefaultRules" #-}
                                           )) of
                                    { !_output ->
                                    (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                                            _output
                                            {-# LINE 3208 "dist/build/DefaultRules" #-}
                                            )) of
                                     { !_lhsOoutput ->
                                     (case ((Syn_TypeSigs _lhsOoutput)) of
                                      { ___node ->
                                      ( _lhsOoutput) }) }) }) }) }) }) }) }) })))
sem_TypeSigs_Nil :: T_TypeSigs
sem_TypeSigs_Nil =
    (T_TypeSigs (\ (!_lhsInt)
                   (!_lhsIparams) ->
                     (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                             []
                             {-# LINE 3220 "dist/build/DefaultRules" #-}
                             )) of
                      { !_output ->
                      (case (({-# LINE 568 "src-ag/DefaultRules.ag" #-}
                              _output
                              {-# LINE 3225 "dist/build/DefaultRules" #-}
                              )) of
                       { !_lhsOoutput ->
                       (case ((Syn_TypeSigs _lhsOoutput)) of
                        { ___node ->
                        ( _lhsOoutput) }) }) })))