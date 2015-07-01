{-# LANGUAGE TypeOperators, BangPatterns #-}
module Jukebox.Clausify where

import Jukebox.Form
import qualified Jukebox.Form as Form
import Jukebox.Name
import Data.List( maximumBy, sortBy, partition )
import Data.Ord
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Jukebox.Seq as S
import Jukebox.Seq(Seq)
import qualified Jukebox.NameMap as NameMap
import Jukebox.NameMap(NameMap)
import qualified Jukebox.Map as Map
import qualified Data.HashSet as Set
import qualified Data.ByteString.Char8 as BS
import Jukebox.Utils
import Jukebox.Options
import Control.Applicative

newtype ClausifyFlags = ClausifyFlags { splitting :: Bool } deriving Show

clausifyFlags =
  inGroup "Clausifier options" $
  ClausifyFlags <$>
    bool "split"
      ["Split the conjecture into several sub-conjectures.",
       "Default: (off)"]

----------------------------------------------------------------------
-- clausify

clausify :: ClausifyFlags -> Problem Form -> CNF
clausify flags inps = close inps (run . clausifyInputs S.Nil S.Nil)
 where
  clausifyInputs theory obligs [] =
    do return (toObligs (S.toList theory) (S.toList obligs))
  
  clausifyInputs theory obligs (inp:inps) | kind inp == Axiom =
    do cs <- clausForm (tag inp) (what inp)
       clausifyInputs (theory `S.append` cs) obligs inps

  clausifyInputs theory obligs (inp:inps) | kind inp `elem` [Conjecture, Question] =
    do clausifyObligs theory obligs (tag inp) (split' (what inp)) inps

  clausifyObligs theory obligs s [] inps =
    do clausifyInputs theory obligs inps
  
  clausifyObligs theory obligs s (a:as) inps =
    do cs <- clausForm s (nt a)
       clausifyObligs theory (obligs `S.append` S.Unit cs) s as inps

  split' a | splitting flags = if null split_a then [true] else split_a
    where split_a = split a
  split' a                   = [a]

split :: Form -> [Form]
split p =
  case positive p of
    ForAll (Bind xs p) ->
      [ ForAll (Bind xs p') | p' <- split p ]
    
    And ps ->
      concatMap split (S.toList ps)
    
    p `Equiv` q ->
      split (nt p \/ q) ++ split (p \/ nt q)

    Or ps ->
      snd $
      maximumBy first
      [ (siz q, [ Or (S.fromList (q':qs)) | q' <- sq ])
      | (q,qs) <- select (S.toList ps)
      , let sq = split q
      ]

    _ ->
      [p]
 where
  select []     = []
  select (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- select xs ]
  
  first (n,x) (m,y) = n `compare` m
  
  siz (And ps)            = S.length ps
  siz (ForAll (Bind _ p)) = siz p
  siz (_ `Equiv` _)       = 2
  siz _                   = 0

{-  
    Or ps | S.size ps > 0 && n > 0 ->
      [ Or (S.fromList (p':ps')) | p' <- split p ]
     where
      pns = [(p,siz p) | p <- S.toList ps]
      ((p,n),pns') = getMax (head pns) [] (tail pns)
      ps' = [ p' | (p',_) <- pns' ]
    
  getMax pn@(p,n) pns [] = (pn,pns)
  getMax pn@(p,n) pns (qm@(q,m):qms)
    | m > n     = getMax qm (pn:pns) qms
    | otherwise = getMax pn (qm:pns) qms
-}

----------------------------------------------------------------------
-- core clausification algorithm

clausForm :: BS.ByteString -> Form -> M [Input Clause]
clausForm s p =
  withName s $
    do miniscoped      <- miniscope . check . simplify                      . check $ p
       noEquivPs       <- removeEquiv                                       . check $ miniscoped
       noExistsPs      <- mapM removeExists                                 . check $ noEquivPs
       noExpensiveOrPs <- fmap concat . mapM removeExpensiveOr              . check $ noExistsPs
       noForAllPs      <- lift . lift . mapM uniqueNames                    . check $ noExpensiveOrPs
       let !cnf_        = S.concatMap cnf                                   . check $ noForAllPs
           !simp        = simplifyCNF . fmap S.toList                       . check $ cnf_
           cs           = S.toList . fmap clause                                    $ simp
           inps         = [ Input (BS.append s (BS.pack i)) Axiom c
                          | (c, i) <- zip cs ("":
                                        [ '_':show i | i <- [1..] ]) ]
       return $! force . check                                                      $ inps

----------------------------------------------------------------------
-- miniscoping
miniscope :: Form -> M Form
miniscope t@Literal{} = return t
miniscope (Not f) = fmap Not (miniscope f)
miniscope (And fs) = fmap And (S.mapM miniscope fs)
miniscope (Or fs) = fmap Or (S.mapM miniscope fs)
miniscope (Equiv f g) = liftM2 Equiv (miniscope f) (miniscope g)
miniscope (ForAll (Bind xs f)) = miniscope f >>= forAll xs
miniscope (Exists (Bind xs f)) = miniscope f >>= forAll xs . nt >>= return . nt

forAll :: NameMap Variable -> Form -> M Form
forAll xs a | Map.null xs = return a
forAll xs a =
  case positive a of
    And as ->
      fmap And (S.mapM (forAll xs) as)
    
    ForAll (Bind ys a)
      | Map.null m -> return (ForAll (Bind ys a))
      | otherwise -> fmap (forAll' ys) (forAll m a)
      where m = xs Map.\\ ys
            forAll' vs (ForAll (Bind vs' t)) = ForAll (Bind (vs `Map.union` vs') t)
            forAll' vs t = ForAll (Bind vs t)

    Or as -> forAllOr xs [ (a, free a) | a <- S.toList as ]

    _ -> return (ForAll (Bind xs a))

forAllOr :: NameMap Variable -> [(Form, NameMap Variable)] -> M Form
forAllOr xs avss = do { y <- yes; forAll xs' (y \/ no) }
  where
    v         = head (NameMap.toList xs)
    xs'       = NameMap.delete v xs
    (bs1,bs2) = partition ((v `NameMap.member`) . snd) avss
    no        = orl [ b | (b,_) <- bs2 ]
    body      = orl [ b | (b,_) <- bs1 ]
    yes       = case bs1 of
                  []      -> return (orl [])
                  [(b,_)] -> forAll (NameMap.singleton v) b
                  _       -> return (ForAll (Bind (NameMap.singleton v) body))
    orl       = foldr (\/) false

----------------------------------------------------------------------
-- removing equivalences

-- removeEquiv p -> ps :
--   POST: And ps is equivalent to p (modulo extra symbols)
--   POST: ps has no Equiv and no Not
removeEquiv :: Form -> M [Form]
removeEquiv p =
  do (defs,pos,_) <- removeEquivAux False p
     return (S.toList (defs `S.append` S.Unit pos))

-- removeEquivAux inEquiv p -> (defs,pos,neg) :
--   PRE: inEquiv is True when we are "under" an Equiv
--   POST: defs is a list of definitions, under which
--         pos is equivalent to p and neg is equivalent to nt p
-- (the reason why "neg" and "nt pos" can be different, is
-- because we want to always code an equivalence as
-- a conjunction of two disjunctions, which leads to fewer
-- clauses -- the "neg" part of the result for the case Equiv
-- below makes use of this)
removeEquivAux :: Bool -> Form -> M (Seq Form,Form,Form)
removeEquivAux inEquiv p =
  case simple p of
    Not p ->
      do (defs,pos,neg) <- removeEquivAux inEquiv p
         return (defs,neg,pos)
  
    And ps ->
      do dps <- sequence [ removeEquivAux inEquiv p | p <- S.toList ps ]
         let (defss,poss,negs) = unzip3 dps
         return ( S.concat defss
                , And (S.fromList poss)
                , Or  (S.fromList negs)
                )

    ForAll (Bind xs p) ->
      do (defs,pos,neg) <- removeEquivAux inEquiv p
         return ( defs
                , ForAll (Bind xs pos)
                , Exists (Bind xs neg)
                )

    p `Equiv` q ->
      do (defsp,posp,negp)    <- removeEquivAux True p
         (defsq,posq,negq)    <- removeEquivAux True q
         (defsp',posp',negp') <- makeCopyable inEquiv posp negp
         (defsq',posq',negq') <- makeCopyable inEquiv posq negq
         return ( S.concat [defsp, defsq, defsp', defsq']
                , (negp' \/ posq') /\ (posp' \/ negq')
                , (negp' \/ negq') /\ (posp' \/ posq')
                )

    Literal l ->
      do return (S.Nil,Literal l,Literal (neg l))

-- makeCopyable turns an argument to an Equiv into something that we are
-- willing to copy. There are two such cases: (1) when the Equiv is
-- not under another Equiv (because we have to copy arguments to an Equiv
-- at least once anyway), (2) if the formula is small.
-- All other formulas will be made small (by means of a definition)
-- before we copy them.
makeCopyable :: Bool -> Form -> Form -> M (Seq Form,Form,Form)
makeCopyable inEquiv pos neg
  | isSmall pos || not inEquiv =
    -- we skolemize here so that we reuse the skolem function
    -- (if we do this after copying, we get several skolemfunctions)
    do pos' <- removeExists pos
       neg' <- removeExists neg
       return (S.Nil,pos',neg')

  | otherwise =
    do dp <- literal "equiv" (free pos)
       return (S.fromList [Literal (Neg dp) \/ pos, Literal (Pos dp) \/ neg], Literal (Pos dp), Literal (Neg dp))
 where
  -- a formula is small if it is already a literal
  isSmall (Literal _)         = True
  isSmall (Not p)             = isSmall p
  isSmall (ForAll (Bind _ p)) = isSmall p
  isSmall (Exists (Bind _ p)) = isSmall p
  isSmall _                   = False

----------------------------------------------------------------------
-- skolemization

-- removeExists p -> p'
--   PRE: p has no Equiv and no Not
--   POST: p' is equivalent to p (modulo extra symbols)
--   POST: p' has no Equiv, no Exists, and no Not
removeExists :: Form -> M Form
removeExists (And ps) =
  do ps <- sequence [ removeExists p | p <- S.toList ps ]
     return (And (S.fromList ps))

removeExists (Or ps) =
  do ps <- sequence [ removeExists p | p <- S.toList ps ]
     return (Or (S.fromList ps))
    
removeExists (ForAll (Bind xs p)) =
  do p' <- removeExists p
     return (ForAll (Bind xs p'))
    
removeExists t@(Exists (Bind xs p)) =
  -- skolemterms have only variables as arguments, arities are large(r)
  do ss <- sequence [ fmap (x |=>) (skolem x (free t)) | x <- NameMap.toList xs ]
     removeExists (subst (foldr (|+|) ids ss) p)
  {-
  -- skolemterms can have other skolemterms as arguments, arities are small(er)
  -- disadvantage: skolemterms are very complicated and deep
  do p' <- skolemize p
     t <- skolem x (S.delete x (free p'))
     return (subst (x |=> t) p')
  -}

removeExists lit =
  do return lit

-- TODO: Avoid recomputing "free" at every step, by having
-- skolemize return the set of free variables as well

-- TODO: Investigate skolemizing top-down instead, find the right
-- optimization

----------------------------------------------------------------------
-- make cheap Ors

removeExpensiveOr :: Form -> M [Form]
removeExpensiveOr p =
  do (defs,p',_) <- removeExpensiveOrAux p
     return (S.toList (defs `S.append` S.Unit p'))

-- cost: represents how it expensive it is to clausify a formula
type Cost = (Integer,Integer) -- (#clauses, #literals)

unitCost :: Cost
unitCost = (1,1)

andCost :: [Cost] -> Cost
andCost cs = (sum (map fst cs), sum (map snd cs))

orCost :: [Cost] -> Cost
orCost []           = (1,0)
orCost [c]          = c
orCost ((c1,l1):cs) = (c1 * c2, c1 * l2 + c2 * l1)
 where
  (c2,l2) = orCost cs
  
removeExpensiveOrAux :: Form -> M (Seq Form,Form,Cost)
removeExpensiveOrAux (And ps) =
  do dcs <- sequence [ removeExpensiveOrAux p | p <- S.toList ps ]
     let (defss,ps,costs) = unzip3 dcs
     return (S.concat defss, And (S.fromList ps), andCost costs)

removeExpensiveOrAux (Or ps) =
  do dcs <- sequence [ removeExpensiveOrAux p | p <- S.toList ps ]
     let (defss,ps,costs) = unzip3 dcs
     (defs2,p,c) <- makeOr (sortBy (comparing snd) (zip ps costs))
     return (S.concat defss `S.append` defs2,p,c)

removeExpensiveOrAux (ForAll (Bind xs p)) =
  do (defs,p',cost) <- removeExpensiveOrAux p
     return (fmap (ForAll . Bind xs) defs, ForAll (Bind xs p'), cost)

removeExpensiveOrAux lit =
  do return (S.Nil, lit, unitCost)

-- input is sorted; small costs first
makeOr :: [(Form,Cost)] -> M (Seq Form,Form,Cost)
makeOr [] =
  do return (S.Nil, false, orCost [])

makeOr [(f,c)] =
  do return (S.Nil,f,c)

makeOr fcs
  | null fcs2 =
    do return (S.Nil, Or (S.fromList (map fst fcs1)), orCost (map snd fcs1))

  | otherwise =
    do d <- literal "or" (free (map fst fcs2))
       (defs,p,_) <- makeOr ((Literal (Neg d),unitCost):fcs2)
       return ( defs `S.snoc` p
              , Or (S.fromList (Literal (Pos d) : map fst fcs1))
              , orCost (unitCost : map snd fcs1)
              )
 where
  (fcs1,fcs2) = split [] fcs
  
  split fcs1 []                            = (fcs1,[])
  split fcs1 (fc@(_,(cc,_)):fcs) | cc <= 1 = split (fc:fcs1) fcs
  split fcs1 fcs@((_,(cc,_)):_)  | cc <= 2 = (take 2 fcs ++ fcs1, drop 2 fcs)
  split fcs1 fcs                           = (take 1 fcs ++ fcs1, drop 1 fcs)

----------------------------------------------------------------------
-- clausification

-- cnf p = cs
--   PRE: p has no Equiv, no Exists, and no Not,
--        and each variable is only bound once
--   POST: And (map Or cs) is equivalent to p
cnf :: Form -> Seq (Seq Literal)
cnf (ForAll (Bind _ p)) = cnf p
cnf (And ps)            = S.concatMap cnf ps
cnf (Or ps)             = cross (fmap cnf ps)
cnf (Literal x)         = S.Unit (S.Unit x)

cross :: Seq (Seq (Seq Literal)) -> Seq (Seq Literal)
cross S.Nil = S.Unit S.Nil
cross (S.Unit x) = x
cross (S.Append cs1 cs2) = liftM2 S.append (cross cs1) (cross cs2)

----------------------------------------------------------------------
-- simplification of CNF

simplifyCNF :: Seq [Literal] -> [[Literal]]
simplifyCNF =
  -- nub: don't generate multiple copies of identical clauses
  nub . S.concatMap (tautElim . unify [])
  where -- remove negative variable equalities X != Y by substitution
        unify xs [] = xs
        unify xs (Neg (Var v :=: t@Var{}):ys) =
          unify (subst (v |=> t) xs) (subst (v |=> t) ys)
        unify xs (l:ys) = unify (l:xs) ys
        -- simplify p | ~p or t = t to true.
        tautElim ls
          | Set.null (pos `Set.intersection` neg) && not (any tauto ls)
            -- reorder the order of the literals in the clause
            -- so that more clauses become equal;
            -- also, remove duplicate literals from the clause
            = S.Unit (map Neg (Set.toList neg) ++ map Pos (Set.toList pos))
          | otherwise = S.Nil
          where pos = Set.fromList [ l | Pos l <- ls ]
                neg = Set.fromList [ l | Neg l <- ls ]
                tauto (Pos (t :=: u)) = t == u
                tauto _ = False

----------------------------------------------------------------------
-- monad

type M = ReaderT Tag (StateT Int NameM)

run :: M a -> NameM a
run x = evalStateT (runReaderT x BS.empty) 0

skolemName :: Named a => String -> a -> M Name
skolemName prefix v = do
  i <- get
  put (i+1)
  s <- getName
  lift . lift . newName $ prefix ++ show i ++ concat [ "_" ++ t | t <- map BS.unpack [s, baseName v], not (null t) ]

nextSk :: M Int
nextSk = do
  i <- get
  put (i+1)
  return i

withName :: Tag -> M a -> M a
withName s m = lift (runReaderT m s)

getName :: M Tag
getName = ask

skolem :: Variable -> NameMap Variable -> M Term
skolem (v ::: t) vs =
  do n <- skolemName "sK" v
     let f = n ::: FunType (map typ args) t
     return (f :@: map Var args)
 where
  args = NameMap.toList vs

literal :: String -> NameMap Variable -> M Atomic
literal w vs =
  do n <- skolemName "sP" w
     let p = n ::: FunType (map typ args) O
     return (Tru (p :@: map Var args))
 where
  args = NameMap.toList vs

----------------------------------------------------------------------
-- the end.
