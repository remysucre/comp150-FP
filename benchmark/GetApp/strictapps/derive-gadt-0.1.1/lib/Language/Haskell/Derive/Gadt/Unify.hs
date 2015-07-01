{-# OPTIONS_GHC -O2 -fglasgow-exts #-}
{-# LANGUAGE BangPatterns, TemplateHaskell #-}

{- |
  A clusterfrolick at the moment. Among other things,
  the @Exists@ con in the @Type@ type isn't handled
  correctly (or at all really, it's just pretended
  that it's @Forall@ because I don't know how to
  handle it).
-}

module Language.Haskell.Derive.Gadt.Unify where

import Data.List
import Data.Tree
import Control.Monad
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Monoid(Monoid(..))
import Data.Either
import Text.PrettyPrint
import Control.Monad
import Control.Monad.Fix
import Data.IORef
import System.IO.Unsafe
        (unsafePerformIO)
import Language.Haskell.Meta.Utils(pretty)
ppHs a = text . pretty $ a

-----------------------------------------------------------------------------

intType = ConT (mkNameG "Int")
doubleType = ConT (mkNameG "Double")

t0 = tupT [varT "x", varT "b"]
t1 = tupT [varT "a", intType]
t2 = tupT [varT "a", doubleType]
t3 = tupT [intType, varT "a"]
t4 = tupT [doubleType, varT "a"]
t5 = tupT [varT "b", intType, t1]
t6 = tupT [doubleType, varT "c", t0]
t7 = tupT [doubleType, varT "c", varT "d"]



t8 = tupT [varT "x", varT "x"]
t9 = tupT [varT "x", t8]


{-
ghci> pprType t5
(((,,) b) Int) (((,) a) Int)
ghci> pprType t6
(((,,) Double) c) (((,) x) b)
ghci> ppHs <$> runQ (unify t5 t6)
Right
  (fromList
     [(NameL "a", [VarT (NameU "a" 2)]),
      (NameL "b", [ConT (NameG "Double")])],
   fromList
     [(NameL "b", [ConT (NameG "Int")]),
      (NameL "c", [ConT (NameG "Int")]),
      (NameL "x", [VarT (NameU "a" 2)])])
-}

-----------------------------------------------------------------------------



-- | .
unify :: Type -> Type -> Q (Either String (Substs,Substs))
unify a b = extractSubsts <$> splitSubsts (matchTypes a b)

type Substs = Map Name [Type]
type SubstMap = Map Name [Name]
type UnifyMap = Map Name [Type]
data UnifyEnv = UnifyEnv
  {substMaps  :: (SubstMap, SubstMap)
  ,isubstMaps :: (SubstMap, SubstMap)
  ,noDupsMap  :: Either String (Map Name (Name,Name))
  ,finalMap   :: Either String (Map Name Name)
  ,unifyMap   :: (UnifyMap, UnifyMap)
  ,stragglers :: [(Type, Type)]}
  deriving(Eq,Show)


varT :: String -> Type
varT s = VarT (mkNameL s)

tupT :: [Type] -> Type
tupT []  = ConT (mkNameG "()")
tupT [t] = t
tupT ts = let n = length ts
          in foldl AppT (tupCon n) ts


listT :: Type -> Type
listT = (listCon `AppT`)



invertSubstMap :: SubstMap -> SubstMap
invertSubstMap = foldl' (\m (a,b) -> M.insertWith' (++) a [b] m) mempty
  . concatMap (\(x,xs) -> fmap (flip (,) x) xs) . M.toList


invertSubsts :: SubstMap -> SubstMap -> Map Name ([Name],[Name])
invertSubsts lmap rmap =
  let lmap2 = fmap (\xs->(xs,[])) (invertSubstMap lmap)
      rmap2 = fmap (\xs->([],xs)) (invertSubstMap rmap)
  in M.unionWith mappend lmap2 rmap2


checkForDups :: Map Name ([Name],[Name]) -> Either String (Map Name (Name,Name))
checkForDups = go [] . M.toList
  where go acc    []                = Right (M.fromList (reverse acc))
        go acc ((x,([a],[b])):rest) = go ((x,(a,b)):acc) rest
        go _   (err          :_)    = Left (show err)

extractSubsts :: UnifyEnv -> Either String (Substs, Substs)
extractSubsts (UnifyEnv {substMaps  = (lmap,rmap)
                        ,isubstMaps = _
                        ,noDupsMap  = _
                        ,finalMap   = fin
                        ,unifyMap   = (lumap,rumap)
                        ,stragglers = ss})
  | (not . null) ss = Left (show ss)
  | otherwise = case fin of
      Left e -> Left e
      Right fin -> let f = fmap VarT fin
                       l = lumap `M.union` fmap (:[]) (f |.| fmap head lmap) -- XXX
                       r = rumap `M.union` fmap (:[]) (f |.| fmap head rmap) -- XXX
                    in Right (l,r)

(|.|) :: (Ord a, Ord b) => Map b c -> Map a b -> Map a c
g |.| f = foldl' (\m (a,b) -> maybe m
                  (flip (M.insert a) m)
                        (M.lookup b g)) mempty
                                        (M.toList f)

renameT :: Type -> Type
renameT = unQ . go
  where go (ForallT ns t) = do
          xs <- replicateM (length ns) (newName "a")
          ForallT xs <$> go (substT (zip ns (fmap VarT xs)) t)
        go (ExistsT ns t) = do
          xs <- replicateM (length ns) (newName "a")
          ExistsT xs <$> go (substT (zip ns (fmap VarT xs)) t)
        go (AppT a b) = AppT <$> go a <*> go b
        go  t = return t

{- UNUSED CURRENTLY
windFunT :: Type -> [Type] -> Type
windFunT = foldr (.->.)

prepFunApp :: Type -> Type -> Maybe ((Type,Type),(Type,[Type]))
prepFunApp f x
  = case unwindFunT (openT f) of
      (_,[]) -> Nothing
      (res,t:args) -> Just ((x,t),(res,args))
-}

noDupsMapToFinalSubsts :: Map Name (Name,Name) -> Either String (Map Name Name)
noDupsMapToFinalSubsts = go [] . M.toList
  where go acc    []         = Right (M.fromList (reverse acc))
        go acc ((x,ns):rest) = check x ns (\n -> go ((x,n):acc) rest)
        check x (NameL _, NameL _) k = k x
        check _ (NameL a, NameG b) k = k (NameG b)
        check _ (NameG a, NameL b) k = k (NameG a)
        check _ (NameG a, NameG b) k
          | a==b = k (NameG a)
          | otherwise = Left (show (NameG a) ++" /= "++ show (NameG b))

buildUMap :: [(Type, Type)] -> ((UnifyMap,UnifyMap),[(Type, Type)])
buildUMap ts = let (ls,xs) = partition varOnLeft ts
                   (rs,ys) = partition varOnRight xs
                   lmap = M.fromList (fmap (\(VarT a,t)->(a,[t])) ls)
                   rmap = M.fromList (fmap (\(t,VarT a)->(a,[t])) rs)
                in ((lmap,rmap),ys) -- wlog, since xs and ys are the same

splitSubsts :: [(Type,Type)] -> Q UnifyEnv
splitSubsts xs = do
  let (vars,tys) = partition bothVars xs
      names = fmap (\(VarT a,VarT b)->(a,b)) vars
      (lnames,rnames) = unzip names --(filter (not . uncurry (==)) names)
  ns <- fmap (:[]) <$> replicateM (length vars) (newName "a")
  let lmap = M.fromListWith (++) (zip lnames ns)
      rmap = M.fromListWith (++) (zip rnames ns)
      (lts,rts) = unzip (filter (not . uncurry (==)) tys)
      rntys = zip (fmap (substT (firstNameMap lmap)) lts)
                  (fmap (substT (firstNameMap rmap)) rts)
      (umaps,others) = buildUMap rntys
      ndm = checkForDups (invertSubsts lmap rmap)
      fsm = either Left noDupsMapToFinalSubsts ndm
  return (UnifyEnv {substMaps  = (lmap,rmap)
                    ,isubstMaps = (invertSubstMap lmap, invertSubstMap rmap)
                    ,noDupsMap  = ndm
                    ,finalMap   = fsm
                    ,unifyMap   = umaps
                    ,stragglers = others})

matchTypes :: Type -> Type -> [(Type, Type)]
matchTypes a b = let c = renameT a
                     d = renameT b
                  in c `seq` d `seq`
                      match typeViaT typeViaT (,)
                                  (openT c)
                                  (openT d)
typeViaT :: ViaT Type Type Type
typeViaT = ViaT
            typeToT
            typeFromT
typeToT :: Type -> T Type
typeToT (t `AppT` t')
  = typeToT t `T` typeToT t'
typeToT t = Tip t
typeFromT :: T Type -> Type
typeFromT (l `T` r)
  = typeFromT l `AppT` typeFromT r
typeFromT (Tip t) = t

firstNameMap :: Map Name [Name] -> [(Name, Type)]
firstNameMap m =
  (flip concatMap (M.toList m)
    (\(a,as)-> case as of
                [] -> []
                n:_ -> [(a,VarT n)]))

isInf :: Name -> Type -> Bool
isInf a t = a `S.member` ftvs t

bothVars :: (Type, Type) -> Bool
bothVars (VarT{},VarT{}) = True
bothVars    _            = False

varOnLeft :: (Type, Type) -> Bool
varOnLeft (VarT{},_) = True
varOnLeft     _      = False

varOnRight :: (Type, Type) -> Bool
varOnRight (_,VarT{}) = True
varOnRight    _       = False

{-
ghci> Right (a,b) <- runQ (unifyTop' (expType[|(,)|]) (expType[|\x y->([x],y)|]))
ghci> pprType a
b -> a -> (b, a)
ghci> fmap (\(a,b)->(a,pprType b)) b
[(b,[] b)]
ghci> ftvs a
fromList [a,b]
-}

-----------------------------------------------------------------------------

data Name
  = NameG String
  | NameL String
  | NameU String !Int
  deriving(Eq,Ord,Read,Show)

data Type
  = ArrowT
  | VarT Name
  | ConT Name
  | AppT Type Type
  | ForallT [Name] Type
  | ExistsT [Name] Type
  deriving(Eq,Ord,Read,Show)




ftvs :: Type -> Set Name
ftvs (VarT n) = S.singleton n
-- ftvs (ConT n) = S.singleton n
ftvs (AppT t t') = ftvs t `S.union` ftvs t'
ftvs (ForallT ns t) = ftvs t `S.difference` S.fromList ns
ftvs (ExistsT ns t) = ftvs t `S.difference` S.fromList ns
ftvs  _ = mempty

btvs :: Type -> Set Name
btvs (ForallT ns t) = S.fromList ns `S.union` btvs t
btvs (ExistsT ns t) = S.fromList ns `S.union` btvs t
btvs _ = mempty


closeT :: Type -> Type
closeT t = case S.toList (ftvs t) of
            [] -> t
            ns -> case t of
                    ForallT ms t -> ForallT (ms++ns) t
                    _ -> ForallT ns t

openT :: Type -> Type
openT (ForallT _ t) = t
openT    t          = t

-- ghci> let t = let a = mkName "a" in AppT (AppT ArrowT (VarT a)) (VarT a)
--
-- ghci> pprType (foldl (.->.) t (replicate 4 t))
-- ((((a -> a) -> a -> a) -> a -> a) -> a -> a) -> a -> a
--
-- ghci> pprType (foldr (.->.) t (replicate 4 t))
-- (a -> a) -> (a -> a) -> (a -> a) -> (a -> a) -> a -> a
(.->.) :: Type -> Type -> Type
a .->. b = (ArrowT `AppT` a) `AppT` b

unwindFunT :: Type -> (Type,[Type])
unwindFunT = go []
  where go acc ((ArrowT `AppT` a)
                        `AppT` b)
                  = go (a:acc) b
        go acc  t = (t, reverse acc)

unwindAppT :: Type -> (Type,[Type])
unwindAppT = go []
  where go acc (AppT a b) = go (b:acc) a
        go acc    t       = (t, acc)

substT :: [(Name, Type)] -> Type -> Type
substT env t = runSubstM (go t) (initSubstEnv env)
  where go (VarT a) = substM a
        go (AppT a b) = AppT <$> localM (go a)
                             <*> localM (go b)
        go (ForallT ns t) = do mapM_ bindM ns
                               ForallT ns <$> go t
        go (ExistsT ns t) = do mapM_ bindM ns
                               ExistsT ns <$> go t
        go  t             = return t

-----------------------------------------------------------------------------

listName :: Name
listName = NameG "[]"

tupName :: Int -> Name
tupName n = NameG
  ("(" ++ replicate (n-1) ',' ++ ")")

listCon :: Type
listCon = ConT listName

tupCon :: Int -> Type
tupCon = ConT . tupName

-----------------------------------------------------------------------------

testType0 :: Type
testType0 = let a = mkName "a"
            in ForallT [a] (AppT (AppT ArrowT (VarT a)) (VarT a))


idType :: Type -> Type
idType t = closeT (AppT (AppT ArrowT t) t)



testType1 :: Type
testType1 = let b = mkName "b"
            in substT [(mkName "a", VarT b)] testType0

testType2 :: Type
testType2 = let a = mkName "a"
                b = mkName "b"
                ta = AppT (AppT ArrowT (VarT a)) (VarT a)
                tb = substT [(a, VarT b)] ta
            in ForallT [a]
                (foldr (.->.)
                       (foldl (.->.)
                              tb
                              (replicate 4 ta))
                       (replicate 2 tb))

-----------------------------------------------------------------------------

pprName :: Name -> Doc
pprName (NameL a) = text a
pprName (NameG a) = text a
pprName (NameU a u) = text a <> char '_' <> int u

pprType :: Type -> Doc
pprType t
  = case unwindFunT t of
      (_,[]) -> go t
      (x,xs) -> hsep . punctuate (space <> text "->") . fmap pprParenType $ xs++[x]
    where go (VarT a)       = pprName a
          go (ConT a)       = pprName a
          go  ArrowT        = text "->" -- ummm
          go (AppT a b)     = pprParenType a <+> pprParenType b
          go (ForallT ns t) = text "forall"
                                <+> hsep (fmap pprName ns)
                                <> char '.'
                                <+> pprType t
          go (ExistsT ns t) = text "exists"
                                <+> hsep (fmap pprName ns)
                                <> char '.'
                                <+> pprType t

pprParenType :: Type -> Doc
pprParenType t
  = case unwindFunT t of
      (_,[]) -> go t
      (x,xs) -> parens . hsep . punctuate (space <> text "->") . fmap pprParenType $ xs++[x]
    where go t@(VarT{}) = pprType t
          go t@(ConT{}) = pprType t
          go t          = parens (pprType t)

-----------------------------------------------------------------------------

(***) f g = \(a,b) -> (f a,g b)
(&&&) f g = \a -> (f a, g a)
mapfst f = \(a,b) -> (f a, b)
mapsnd f = \(a,b) -> (a, f b)

-----------------------------------------------------------------------------

newtype Q a = Q (IO a)
runQ :: Q a -> IO a
runQ (Q io) = io
runIO :: IO a -> Q a
runIO = Q
unQ :: Q a -> a
unQ = unsafePerformIO . runQ

newName :: String -> Q Name
newName s = do
  u <- tick gensymQ
  return (NameU s u)
mkName :: String -> Name
mkName = mkNameL
mkNameG :: String -> Name
mkNameL :: String -> Name
mkNameU :: String -> Int -> Name
mkNameG = NameG
mkNameL = NameL
mkNameU = NameU

newUniq :: Q Int
newUniq = tick gensymQ

instance Functor Q where
  fmap f (Q io) = Q (fmap f io)
instance Monad Q where
  return a = Q (return a)
  Q io >>= k = Q (runQ . k =<< io)
instance Applicative Q where
  pure = return
  (<*>) = ap
gensymQ :: IORef Int
{-# NOINLINE gensymQ #-}
gensymQ = unsafePerformIO (newIORef 0)
tick :: IORef Int -> Q Int
tick ref = runIO (atomicModifyIORef ref (\n -> (n+1,n)))
reset :: IORef Int -> Q ()
reset ref = runIO (writeIORef ref 0)
resetQ :: Q ()
resetQ = reset gensymQ

-----------------------------------------------------------------------------




newtype S s a = S {unS :: forall o. (a -> s -> o) -> s -> o}
instance Functor (S s) where
  fmap f (S g) = S (\k -> g (k . f))
instance Monad (S s) where
  return a = S (\k -> k a)
  S g >>= f = S (\k -> g (\a -> unS (f a) k))
instance MonadFix (S s) where
  mfix f = S (\k s -> let (a,s') = unS (f a) (,) s in k a s')
instance Applicative (S s) where
  pure = return
  (<*>) = ap
get :: S s s
get = S (\k s -> k s s)
gets :: (s -> a) -> S s a
gets f = S (\k s -> k (f s) s)
set :: s -> S s ()
set s = S (\k _ -> k () s)
modify :: (s -> s) -> S s ()
modify f = S (\k -> k () . f)
runS :: S s a -> s -> (a, s)
runS (S g) = g (,)
evalS :: S s a -> s -> a
evalS (S g) = g const
execS :: S s a -> s -> s
execS (S g) = g (flip const)






type SubstM a = S SubstEnv a
data SubstEnv = SubstEnv
  {boundSet :: Set Name
  ,substMap :: Map Name Type}
  deriving(Eq,Ord,Read,Show)
initSubstEnv :: [(Name, Type)] -> SubstEnv
initSubstEnv = SubstEnv mempty . M.fromList
runSubstM :: SubstM a -> SubstEnv -> a
runSubstM m env = fst (runS m env)
bindM :: Name -> SubstM ()
bindM n = do
  bs <- gets boundSet
  modify(\e->e{boundSet=n`S.insert`bs})
substM :: Name -> SubstM Type
substM n = do
  bs <- gets boundSet
  case n `S.member` bs of
    True -> return (VarT n)
    False -> do o <- gets (M.lookup n . substMap)
                case o of
                  Nothing -> return (VarT n)
                  Just t  -> return t
localM :: SubstM a -> SubstM a
localM m = do s <- get
              a <- m
              set s
              return a

-----------------------------------------------------------------------------

data T a
  = Tip a
  | T (T a) (T a)
  deriving (Eq,Ord,Show,Read)

instance Functor T where
  fmap f (Tip a) = Tip (f a)
  fmap f (T t t') = T (fmap f t) (fmap f t')

-----------------------------------------------------------------------------

toListT :: T a -> [a]
toListT = foldrT (:) []

fromListT :: [a] -> [T a]
fromListT [] = []
fromListT [a] = [Tip a]
fromListT [a,b] = [T (Tip a) (Tip b)]
fromListT xs = let (ys, zs) = splitAt (length xs `div` 2) xs
                   [y] = fromListT ys
                   [z] = fromListT zs
                in [T y z]

toTreeT :: (Maybe a -> b) -> T a -> Tree b
toTreeT f (Tip a) = Node (f (Just a)) []
toTreeT f (T l r) = Node (f Nothing) (fmap (toTreeT f) [l,r])

-----------------------------------------------------------------------------

foldrT :: (a -> b -> b) -> b -> T a -> b
foldrT (<>) b t = go (<>) b t id
  where go (<>) b (Tip a)  k = a <> k b
        go (<>) b (T t t') k = go (<>) b t (\b ->
                                go (<>) b t' k)

foldlT :: (a -> b -> a) -> a -> T b -> a
foldlT (<>) a t = go (<>) a t id
  where go (<>) a (Tip b)  k = k (a <> b)
        go (<>) a (T t t') k = go (<>) a t (\a ->
                                go (<>) a t' k)

foldl'T :: (a -> b -> a) -> a -> T b -> a
foldl'T (<>) !a t = go (<>) a t id
  where go (<>) !a (Tip b)  k = k (a <> b)
        go (<>) !a (T t t') k = go (<>) a t (\a ->
                                  go (<>) a t' k)

sumT :: (Num a) => T a -> a
sumT = foldl'T (+) 0

prodT :: (Num a) => T a -> a
prodT = foldl'T (*) 1

andT :: T Bool -> Bool
andT = foldrT (&&) True

orT :: T Bool -> Bool
orT = foldrT (||) False

-----------------------------------------------------------------------------

unifyT :: (Either a (T a) -> c)
       -> (Either b (T b) -> d)
       -> (c -> d -> e)
       -> (T a -> T b -> T e)
unifyT f g (<>) (Tip a) (Tip b)   = Tip (f (Left a) <> g (Left b))
unifyT f g (<>) (Tip a) b@(T _ _) = Tip (f (Left a) <> g (Right b))
unifyT f g (<>) a@(T _ _) (Tip b) = Tip (f (Right a) <> g (Left b))
unifyT f g (<>) (T a a') (T b b') = let (><) = unifyT f g (<>)
                                    in T (a >< b) (a' >< b')

zipT :: (T a -> T b -> c)
     -> (T a -> T b -> T c)
zipT = unifyT (either Tip id)
              (either Tip id)

matchT :: (a -> T x)
       -> (b -> T y)
       -> (T x -> c)
       -> (T y -> d)
       -> (c -> d -> e)
       -> (a -> b -> T e)
matchT ia ib px py (<>) a b = unifyT (either (px . Tip) px)
                                     (either (py . Tip) py)
                                     (<>)
                                     (ia a)
                                     (ib b)

-----------------------------------------------------------------------------

data ViaT a b c
  = ViaT {toT   :: a -> T b
         ,fromT :: T b -> c}

match :: ViaT a x c
      -> ViaT b y d
      -> (c -> d -> e)
      -> (a -> b -> [e])
match da db = ((toListT .) .)
  . matchT (toT da)
           (toT db)
           (fromT da)
           (fromT db)

-----------------------------------------------------------------------------
