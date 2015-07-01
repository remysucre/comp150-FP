{-# LANGUAGE FlexibleInstances, CPP, MultiParamTypeClasses #-}
module Language.CalDims.Action 
	( Command (..)
	, Result (..)
	, Mon
	, run
	, eval
	, doCall
	, dependencies
	, evalDimsPart
	, process) where

import Language.CalDims.Helpers
import Language.CalDims.Types
import Language.CalDims.Texts as Texts
import Language.CalDims.State 
import Language.CalDims.Expr ()

import Control.Monad.State hiding (State)
import qualified Control.Monad.State as SM
import Control.Monad.Error
import qualified Data.Map as Map
import Data.Ratio
import Data.List (nub)

#ifdef DEBUG
import Debug.Trace
#endif

data Command 
	= AddFunction Name Args Expr 
	| AddBasicUnit Name 
	| AddUnit Name Expr 
	| Echo String 
	| Remove Name 
	| RemoveCascade Name 
	| Help
	| Eval Expr Conversion
	| EvalDimsPart Expr
	| DebugExpr Expr 
	| DebugName Name 
	| DebugDependencies Name 
	| GetState 
	| WriteState String deriving Show

instance Pretty Command where pretty = show

data Result = Ok (Maybe String) | StringResult String | EvaledResult (R, Dims) | ExprResult Expr | DimsResult Dims | WriteStringToFile String String deriving Show
instance Pretty Result where
	pretty (Ok Nothing) = "ok."
	pretty (Ok (Just s)) = "ok, but " ++ s
	pretty (StringResult s) = s
	pretty (EvaledResult (r, dims)) = let d = pretty dims in 
		pretty r ++ (if null d then "" else " " ++ pretty dims)
	pretty (DimsResult dims) = pretty dims
	pretty (ExprResult e) = pretty e
	pretty (WriteStringToFile _ fn) = "Writing state to " ++ fn ++ "."


minEval :: Expr -> Mon (R, Dims)
minEval e = do
	x <- eval e
	dims <- minDims (snd x)
	convert x dims

type Mon a = ErrorAndState String State a

process :: Command -> Mon Result
process (AddFunction n args expr) = insert n $ Function args expr
process (AddBasicUnit n) = insert n $ BasicDimension
process (AddUnit n expr) = insert n $ Dimension expr
process (Echo s) = return $ StringResult s
process (Remove n) = remove False n 
process (RemoveCascade n) = remove True n 
process Help = return $ StringResult (show Texts.helpText)
process (Eval expr conv) = do
	res <- eval expr
	case conv of
		Keep -> return $ EvaledResult res
		Explicit dims -> do
			res' <- convert res dims
			return $ EvaledResult res'
		InTermsOf tExpr -> do
			r@(n,_) <- minEval tExpr
			if (n==0) 
				then fail "cannot express in terms of 0."
				else do
					r' <- minEval (Bin Div expr (Evaled r))
					return $ ExprResult (Bin Mul (Evaled r') tExpr) 
				-- FIXME: there are actually 3 ways to combine:
				-- a/b b/a and a*b -> choose the simplest
		Basic -> do
			res' <- convertBasic res
			return $ EvaledResult res'
		Minimal -> do
			dims <- minDims (snd res)
			res' <- convert res dims
			return $ EvaledResult res'

process (EvalDimsPart expr) = do
	res <- evalDimsPart expr
	return $ DimsResult res
process (DebugExpr expr) = return $ StringResult $ pretty expr
process (DebugName n) = do
	entry <- requireEntry n
	return $ StringResult (pretty (n, entry))
process (DebugDependencies n) = do
	d <- dependencies n
	r <- reverseDependencies n
	return $ StringResult (unlines' [pretty d, pretty r])
	
process GetState = do
	state <- get
	return $ StringResult (pretty state)
process (WriteState file) = do
	state <- get
	return $ WriteStringToFile (pretty state) file
	

instance Pretty State where --FIXME: the implementation might be too complex
        pretty s = case (runState $ runErrorT f) s of
                                (Left e, _) -> error e
                                (Right l, _) -> unlines' l 
		where 
			f = do
                        	state <- get
	                        depList <- liftM nub $ liftM reverse $ liftM (filter (not . isBuiltin state)) $ liftM concat $ sequence $ map dependencies' (Map.keys $ getScope state)
        	                mapM pretty' depList

			pretty' :: Name -> Mon String
			pretty' n = do
				e <- requireEntry n
				return $ pretty (n, e)


remove :: Bool -> Name -> Mon Result
remove casc n = do
	rd <- reverseDependencies n
	case (null rd, casc) of
		(True, False) -> remove_ >> return (Ok Nothing)
		(_, True) -> removeList_ (n:rd)  >> return (Ok Nothing)
		(False, False) -> throwError $ "Please remove " ++ pretty rd ++ " first, or remove " ++ pretty n ++ " cascading."
	where
		remove_ = modi $ Map.delete n
		removeList_ l = modi $ Map.filterWithKey (\x _ -> not $ x `elem` l)


modi :: (Scope -> Scope) -> Mon ()
modi f = modify (\x -> x {getScope = f (getScope x)})


insert :: Name -> StateEntry -> Mon Result
insert n newEntry = let 
	doInsert = modi $ Map.insert n newEntry in do 
        oldEntry <- getEntry n
      	warn <- case oldEntry of
                Nothing -> doInsert >> return Nothing
                Just oldEntry_ -> do
			i <- typeIsomorph newEntry oldEntry_
			if i
				then doInsert >> return (Just "an object has changed")
				else throwError $ "There already is an object `" ++ pretty n ++ "` with another type."
	return (Ok warn)


dependencies' :: Name -> Mon [Name]
dependencies' n = do
	d <- dependencies n
	return $ n:d


dependencies :: Name -> Mon [Name]
dependencies n = do
	e <- requireEntry n
	return $ nub $ case e of
		(Function args def) -> f args def
		(Dimension expr) -> getDepsExpr expr
		(BasicDimension) -> []
		(Builtin args def) -> f args def

	where f args def = concatMap getDepsArg args ++ getDepsExpr def


getDepsExpr :: Expr -> [Name]
getDepsExpr (Bin _ expr1 expr2) = getDepsExpr expr1 ++ getDepsExpr expr2
getDepsExpr (Uni _ expr) = getDepsExpr expr
getDepsExpr (ArgRef _) = []
getDepsExpr (Call n es) = n : concatMap getDepsExpr es
getDepsExpr (Evaled (_, d)) = getDepsDims d

getDepsArg :: Arg -> [Name]
getDepsArg = getDepsDims . getArgType

getDepsDims :: Dims -> [Name]
getDepsDims (Dims d) = Map.keys d

reverseDependencies :: Name -> Mon [Name]
reverseDependencies n = do
	state <- get
	let 
		all_ = Map.keys (getScope state)
		filterF name_ = do
			d <- dependencies name_
			return $ n `elem` d

	res <- liftM nub $ filterM filterF all_
	if n `elem` all_
		then return res
		else fail ("no such thing: " ++ unName n)
	

getEntry :: Name -> Mon (Maybe StateEntry)
getEntry s = do
	state <- get
	return $ Map.lookup s (getScope state)

requireEntry :: Name -> Mon StateEntry
requireEntry n = do
	e <- getEntry n
	case e of
		Nothing -> throwError $ "No such object: " ++ pretty n
		Just sth -> return sth

requireFunction :: Name -> Mon (Args, Expr)
requireFunction n = do
	e <- requireEntry n
	case e of
		Function args expr -> return (args, expr)
		Builtin args expr -> return (args, expr)
		_ -> throwError $ pretty n ++ " is not a function"

typeIsomorph :: StateEntry -> StateEntry -> Mon Bool
typeIsomorph a b = case (a, b) of
	(BasicDimension, BasicDimension) -> return True
	(_, BasicDimension) -> return False
	(BasicDimension, _) -> return False
	(Dimension e1, Dimension e2) -> check1 e1 e2
	(Dimension _, _) -> return False
	(_, Dimension _) -> return False

	(Function a1 e1, Function a2 e2) -> check2 e1 e2 a1 a2
	(Builtin a1 e1, Builtin a2 e2) -> check2 e1 e2 a1 a2 -- FIXME rplc Builtin by Function Bool
	(Builtin a1 e1, Function a2 e2) -> check2 e1 e2 a1 a2
	(Function a1 e1, Builtin a2 e2) -> check2 e1 e2 a1 a2


check1 :: Expr -> Expr -> Mon Bool
check1 e1 e2 = checkReturnDims (e1, []) (e2, [])

checkReturnDims :: (Expr, Args) -> (Expr, Args) -> Mon Bool
checkReturnDims (e1, a1) (e2, a2) = do
	(_, d1) <- doCall' a1 e1 (f a1)
	(_, d2) <- doCall' a2 e2 (f a2)
	(return d1 #==# return d2)

	where 
		f = map f'
		f' :: Arg -> (R, Dims)
		f' a = (undefined, getArgType a)

--doCall' :: Args -> Expr -> [(R, Dims)] -> Mon (R, Dims)

check2 :: Expr -> Expr -> Args -> Args -> Mon Bool
check2 e1 e2 a1 a2 = (return a1 #==# return a2) #&&# checkReturnDims (e1, a1) (e2, a2)

evalDimsPart :: Expr -> Mon Dims
evalDimsPart e = do
	res <- eval e
	return $ snd res

eval :: Expr -> Mon (R, Dims)
eval (Call n args) = do
	args' <- mapM eval args
	doCall n args'

eval (ArgRef (Arg _ i _)) = do
	state <- get
	getArgValues state #!!# i

eval (Bin Add e1 e2) = eval e1 #+# eval e2
eval (Bin Sub e1 e2) = eval e1 #-# eval e2
eval (Bin Mul e1 e2) = eval e1 #*# eval e2
eval (Bin Div e1 e2) = eval e1 #/# eval e2
eval (Bin Exp e1 e2) = eval e1 #^# eval e2
eval (Bin LogBase e1 e2) = eval e1 #~# eval e2

eval (Uni Negate e) = negateM $ eval e
eval (Uni Expot e) = expM $ eval e
eval (Uni Log e) = logM $ eval e
eval (Uni Sin e) = sinM $ eval e
eval (Uni Cos e) = cosM $ eval e
eval (Uni Tan e) = tanM $ eval e
eval (Uni Asin e) = asinM $ eval e
eval (Uni Acos e) = acosM $ eval e
eval (Uni Atan e) = atanM $ eval e
eval (Uni Sinh e) = sinhM $ eval e
eval (Uni Cosh e) = coshM $ eval e
eval (Uni Tanh e) = tanhM $ eval e
eval (Uni Asinh e) = asinhM $ eval e
eval (Uni Acosh e) = acoshM $ eval e
eval (Uni Atanh e) = atanhM $ eval e

eval (Evaled e) = return e

(#!!#) :: Pretty a => [a] -> Int -> Mon a
vals #!!# i = if (i < 0) || (length vals < i + 1)
		then throwError $ "Index out of bounds: " ++ pretty vals ++ "!!" ++ pretty i
		else return $ vals !! i


doCall :: Name -> [(R, Dims)] -> Mon (R, Dims)
doCall n args  = do
	(sig, expr) <- requireFunction n
	doCall' sig expr args

doCall' :: Args -> Expr -> [(R, Dims)] -> Mon (R, Dims)
doCall' sig expr args  = do

	state <- get
	let old = getArgValues state
        modify (\x -> x {getArgValues = args})

	when (length sig /= length args) (throwError "wrong number of arguments")
	c <- check (zip (map snd args) sig)
	when (not c) (throwError "wrong typed argument")

	res <- eval expr
	modify (\x -> x {getArgValues = old})
	
	return $ res

check, check' :: [(Dims, Arg)] -> Mon Bool

#ifdef DEBUG
check l = trace (show l) (check' l)
#else
check = check'
#endif

check' [] = return True
check' ((d', (Arg _ _ d)):as) = (return d' #==# return d) #&&# (check' as)


instance EqM Arg (ErrorT String (SM.State State)) where
	(#==#) a b = do
		(Arg _ i1 t1) <- a
		(Arg _ i2 t2) <- b
		t <- (return t1) #==# (return t2)
		return (t && i1 == i2)

instance EqM [Arg] (ErrorT String (SM.State State)) where
	(#==#) l1 l2 = let
		nullM = liftM null
		nNullM = liftM (not . null)
		headM = liftM head
		tailM = liftM tail in
		caseM
			[ (nullM l1 #&&# nullM l2, return True)
			, (nNullM l1 #&&# nullM l2, return False)
			, (nullM l1 #&&# nNullM l2, return False)]
			((headM l1 #==# headM l2) #&&# (tailM l1 #==# tailM l2))

caseM :: Monad m => [(m Bool, m a)] -> m a -> m a
caseM [] base = base
caseM ((cond, res):rest) base = ifM cond res (caseM rest base)

ifM :: Monad m => (m Bool) -> m a -> m a -> m a
ifM cond f s = do
	cond' <- cond
	if cond' then f else s

convertBasic :: (R, Dims) -> Mon (R, Dims)
convertBasic (r1, d1) = do
	(r2, d2) <- compileDims d1
	return (r1*r2, d2)


convert :: (R, Dims) -> Dims -> Mon (R, Dims)
convert (r, d) target = ifM
	((return d) #==# (return target))
	(do
		(ra, _) <- compileDims d
		(rb, _) <- compileDims target
		return (r * ra / rb, target))
	(throwError "Conversion to incompatible dimensions not possible.")

compileDims :: Dims -> Mon (R, Dims)
compileDims d = do
	(r, d_) <- compileDims' d
	return (r, Dims $ (Map.filter (/=0) . unDims) d_)

compileDims' :: Dims -> Mon (R, Dims)
compileDims' d = case dims2list d of
	[] -> return (1, noDims)
	[(n, i)] -> do
		e <- requireEntry n
		case e of 
			BasicDimension -> return (1, Dims $ Map.singleton n i)
			Dimension expr -> do
				(r1, d1) <- eval (Bin Exp expr (Evaled (i, noDims)))
				(r2, d2) <- compileDims d1
				return (r1*r2, d2)
			_ -> throwError $ pretty n ++ " is not a unit"
	((n,i):ds) -> do
		(r1, d1) <- compileDims (Dims $ Map.singleton n i)
		(r2, d2) <- compileDims (Dims $ Map.fromList ds)
		d_ <- (return d1) #*# (return d2)
		return (r1*r2, d_)

dims2list :: Dims -> [(Name, R)]
dims2list d = Map.toList ((Map.filter (/=0) . unDims) d)

canonicalizeDims :: Dims -> Mon Dims
canonicalizeDims d = do
	c <- compileDims d
	return $ snd c

instance EqM Dims (ErrorT String (SM.State State)) where
	(#==#) d1 d2 = do
		d1' <- d1
		d2' <- d2
		let 
			c1 = canonicalizeDims d1'
			c2 = canonicalizeDims d2'
		res <- (liftM2 (\a b -> unDims a == unDims b)) c1 c2
# ifdef DEBUG
		d1'' <- c1
		d2'' <- c2
		return $ trace (show d1'' ++ " #==# " ++ show d2'' ++ " -> " ++ show res) res
# else
		return res
# endif

instance MulM Dims (ErrorT String (SM.State State)) where
	(#*#) = std (+)

instance DivM Dims (ErrorT String (SM.State State)) where
	(#/#) e1 e2 = std (+) e1 (std' negate e2)

dou :: R -> D
dou x = (fromIntegral . numerator) x / (fromIntegral . denominator) x

rat :: D -> R
rat = toRational


instance ExpM (R, Dims) (ErrorT String (SM.State State)) where
	a #^# b = do
		(ra, da) <- a
		(rb, db) <- b
		na <- nullD da
		nb <- nullD db
		let rn = rat $ dou ra ** dou rb
		case (na, nb) of
			(True, True) -> return (rn, noDims)
			(False, True) -> if_ (denominator rb == 1)
				(do d <- da *** rb; return (rn, d))
				(do
 					(r, d) <- convertBasic (ra, da)
					d'  <- d *** rb
					return (rat $ dou r ** dou rb, d'))
			(_, False) -> throwError $ "Operation requires a number without unit: " ++ pretty (Evaled (ra, da)) ++ " #^# " ++ pretty (Evaled (rb, db))

toI :: Integral a => Ratio a -> Mon a
toI x = let a = numerator x; b = denominator x in
	if_ (b == 1) (return a) (throwError "Integer required.")

instance LogBaseM (R, Dims) (ErrorT String (SM.State State)) where 
	a #~# b = do
		(ra, da) <- a
		(rb, db) <- b
		ifM (nullD da #&&# nullD db) (return $ (rat $ logBase (dou ra) (dou rb), noDims))
			(do
				ia <- toI ra
				ib <- toI rb
				case logBaseInt ia ib of
					Nothing -> throwError "Integer exponent required for logbase on values with dimensions."
					Just re -> do
						ifM (da *** (re%1) #==# return db)
							(return ((re%1), noDims))
							(throwError "Dimensions are in other logbase relation than the numbers"))
{-
(***) :: Dims -> R -> Mon Dims
(***) (Dims d) r = do
	let res = Map.map (\x -> x * r) d
	if Map.null $ Map.filter (\x -> denominator x /= 1) res
		then return $ Dims $ Map.map (fromIntegral . numerator) res
		else throwError $ "Operation requires a number without unit: " ++ pretty (Dims d) ++ " *** " ++ pretty r
-}

(***) :: Dims -> R -> Mon Dims
(***) (Dims d) r = return (Dims $ Map.map (\x -> x * r) d)


std :: Monad m => (R -> R -> R) -> (m Dims -> m Dims -> m Dims)
std f = liftM2 (\ a b -> Dims $ Map.unionWith f (Map.filter (/=0) $ unDims a) (Map.filter (/=0) $ unDims b))

std' :: Monad m => (R -> R) -> (m Dims -> m Dims)
std' f = liftM (\ a -> Dims $ Map.map f (Map.filter (/=0) $ unDims a))

nullD :: Dims -> Mon Bool
nullD d = return d #==# return noDims

run :: State -> Command -> (Either String Result, State)
run state command = (runState $ runErrorT (process command)) state

uniInstance :: (Double -> Double) -> Mon (R, Dims) -> Mon (R, Dims)
uniInstance fr a = do
	(r, d) <- a
	return (rat . fr . dou $ r, d)

minDims :: Dims -> Mon Dims
minDims d = do
	state <- get
	let
		isDims = (\x -> case x of BasicDimension -> True; (Dimension _) -> True; _ -> False)
		neg a = justDo (return noDims #/# return a)
		allDims = map (\x -> Dims $ Map.singleton x 1) $ Map.keys $ Map.filter isDims $ getScope state

		test :: Dims -> Mon Bool
		test d' = return d' #==# return d

		merge :: [Dims] -> Dims
		merge = foldl (\a b -> justDo (return a #*# return b)) noDims

		combineUpTo :: Int -> [a] -> [[a]]
		combineUpTo i l = concatMap (\i' -> combination i' l) [1..i]

		unique :: [Dims] -> [(Dims, [Dims])]
		unique = eqClassesWith (\a b -> justDo (return a #==# return b))
		
		dims = map fst $ unique allDims

		justDo f = case fst $ (runState $ runErrorT f) state of Left e -> error e; Right b -> b

		dimsList = map merge $ combineUpTo 2 $ (map neg dims ++ dims)

	liftM (headWithDefault d) $ filterM test $ dimsList

instance AddM (R, Dims) (ErrorT String (SM.State State)) where 
	( #+# ) = linear (+)

linear :: (R -> R -> R) -> Mon (R, Dims) -> Mon (R, Dims) -> Mon (R, Dims)

linear f a b = do
	(ra, da) <- a
	b'@(_, db) <- b
	(rb, _) <- convert b' da

	ifM (return da #==# return db)
		(return (f ra rb, da))
		(throwError "Incompatible dimensions cannot be added.")

muldiv :: (R -> R -> R) -> (Mon Dims -> Mon Dims -> Mon Dims) 
	-> Mon (R, Dims) -> Mon (R, Dims) -> Mon (R, Dims)
muldiv fr fd a b = do
	(ra, da) <- a
	(rb, db) <- b
	d <- fd (return da) (return db)
	return (fr ra rb, d)

instance SubM (R, Dims) (ErrorT String (SM.State State)) where 
	( #-# ) = linear (-)

instance MulM (R, Dims) (ErrorT String (SM.State State)) where 
	( #*# ) = muldiv (*) ( #*# )

instance DivM (R, Dims) (ErrorT String (SM.State State)) where 
	( #/# ) = muldiv (/) ( #/# )

instance NegateM (R, Dims) (ErrorT String (SM.State State)) where 
	negateM = liftM (\ (r,d) -> (negate r, d))

instance ExpotM (R, Dims) (ErrorT String (SM.State State)) where 
	expM = uniInstance exp

instance LogM (R, Dims) (ErrorT String (SM.State State)) where 
	logM = uniInstance log

instance SinM (R, Dims) (ErrorT String (SM.State State)) where 
	sinM = uniInstance sin

instance CosM (R, Dims) (ErrorT String (SM.State State)) where 
	cosM = uniInstance cos

instance TanM (R, Dims) (ErrorT String (SM.State State)) where 
	tanM = uniInstance tan

instance AsinM (R, Dims) (ErrorT String (SM.State State)) where 
	asinM = uniInstance asin

instance AcosM (R, Dims) (ErrorT String (SM.State State)) where 
	acosM = uniInstance acos

instance AtanM (R, Dims) (ErrorT String (SM.State State)) where 
	atanM = uniInstance atan

instance SinhM (R, Dims) (ErrorT String (SM.State State)) where 
	sinhM = uniInstance sinh

instance CoshM (R, Dims) (ErrorT String (SM.State State)) where 
	coshM = uniInstance cosh

instance TanhM (R, Dims) (ErrorT String (SM.State State)) where 
	tanhM = uniInstance tanh

instance AsinhM (R, Dims) (ErrorT String (SM.State State)) where 
	asinhM = uniInstance asinh

instance AcoshM (R, Dims) (ErrorT String (SM.State State)) where 
	acoshM = uniInstance acosh

instance AtanhM (R, Dims) (ErrorT String (SM.State State)) where 
	atanhM = uniInstance atanh
