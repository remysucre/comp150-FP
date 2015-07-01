{-# LANGUAGE
 BangPatterns
 #-}
module Context where

import AST
import Substitution
import Data.Monoid
import Data.Functor
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map)
import Data.Set (Set)
import Control.Monad.State (StateT, runStateT, modify, get, put)
import Control.Monad.RWS (RWST, ask, local, censor, runRWST, get, put,listen)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Cont
import Choice
import Data.List
import Debug.Trace

--------------------
---  context map ---
--------------------
type ContextMap = Map Name ((Bool,Integer),Type)
type ContextMapT = Map Name Type


--------------------------------
---  constraint context list ---
--------------------------------
data Binding = Binding { elmQuant :: Quant
                       , elmName :: Name
                       , elmType :: Type
                       , elmPrev :: Maybe Name
                       , elmNext :: Maybe Name
                       } deriving (Show)
               
instance Subst Binding where
  substFree sub f b = b { elmType = substFree sub f $! elmType b }
                    
data Context = Context { ctxtHead :: Maybe Name
                       , ctxtMap  :: Map Name Binding
                       , ctxtTail :: Maybe Name
                       } deriving (Show)
                                  
instance Subst Context where               
  substFree sub f b = b { ctxtMap = substFree sub f <$> ctxtMap b }

lookupWith s a ctxt = case M.lookup a ctxt of
  Just r -> r
  Nothing -> error s



emptyContext = Context Nothing mempty Nothing

-- assumes the element is not already in the context, or it is and the only thing that is changing is it's type.
addToContext :: String -> Context -> Binding -> Context
addToContext s (Context Nothing ctxt Nothing) elm@(Binding _ nm _ Nothing Nothing) | M.null ctxt = checkContext (s++"\naddToCtxt N N: ") $ 
                                                                                                 Context (Just nm) (M.singleton nm elm) (Just nm)
addToContext s c (Binding _ _ _ Nothing Nothing) = error $ "context not empty so can't add to tail: "++show c
addToContext s c@(Context h ctxt t) elm@(Binding _ nm _ t'@(Just p) Nothing) | t' == t = checkContext (s++"\naddToCtxt J N: "++show elm ++ "\n\tOLD CONTEXT: "++show c) $ 
  Context h (M.insert p t'val $ M.insert nm elm $ ctxt) (Just nm)
  where t'val = (lookupWith (s++" looking up p ctxt") p ctxt) { elmNext = Just nm }
addToContext s _ (Binding _ _ _ _ Nothing) = error "can't add this to tail"
addToContext s (Context h ctxt t) elm@(Binding _ nm _ Nothing h'@(Just n)) | h' == h = checkContext (s++"\naddToCtxt N J: ") $ 
  Context (Just nm) (M.insert n h'val $ M.insert nm elm $ ctxt) t
  where h'val = (lookupWith (s++" looking up n ctxt") n ctxt) { elmPrev = Just nm }
addToContext s _ (Binding _ _ _ Nothing _) = error "can't add this to head"
addToContext s ctxt@Context{ctxtMap = cmap} elm@(Binding _ nm _ (Just p) (Just n)) = checkContext (s++"\naddToCtxt J J: ") $ 
  ctxt { ctxtMap = M.insert n n'val $ M.insert p p'val $ M.insert nm elm $ cmap }
  where n'val = (lookupWith (s++" looking up n cmap") n cmap) { elmPrev = Just nm }
        p'val = (lookupWith (s++" looking up p cmap") p cmap) { elmNext = Just nm }
  
removeFromContext :: Name -> Context -> Context
removeFromContext nm ctxt@(Context h cmap t) = case M.lookup nm cmap of
  Nothing -> checkContext "removing: nothing" $ ctxt
  Just Binding{ elmPrev = Nothing, elmNext = Nothing } -> emptyContext
  Just Binding{ elmPrev = Nothing, elmNext = Just n } -> isSane (Just nm == h) $ checkContext "removing: N J" $ Context (Just n) (M.insert n h' $ M.delete nm cmap) t
    where h' = (lookupWith "attempting to find new head" n cmap) { elmPrev = Nothing }
  Just Binding{ elmPrev = Just p, elmNext = Nothing } -> isSane (Just nm == t) $ checkContext "removing: J N" $ Context h (M.insert p t' $ M.delete nm cmap) (Just p)
    where t' = (lookupWith "attempting to find new tail" p cmap) { elmNext = Nothing }
  Just Binding{elmPrev = Just cp, elmNext = Just cn } -> case () of
    _ | h == t -> checkContext "removing: J J | h == t " $ Context Nothing mempty Nothing
    _ | h == Just nm -> checkContext "removing: J J | h == Just nm  " $ Context (Just cn) (n' $ M.delete nm cmap) t
    _ | t == Just nm -> checkContext "removing: J J | t == Just nm  " $ Context h   (p' $ M.delete nm cmap) (Just cp)
    _ -> checkContext ("removing: J J | h /= t \n\t"++show ctxt) $ Context h (n' $ p' $ M.delete nm cmap) t
    where n' = M.insert cn $ (lookupWith "looking up a cmap for n'" cn cmap) { elmPrev = Just cp }
          p' = M.insert cp $ (lookupWith "looking up a cmap for p'" cp cmap ) { elmNext = Just cn }
  where isSane bool a = if bool then a else error "This doesn't match intended binding"

addAfter s t quant nm tp ctxt@Context{ctxtMap = cmap} = checkContext "addAfter" $ do
  let top = lookupWith ("looking up "++t++"\n\t in context: "++show cmap++"\n\t"++s) t cmap
  addToContext ("ADD_AFTER: "++s) ctxt $ Binding quant nm tp (Just $ elmName top) (elmNext top)

addBefore s b quant nm tp ctxt@Context{ctxtMap = cmap} = checkContext "addBefore" $ do
  let bot = lookupWith ("looking up "++b++"\n\t in context: "++show cmap++"\n\t"++s) b cmap
  addToContext ("ADD_BEFORE: "++s) ctxt $ Binding quant nm tp (elmPrev bot) (Just $ elmName bot)

addToHead s quant nm tp ctxt@Context{ctxtMap = cmap} = case M.lookup nm cmap of 
  Nothing -> addToContext ("ATH: "++s) ctxt $ Binding quant nm tp Nothing (ctxtHead ctxt)
  Just (Binding{ elmQuant = quant', elmType = tp'}) | quant' == quant && tp' == tp && quant == Forall -> 
    addToContext ("ATH2: "++s) ctxt' $ Binding quant nm tp Nothing (ctxtHead ctxt')
    where ctxt' = removeFromContext nm ctxt
  _ -> error $ "Can't add to head, already in context: "++show nm++" : "++show tp++"\n@"++show ctxt
  
addToTail s quant nm tp ctxt@Context{ctxtMap = cmap} = case M.lookup nm cmap of
  Nothing -> addToContext ("ATT: "++s) ctxt $ Binding quant nm tp (ctxtTail ctxt) Nothing
  Just (Binding{ elmQuant = quant', elmType = tp'}) | quant' == quant && tp' == tp && quant == Forall -> ctxt
  _ -> error $ "Can't add to tail, already in context: "++show nm++" : "++show tp++"\n@"++show ctxt

removeHead ctxt = case ctxtHead ctxt of 
  Nothing -> ctxt
  Just a -> removeFromContext a ctxt

removeTail ctxt = case ctxtTail ctxt of 
  Nothing -> ctxt
  Just a -> removeFromContext a ctxt

getTail (Context _ ctx (Just t)) = lookupWith "getting tail" t ctx
getTail (Context _ _ Nothing) = error "no tail!"

getHead (Context (Just h) ctx _) = lookupWith "getting head" h ctx
getHead (Context Nothing _ _) = error "no head"

-- gets the list of bindings after (below) a given binding
getAfter s bind ctx = tail $ getAfterInclusive s bind ctx            
getAfterInclusive s bind ctx@(Context{ ctxtMap = ctxt }) = gb bind
  where gb ~(Binding quant nm ty _ n) = (quant, (nm,ty)):case n of
          Nothing -> []
          Just n -> gb $ case M.lookup n ctxt of 
            Nothing -> error $ "element "++show n++" not in map \n\twith ctxt: "++show ctx++" \n\t for bind: "++show bind++"\n\t"++s
            Just c -> c

-- gets the list of bindings before (above) a given binding
getBefore s bind ctx = tail $ getBeforeInclusive s bind ctx            
getBeforeInclusive s bind ctx@(Context{ ctxtMap = ctxt }) = gb bind
  where gb ~(Binding quant nm ty p _) = (quant, (nm,ty)):case p of
          Nothing -> []
          Just p -> gb $ case M.lookup p ctxt of 
            Nothing -> error $ "element "++show p++" not in map \n\twith ctxt: "++show ctx++" \n\t for bind: "++show bind++"\n\t"++s
            Just c -> c

checkContext _ c = c
{-
checkContext _ c@(Context Nothing _ Nothing) = c
checkContext s ctx = foldr (\v c -> seq (checkEquals v) c) ctx $ zip st (reverse $ ta)
  where st = getBeforeInclusive s (getTail ctx) ctx
        ta = getAfterInclusive s (getHead ctx) ctx
        checkEquals (a,b) | (a == b) = ()
        checkEquals (a,b) = error $ s++" \n\tNOT THE SAME" ++show (a,b) ++ " \n\t IN "++show ctx
-}

-------------------------
---  Traversal Monad  ---
-------------------------
data ContextState = ContextState { stateNum :: !Integer
                                 , stateCtxt :: Context 
                                 }
                    
emptyState = ContextState 0 emptyContext



instance ValueTracker ContextState where
  putValue i c = c { stateNum = i }
  takeValue c = stateNum c
  
type Env = RWST ContextMap Constraint ContextState Choice

isolateForFail m = do
  s <- get
  c <- m
  case c of
    Nothing -> do
      put s
      return Nothing
    _ -> return c

------------------------        
-- env with a context --        
------------------------


getElm :: String -> Name -> Env (Either Binding Spine)
getElm _ !x | isChar x = do
  return $ Right $ var "char"
getElm s !x = do
  ty <- lookupConstant x
  case ty of
    Nothing -> Left <$> (\ctxt -> lookupWith ("looking up "++x++"\n\t in context: "++show ctxt++"\n\t"++s) x ctxt) <$> ctxtMap <$> stateCtxt <$> get
    Just a -> return $ Right a
  
getBindings :: Binding -> Env [(Name,Type)]
getBindings bind = fmap snd <$> getQuantBindings bind

getBindingsInclusive :: Binding -> Env [(Name,Type)]
getBindingsInclusive bind = fmap snd <$> getQuantBindings bind

-- | This gets all the bindings outside of a given bind and returns them in a list (not including that binding).
getQuantBindings :: Binding -> Env [(Quant, (Name,Type))]
getQuantBindings bind = do
  ctx <- stateCtxt <$> get
  return $ getBefore "IN: getQuantBindings" bind ctx
  
getQuantBindingsInclusive :: Binding -> Env [(Quant, (Name,Type))]
getQuantBindingsInclusive bind = do
  ctx <- stateCtxt <$> get
  return $ getBeforeInclusive "IN: getQuantBindingsInclusive" bind ctx  
    
  
getBindingsBetween :: Binding -> Binding -> Env [(Name,Type)]
getBindingsBetween top bottom = do
  above <- getBindings top
  target <- getBindings bottom
  let aboveSet = S.insert (elmName top) $ S.fromList $ fst <$> above 
  return $ filter (\n -> not $ S.member (fst n) aboveSet) target
  
getAnExist :: Env (Maybe (Name,Type))
getAnExist = do
  ctx <- stateCtxt <$> get
  let til = getTail ctx
      last = (elmQuant til, (elmName til, elmType til))
  return $ case ctx of
    Context _ _ Nothing -> Nothing
    _ -> snd <$> find (\(q,_) -> q == Exists) (last:getBefore "IN: getAnExist" til ctx)

-- | `getAllBindings` gets all bindings, listed from tightest to loosest
-- ie, ∀a:t∃b:t will return [(∃,(b,t)), (∀,(a,t))]
getAllBindings = do
  ctx <- stateCtxt <$> get
  case ctx of
    Context _ _ Nothing -> return []
    _ -> getQuantBindingsInclusive $ getTail ctx

    
getForalls :: Env ContextMap
getForalls = do
  ctx <- ctxtMap <$> stateCtxt <$> get
  return $ anonymous <$> elmType <$> M.filter (\q -> elmQuant q == Forall) ctx

getForallsAfter :: Binding -> Env (S.Set Name)
getForallsAfter bind = do
  ctx <- stateCtxt <$> get
  return $ S.fromList $ map (fst . snd) $ filter ((== Forall) . fst) $ getAfter "IN: getForalls" bind ctx
  
getExistsAfter :: Binding -> Env (S.Set Name)
getExistsAfter bind = do
  ctx <- stateCtxt <$> get
  return $ S.fromList $ map (fst . snd) $ filter ((== Exists) . fst) $ getAfter "IN: getExistsAfter" bind ctx      

getExists :: Env ContextMap
getExists = do
  ctx <- ctxtMap <$> stateCtxt <$> get
  return $ anonymous <$> elmType <$> M.filter (\q -> elmQuant q == Exists) ctx

getConstants :: Env ContextMap
getConstants = ask  

clearContext :: Env ()
clearContext = do
  ContextState i _ <- get
  put $ ContextState i emptyContext


getFullCtxt :: Env ContextMap
getFullCtxt = do
  constants <- getConstants
  ctx <- ctxtMap <$> stateCtxt <$> get
  return $ M.union (anonymous <$> elmType <$> ctx) constants

getVariablesBeforeExists :: Name -> Env ContextMap
getVariablesBeforeExists nm = do
  constants <- getConstants
  ctx <- stateCtxt <$> get  
  let bind = ctxtMap ctx M.! nm
  return $ M.union constants 
         $ M.fromList $ (\(nm,v) -> (nm, anonymous v)) <$> snd <$> getBefore "IN: getVariablesBeforeExists" bind ctx
  

modifyCtxt :: (Context -> Context) -> Env ()
modifyCtxt f = modify $ \m -> m { stateCtxt = f $ stateCtxt m }


-------------------------
---  traversal monads ---
-------------------------
lookupConstant :: Name -> Env (Maybe Type)
lookupConstant x = fmap snd <$> (M.lookup x) <$> ask 

type TypeChecker = ContT Spine Env

typeCheckToEnv :: TypeChecker Spine -> Env (Spine,Constraint)
typeCheckToEnv m = listen $ runContT m return




addToEnv :: (Name -> Type -> Constraint -> Constraint) -> Name -> Type -> TypeChecker a -> TypeChecker a
addToEnv e x ty = mapContT (censor $ e x ty) . liftLocal ask local (M.insert x $ anonymous ty)
