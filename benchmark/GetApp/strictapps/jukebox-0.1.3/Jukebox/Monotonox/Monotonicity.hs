{-# LANGUAGE TypeOperators #-}
module Jukebox.Monotonox.Monotonicity where

import Prelude hiding (lookup)
import Jukebox.Name
import Jukebox.Form hiding (Form, clause, true, false, conj, disj)
import Jukebox.HighSat
import Jukebox.NameMap as NameMap
import Jukebox.Utils
import Data.Hashable
import Control.Monad

data Extension = TrueExtend | FalseExtend | CopyExtend deriving Show

data Var = FalseExtended Function | TrueExtended Function deriving (Eq, Ord)

instance Hashable Var where
  hashWithSalt s = hashWithSalt s . convert
    where convert (FalseExtended x) = Left x
          convert (TrueExtended x) = Right x

annotateMonotonicity :: Problem Clause -> IO (Problem Clause)
annotateMonotonicity prob = do
  m <- monotone (map what (open prob))
  let f O = O
      f ty =
        case lookup (name ty) m of
          Nothing -> ty
          Just{} -> ty { tmonotone = Finite 0 }
  return (fmap (mapType f) prob)

monotone :: [Clause] -> IO (NameMap (Type ::: Maybe (NameMap (Function ::: Extension))))
monotone cs = runSat watch tys $ do
  let fs = functions cs
  mapM_ (clause . toLiterals) cs
  fmap NameMap.fromList . forM tys $ \ty -> atIndex ty $ do
    r <- solve []
    case r of
      False -> return (ty ::: Nothing)
      True -> do
        m <- model
        return (ty ::: Just (fromModel fs ty m))
  where watch (FalseExtended f) =
          addForm (disj [Lit (Neg (FalseExtended f)),
                         Lit (Neg (TrueExtended f))])
        watch _ = return ()
        tys = types' cs

fromModel :: [Function] -> Type -> (Var -> Bool) -> NameMap (Function ::: Extension)
fromModel fs ty m = NameMap.fromList [ f ::: extension f m | f <- fs, typ f == O, ty `elem` args (rhs f) ]

extension :: Function -> (Var -> Bool) -> Extension
extension f m =
  case (m (FalseExtended f), m (TrueExtended f)) of
    (False, False) -> CopyExtend
    (True, False) -> FalseExtend
    (False, True) -> TrueExtend

clause :: [Literal] -> Sat Var Type ()
clause ls = mapM_ (literal ls) ls

literal :: [Literal] -> Literal -> Sat Var Type ()
literal ls (Pos (t :=: u)) = atIndex (typ t) $ do
  addForm (safe ls t)
  addForm (safe ls u)
literal ls (Neg (_ :=: _)) = return ()
literal ls (Pos (Tru (p :@: ts))) =
  forM_ ts $ \t -> atIndex (typ t) $ addForm (disj [safe ls t, Lit (Neg (FalseExtended p))])
literal ls (Neg (Tru (p :@: ts))) =
  forM_ ts $ \t -> atIndex (typ t) $ addForm (disj [safe ls t, Lit (Neg (TrueExtended p))])

safe :: [Literal] -> Term -> Form Var
safe ls (Var x) = disj [ guards l x | l <- ls ]
safe _ _ = true

guards :: Literal -> Variable -> Form Var
guards (Neg (Var _ :=: Var _)) _ = error "Monotonicity.guards: found a variable inequality X!=Y after clausification"
guards (Neg (Var x :=: _)) y | x == y = true
guards (Neg (_ :=: Var x)) y | x == y = true
guards (Pos (Tru (p :@: ts))) x | Var x `elem` ts = Lit (Pos (TrueExtended p))
guards (Neg (Tru (p :@: ts))) x | Var x `elem` ts = Lit (Pos (FalseExtended p))
guards _ _ = false
