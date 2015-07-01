import Data.LinkMap

type Link i k = LinkMap i k ()


type TEq k = (Id k, Id k)

data TypeNode k
	= TyApp (Id k) (Id k)
	| TyVar
	| TyCon String
		deriving Eq


type Type k = Id k -> TypeNode k

solveEqs
	:: I i => Link i k
	-> Type k
	-> [TEq k]
	-> ([TEq k], Link i k)	-- (faild unifications, )


solveEqs lm f eqs = foldl solveEq ([], lm) eqs where

	solveEq (skipped, lm) (x, y) 
		| same lm x y	= (skipped, lm)
		| otherwise	= case (f $ follow lm x, f $ follow lm y) of

			(TyVar, _)
				-> (skipped, link x y lm)

			(_, TyVar)
				-> (skipped, link y x lm)

			(TyApp a b, TyApp c d)
				-> solveEq (solveEq (skipped, link x y lm) (a, c)) (b, d)

			(TyCon a, TyCon b)	| a == b
				-> (skipped, link x y lm)

			_	-> ((x, y): skipped, lm)


