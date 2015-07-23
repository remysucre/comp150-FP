{-# LANGUAGE BangPatterns #-}
import Harness

-- The details are not important, just that:
--  1. It's spine strict, but
--  2. It's lazy in its values.
data SpineStrictList a = Nil | Cons a !(SpineStrictList a) 
ssFromList [] l = l
ssFromList (x:xs) l = ssFromList xs (Cons x l)
-- Can stack overflow: more usual spine strict structures will
-- attempt to make sure their spine grows slowly.
ssMap _ Nil = Nil
ssMap f (Cons x xs) = Cons (f x) (ssMap f xs)

main = do
    let x = ssFromList (zip [1..100] (repeat 1)) Nil
    evaluate (loop 80000 x)

loop 0 x = x
loop n x = loop (n-1) (ssMap permute x)

permute (y, z) = (y * 2 + 4 * z, z + 1)

{- reduction path: 

-> evaluate (loop 2 x)
-> toWhnf (loop 2 x)
-> toWhnf (loop 1 (ssMap permute x))
...
-> toWhnf (loop 0 (ssMap permute (ssMap permute x)))
-> toWhnf (ssMap permute (ssMap permute  x))
-> toWhnf (ssMap permute (toCons $ ssMap permute x))
-> toWhnf (s p (toCons s p $ toCons $ ssFromList (zip [1..100] (repeat 1) Nil)))
-> ... ssFromList $ toCons (zip [1..100] (repeat 1)) Nil
... ** NOTE following 3 lines to show ssFromList expands zip... to spine-strict
-> ... ssFromList $ to: (1,1):ns Nil
-> ... ssFromList (1,1):ns Nil
-> toWhnf (s p (toCons s p $ toCons $ ssFromList (1,1):ns Nil
...
->  toWhnf (s p (toCons s p $ toCons $ (Cons (1,1) (Cons (1,2)...))
->  toWhnf (s p (toCons s p (Cons (1,1) (Cons (1,2)...))
->  toWhnf (s p (toCons ssMap permute (Cons (1,1) (Cons (1,2)...))
->  toWhnf (s p (toCons ssMap permute (Cons (1,1) (Cons (1,2)...))
->  toWhnf (s p (toCons Cons permute (1,1) ssMap (Cons (1,2)...)))
->  toWhnf (ssMap permute $ Cons permute (1,1) ssMap permute (Cons ...))
->  toWhnf Cons (permute $ permute (1,1)) (ssMap permute (ssMap permute ...))
-> Cons _ _

TL;DR: evaluate wants to reduce its argument to whnf as follows:
1. reduce `loop` to a manifold of `ssMap permute` wrapped on x
2. reduce x to spine-strict
3. unwraps ssMap permute while only unwrapping one Cons of x, because the result of ssMap is in Whnf

Therefore, thunks are created by: 
1. reducing loop
2. reducing x

TODO:
show that if x were spine-strict all the time it creates more thunk. 
-}
-- Fixes:
--  * Publish a strict version of ssMap
--  * Make the structure spine lazy (e.g. use a traditional list)
--  * rnf the structure every iteration (not recommended!)
-- Not fixes:
--  * Bang pattern permute
-- Perturbations:
--  * Write ssMap in the same style as ssFromList, to avoid stack
--    overflows.  Then you cannot simply make the structure lazy,
--    since the implicit reverse automatically makes the structure spine
--    strict.
-- Note: This example involves 100 moderately long thunk chains
