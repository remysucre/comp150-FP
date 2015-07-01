{-# LANGUAGE CPP, TypeOperators, EmptyDataDecls, RankNTypes #-}
module Data.Subtyping
    ( (:|:)
    , Incl (left, right)
    , Incl2 (left2, right2)
    ) where

import Unsafe.Coerce (unsafeCoerce)


-- | @(:|:)@ is intended to be used only in data type indexes. 
-- @T (a :|: b)@ represents the disjoint union of the sets represented by @T a@ and @T b@.

-- @T (a :|: b)@ is a subtype of both @T a@ and @T b@.
-- There is no subtyping in Haskell, so the 'left' and 'right' functions should be used to express
-- the subtyping coercions.
-- Examples: 
-- 
-- * If @x :: T a@ then @'left' x :: T (a :|: b)@.
--
-- * If @x :: T b@ then @'right' x :: T (a :|: b)@.
--
-- * If @(x, y) :: (T a, T b)@ then @['left' x, 'right' y] :: [T (a :|: b)]@.
--
-- * If @x :: T a@ then @['left' x, 'right' x] :: [T (a :|: a)]@.
--
-- * If @x :: [T a]@ then @('fmap' 'left' x) :: [T (a :|: b)]@.
--
-- * If @x :: [(T a, 'Int')]@ then @'fmap' ('fmap2' 'left') x :: [(T (a :|: b), 'Int')]@ for all @b@.
--
-- * If @x :: 'Either' (T a) (T b)@ then @'fmap2' ('fmap' 'right' x) :: 'Either' (T (a :|: b)) (T b)@.


infixr 2 :|:

data a :|: b

class Incl c where

    left  :: c a -> c (a :|: b)
    right :: c b -> c (a :|: b)

class Incl2 c where

    left2  :: c a x -> c (a :|: b) x
    right2 :: c b x -> c (a :|: b) x


#ifndef __PURE__
{-# RULES
"fmap/left"  forall x . fmap left  x = unsafeCoerce x
"fmap/right" forall x . fmap right x = unsafeCoerce x
 #-}
#endif

