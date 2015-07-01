{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, RankNTypes #-}
-----------------------------------------------------------------------------
-- | Very simple type-level integers
-----------------------------------------------------------------------------
module Data.TypeInt
    ( 
    -- * Constructors
      Zero
    , Succ

    -- * Predefined values
    , I0, I1, I2, I3, I4, I5, I6, I7, I8, I9
    , I10, I11, I12, I13, I14, I15, I16, I17, I18, I19
    , I20, I21, I22, I23, I24, I25, I26, I27, I28, I29
    , I30, I31, I32

    -- * Conversion to 'Int'
    , I
        ( num
        , induction
        , induction'
        , induction''
        )
    ) where

------------------------------------

-- | @Zero@ represents 0 at the type level

data Zero

-- | If @a@ represents the natural number @n@ at the type level then @(Succ a)@ represents @(1 + n)@ at the type level.

data Succ a

type I0 = Zero
type I1 = Succ I0
type I2 = Succ I1
type I3 = Succ I2
type I4 = Succ I3
type I5 = Succ I4
type I6 = Succ I5
type I7 = Succ I6
type I8 = Succ I7
type I9 = Succ I8
type I10 = Succ I9
type I11 = Succ I10
type I12 = Succ I11
type I13 = Succ I12
type I14 = Succ I13
type I15 = Succ I14
type I16 = Succ I15
type I17 = Succ I16
type I18 = Succ I17
type I19 = Succ I18
type I20 = Succ I19
type I21 = Succ I20
type I22 = Succ I21
type I23 = Succ I22
type I24 = Succ I23
type I25 = Succ I24
type I26 = Succ I25
type I27 = Succ I26
type I28 = Succ I27
type I29 = Succ I28
type I30 = Succ I29
type I31 = Succ I30
type I32 = Succ I31

-- | Conversion to 'Int' is achieved by the @I@ type class.

class I m where 

    num :: m -> Int
    induction :: m -> a -> (a -> a) -> a
    induction' :: a Zero -> (forall i. a i -> a (Succ i)) -> a m
    induction'' :: a Zero x -> (forall i. a i x -> a (Succ i) x) -> a m x

instance I Zero where 

    num _ = 0
    induction _ x _ = x
    induction' x _ = x
    induction'' x _ = x

instance I a => I (Succ a) where 

    num _ = 1 + num (undefined :: a)
    induction _ x f = f (induction (undefined :: a) x f)
    induction' x f = f (induction' x f)
    induction'' x f = f (induction'' x f)


