{-# LANGUAGE KindSignatures #-} 
-----------------------------------------------------------------------------
-- | @Functor2@ and @Functor3@ type classes
-----------------------------------------------------------------------------
module Control.Functor (
      Functor2(fmap2)
    , Functor3(fmap3)
    ) where


class Functor2 (f :: * -> * -> *)  where
    fmap2 :: (a -> b) -> f a x -> f b x

class Functor3 (f :: * -> * -> * -> *)  where
    fmap3 :: (a -> b) -> f a x y -> f b x y


