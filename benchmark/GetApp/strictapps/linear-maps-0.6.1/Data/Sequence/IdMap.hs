{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
module Data.Sequence.IdMap
    ( Seq
    , empty
    , singleton
    , (<|)
    , (|>)
    , (><)
    , fromList
    , toList
    , viewr
    , ViewR (..)
    , viewl
    , ViewL (..)
--  , size
    ) where

import Data.IdMap hiding (insert)
import qualified Data.IdMap as M

import qualified Data.List as List
import Prelude hiding (last)


------------------------------------------

data Seq a 
    = forall k . Seq
        { first :: Id k
        , last  :: Id k
        , prev  :: {-# UNPACK #-} !(Map I0 k (Id k))
        , next  :: {-# UNPACK #-} !(Map I1 k (Id k))
        , value :: {-# UNPACK #-} !(Map I2 k a)
        }
    | Empty

empty :: Seq a
empty = Empty

singleton :: forall a. a -> Seq a
singleton a = runICC f where

    f :: ICC I2 v (Seq a)
    f (v `PlusMap` n `PlusMap` _) p (i:_) = Seq
        { first = i
        , last  = i
        , prev  = p
        , next  = n
        , value = M.insert i a v
        }
{-
    f :: ICC1 I2 v (Seq a)
    f (v `PlusMap1` n `PlusMap1` _) p (i:_) = Seq
        { first = i
        , last  = i
        , prev  = p
        , next  = n
        , value = M.insert i a v
        }
-}
(><) :: Seq a -> Seq a -> Seq a
Empty >< x = x
x >< Empty = x
(Seq f l p n v) >< (Seq f' l' p' n' v') 
    = Seq
        { first = left  f
        , last  = right l'
        , prev  = M.insert (right f') (left  l)  $ fmap left p `union` fmap right p'
        , next  = M.insert (left  l)  (right f') $ fmap left n `union` fmap right n'
        , value = v `union` v'
        }
        
(<|) :: a -> Seq a -> Seq a
a <| x = singleton a >< x

(|>) :: Seq a -> a -> Seq a
x |> a = x >< singleton a



data ViewR a
    = EmptyR
    | Seq a :> !a

viewr :: Seq a -> ViewR a
viewr Empty = EmptyR
viewr (Seq f l p n v) = s' :> vl where 

    vl = v M.! l

    s' = case lookUp l p of
        Nothing -> Empty
        Just pl -> Seq
            { first = f
            , last  = pl
            , prev  = p
            , next  = M.delete pl n
            , value = v
            }


data ViewL a
    = EmptyL
    | !a :< Seq a

viewl :: Seq a -> ViewL a
viewl Empty = EmptyL
viewl (Seq f l p n v) = vf :< s' where

    vf = v M.! f

    s' = case lookUp f n of
        Nothing -> Empty
        Just nf -> Seq
            { first = nf
            , last  = l
            , prev  = M.delete nf p
            , next  = n
            , value = v
            }




----------------------------


toList :: Seq a -> [a]
toList s = case viewl s of
    EmptyL  -> []
    a :< ss -> a: toList ss

fromList :: [a] -> Seq a
fromList l = List.foldl' (|>) empty l


