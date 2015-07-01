module Data.Sequence.IdMap2
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

import Data.IdMap.Static hiding (insert)
import qualified Data.IdMap.Static as M

import qualified Data.List as List
import Prelude hiding (last)


------------------------------------------

data Seq a 
    = forall k . Seq
        { first :: (k :. a)
        , last  :: (k :. a)
        , prev  :: {-# UNPACK #-} !(Map I0 k (k :. a))
        , next  :: {-# UNPACK #-} !(Map I1 k (k :. a))
        }
    | Empty

empty :: Seq a
empty = Empty

singleton :: forall a. a -> Seq a
singleton a = runICC f where

    f :: ICC I1 v (Seq a)
    f (n `PlusMap` _) p (i:_) = Seq
        { first = i'
        , last  = i'
        , prev  = p
        , next  = n
        }
     where
        i' = i :. a


(><) :: Seq a -> Seq a -> Seq a
Empty       >< x        = x
x           >< Empty    = x
Seq f l p n >< Seq f' l' p' n'
    = Seq
        { first = left2  f
        , last  = right2 l'
        , prev  = M.insert (right2 f') (left2  l)  $ fmap left2 p `union` fmap right2 p'
        , next  = M.insert (left2  l)  (right2 f') $ fmap left2 n `union` fmap right2 n'
        }
        
(<|) :: a -> Seq a -> Seq a
a <| x = singleton a >< x

(|>) :: Seq a -> a -> Seq a
x |> a = x >< singleton a



data ViewR a
    = EmptyR
    | Seq a :> a

viewr :: Seq a -> ViewR a
viewr Empty = EmptyR
viewr (Seq f l@(_ :. a) p n) = s' :> a where 

    s' = case lookUp l p of
        Nothing -> Empty
        Just pl -> Seq
            { first = f
            , last  = pl
            , prev  = p
            , next  = M.delete pl n
            }


data ViewL a
    = EmptyL
    | a :< Seq a

viewl :: Seq a -> ViewL a
viewl Empty = EmptyL
viewl (Seq f@(_ :. a) l p n) = a :< s' where

    s' = case lookUp f n of
        Nothing -> Empty
        Just nf -> Seq
            { first = nf
            , last  = l
            , prev  = M.delete nf p
            , next  = n
            }


----------------------------


toList :: Seq a -> [a]
toList s = case viewl s of
    EmptyL  -> []
    a :< ss -> a: toList ss

fromList :: [a] -> Seq a
fromList l = List.foldl' (|>) empty l




