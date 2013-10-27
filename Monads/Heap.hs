{-# OPTIONS -Wall -Werror -fno-warn-name-shadowing #-}

{-
    Heap: Typeclass to characterize a tree-like structure, incorrectly
          named as Heap. Also includes any functions that are necessary
          for the Heap.
 
          This idea is taken from Andrew Gallant's Dotter monad.
-}
module Monads.Heap (
Heap(..)
) where

-- Idea taken from Andrew Gallant
class Heap h where
    --empty :: h a
    left :: h a -> h a
    right :: h a -> h a
    val :: h a -> a
    isEmpty :: h a -> Bool

