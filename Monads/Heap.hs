{-# OPTIONS -Wall -Werror -fno-warn-name-shadowing #-}

module Monads.Heap (
Heap(..)
, heapToDot
) where

import Monads.DotWriter as DW

-- Idea taken from Andrew Gallant
class Heap h where
    --empty :: h a
    left :: h a -> h a
    right :: h a -> h a
    val :: h a -> a
    isEmpty :: h a -> Bool

-- Convert a Heap to a dot string
heapToDot :: (Show a, Heap h) => h a -> DW.Dot ()
--heapToDot empty = return ()
heapToDot hp 
           | isEmpty hp = return ()
           | otherwise =  nodeDot >> leftDot' >> rightDot'
                          where
                              (l, r) = (left hp, right hp)
                              nv = DW.createNode $ show $ val hp
                              nodeDot = DW.addNode nv
                              leftDot = heapToDot l
                              rightDot = heapToDot r
                   --(leftDot, rightDot) = fmap heapToDot (l, r)
                   --addChild :: h a -> DW.Dot() -> DW.Dot ()
                              addChild child childDot = if isEmpty child 
                                                        then childDot
                                                        else childDot >> DW.addEdge nv (DW.createNode $ show $ val child) Nothing
                              leftDot' = addChild l leftDot
                              rightDot' = addChild r rightDot
             
