{-# LANGUAGE BangPatterns #-}
import Control.Concurrent.MVar
import Data.List

main = main10

-- http://hackage.haskell.org/trac/ghc/ticket/2607
data Tree a = Tree a [Tree a] deriving Show
data TreeEvent = Start String
                | Stop
                | Leaf String
                deriving Show
main10 = print . snd . build $ Start "top" : replicate 500000 (Leaf "sub")
--main10 = print . build $ Start "top" : replicate 50 (Leaf "sub")
type UnconsumedEvent = TreeEvent        -- Alias for program documentation
build :: [TreeEvent] -> ([UnconsumedEvent], [Tree String])
build (Start str : es) =
        let (es', !subnodes) = build es
            (spill, !siblings) = build es'
        in (spill, (Tree str subnodes : siblings))
build (Leaf str : es) =
        let (spill, !siblings) = build es
        in (spill, Tree str [] : siblings)
build (Stop : es) = (es, [])
build [] = ([], [])
