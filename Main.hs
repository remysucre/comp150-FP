{-# OPTIONS -Wall -Werror -fno-warn-name-shadowing #-}

import qualified Monads.DotWriter as DW

data Tree a = Nil | N { left :: Tree a, val :: a, right :: Tree a } deriving Show

-- Convert a Tree to a dot string
treeToDot :: Show a => Tree a -> DW.Dot ()
treeToDot Nil           = return ()
treeToDot (N Nil v Nil) = DW.addNode nv >> return ()
                          where
                              nv = DW.createNode $ show v
treeToDot (N l   v Nil) = DW.addNode nv >> treeToDot l >> DW.addNode nl >> DW.addEdge nv nl Nothing
                          where
                              nv = DW.createNode $ show v
                              nl = DW.createNode $ show $ val l
treeToDot (N Nil v r)   = DW.addNode nv >> treeToDot r >> DW.addNode nr >> DW.addEdge nv nr Nothing
                          where
                              nv = DW.createNode $ show v
                              nr = DW.createNode $ show $ val r
treeToDot n             = DW.addNode nv >> treeToDot l >> treeToDot r >> DW.addEdge nv nl Nothing >> DW.addEdge nv nr Nothing
                          where
                              l  = left n
                              v  = val n
                              r  = right n	
                              nv = DW.createNode $ show v
                              nl = DW.createNode $ show $ val l
                              nr = DW.createNode $ show $ val r          

-- Convert list into a Tree
listToTree :: [a] -> Tree a
listToTree as = listToTreeHelper as 0

-- Helper function taking an index
listToTreeHelper :: [a] -> Int -> Tree a
listToTreeHelper xs index 
                 | index >= length xs = Nil
                 | otherwise          = N l v r
                                        where
                                            v = xs !! index
                                            l = listToTreeHelper xs (2 * index + 1)
                                            r = listToTreeHelper xs (2 * (index + 1))
-- program 
main :: IO ()
main = let list = take 10 [1..]
       in putStr $ DW.toString $ treeToDot $ listToTree $ list
