{-# OPTIONS -Wall -Werror -fno-warn-name-shadowing #-}

import DotWriter as DW

data Tree a = Nil | N { left :: Tree a, val :: a, right :: Tree a } deriving Show

-- Convert a Tree to a dot string
treeToDot :: Show a => Tree a -> Dot ()
treeToDot Nil           = return ()
treeToDot (N Nil v Nil) = addNode nv >> return ()
                          where
                              nv = createNode $ show v
treeToDot (N l   v Nil) = addNode nv >> treeToDot l >> addNode nl >> addEdge nv nl Nothing
                          where
                              nv = createNode $ show v
                              nl = createNode $ show $ val l
treeToDot (N Nil v r)   = addNode nv >> treeToDot r >> addNode nr >> addEdge nv nr Nothing
                          where
                              nv = createNode $ show v
                              nr = createNode $ show $ val r
treeToDot n             = addNode nv >> treeToDot l >> treeToDot r >> addEdge nv nl Nothing >> addEdge nv nr Nothing
                          where
                              l  = left n
                              v  = val n
                              r  = right n	
                              nv = createNode $ show v
                              nl = createNode $ show $ val l
                              nr = createNode $ show $ val r          

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
       in putStr $ toString $ treeToDot $ listToTree $ list
