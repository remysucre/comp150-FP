{-# OPTIONS -Wall -Werror -fno-warn-name-shadowing #-}

import qualified Monads.DotWriter as DW
import qualified Monads.Heap as HP
import qualified Monads.RedBlack as RB
import qualified Monads.Rand as Rand
import Control.Monad (replicateM)
import System.Random (StdGen, getStdGen)
import System.Environment (getArgs)

addColorToTree :: Show a => RB.RBTree a -> DW.Dot ()
addColorToTree RB.Empty = return ()
addColorToTree (RB.Node RB.Red l v r) = addColorToTree l >> addColorToTree r >> DW.addColor nv DW.Red >> DW.addStyle nv DW.Bold
                                        where nv = DW.createNode $ show v
addColorToTree (RB.Node RB.Black l v r) = addColorToTree l >> addColorToTree r >> DW.addColor nv DW.Black
                                        where nv = DW.createNode $ show v

getRandListLen :: [String] -> Int
getRandListLen [] = 100
getRandListLen strs = read $ head strs

printRBTree :: StdGen -> Int -> IO ()
printRBTree gen len = putStr $ DW.toString $ (HP.heapToDot tree) >> addColorToTree tree
                      where 
                          list = fst $ (Rand.escape $ replicateM len Rand.zhe) gen
                          tree = RB.rbify $ list

-- main program 
main :: IO ()
main = do
         args <- getArgs;
         gen <- getStdGen;
         printRBTree gen $ getRandListLen args
