{-# OPTIONS -Wall -Werror -fno-warn-name-shadowing #-}

{-
    Main: Optionally takes a number n and produces the graphviz dot
          of a Red Black tree made out of n random numbers.

    Usage: ./main [num_rands]

    Recommended Usage: ./main n | dot -Tpdf > a.pdf && pdfreader a.pdf

    NOTE: Uses a slight modified Red Black tree from Andrew Gallant's GitHub.
          File is Monads/RedBlack.hs

   (c)2013 Diogenes A. Nunez
-}

import qualified Monads.DotWriter as DW
import qualified Monads.Heap as HP
import qualified Monads.RedBlack as RB
import qualified Monads.Rand as Rand

import Control.Monad (replicateM)
import System.Random (StdGen, getStdGen)
import System.Environment (getArgs)

import Debug.Trace

-- Convert a Heap to a dot string
heapToDot :: (Show a, HP.Heap h) => h a -> DW.Dot DW.Node
heapToDot hp 
           | HP.isEmpty hp = return $ DW.createNode ""
           | otherwise =  rightDot' >> return (DW.escape nodeDot)
                          where
                              (l, r) = (HP.left hp, HP.right hp)
                              nv = DW.createNode $ show $ HP.val hp
                              nodeDot = DW.addNode (return ()) nv
                              leftDot = nodeDot >> heapToDot l
                              --rightDot = heapToDot r
                              --nodeDot = trace ("kids: (" ++ (show $ DW.escape leftDot) ++ "," ++ (show $ DW.escape rightDot) ++ ")") $ rightDot >> DW.addNode rightDot nv
                              addChild child childNode = if HP.isEmpty child 
                                                        then return ()
                                                        else DW.addEdge (DW.escape nodeDot) childNode Nothing
                              leftDot' = leftDot >> trace ("intern at left: " ++ (show $ DW.escape leftDot)) addChild l (DW.escape leftDot)
                              rightDot = leftDot' >> heapToDot r
                              rightDot' = rightDot >> trace ("intern at right: " ++ (show $ DW.escape rightDot)) addChild r (DW.escape rightDot)
                              --subDot = leftDot' >> rightDot'
             
{-
-- Crawl the tree and add colors to every Node in the Dot structure
addColorToTree :: Show a => RB.RBTree a -> DW.Dot ()
addColorToTree RB.Empty = return ()
addColorToTree (RB.Node RB.Red l v r) = addColorToTree l >> addColorToTree r >> DW.addColor nv DW.Red >> DW.addStyle nv DW.Bold
                                        where nv = DW.createNode $ show v
addColorToTree (RB.Node RB.Black l v r) = addColorToTree l >> addColorToTree r >> DW.addColor nv DW.Black
                                        where nv = DW.createNode $ show v
-}

-- Discern how many random numbers the user wants
getRandListLen :: [String] -> Int
getRandListLen [] = 100 -- Default length of list of random numbers
getRandListLen strs = read $ head strs

-- Print the dot string to stdout
printRBTree :: StdGen -> Int -> IO ()
printRBTree gen len = putStr $ DW.toString $ (heapToDot tree) -- >> addColorToTree tree
                      where 
                          list = fst $ (Rand.eval $ replicateM len Rand.zhe) gen
                          tree = RB.rbify $ list

-- main program 
main :: IO ()
main = do
         args <- getArgs;
         gen <- getStdGen;
         printRBTree gen $ getRandListLen args
