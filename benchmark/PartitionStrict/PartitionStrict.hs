module Main where
import System.Environment 
import System.Process
import System.Directory
import Data.List

-- download packages listed in named file from cmd

getApps :: [String] -> [String]
getApps = map ("cabal get " ++)

getCmd :: String -> [String] -> String
getCmd mode = concat . (intersperse mode)   -- TODO: can this be pointfree?

getPara :: [String] -> String
getPara = getCmd " & "

getSeq :: [String] -> String
getSeq = getCmd " ; "

groupsOf :: Int -> [a] -> [[a]]
groupsOf n [] = []
groupsOf n xs = take n xs : (groupsOf n $ drop n xs)

cmds names = (++) "cd Apps; mkdir strictapps; " $ getSeq $ map getPara $ groupsOf 1 $ moves names

moves = map ("../MoveIfStrict "++) 

main :: IO () 
main = do 
    fp <- getArgs
    let dir = head fp
    packs <- getDirectoryContents dir
    --system $ cmds packs
    putStrLn $ cmds packs
    return ()
