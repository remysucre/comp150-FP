module Main where
import System.Environment 
import System.Process
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

cmds names = (++) "cd Apps; " $ getSeq $ map getPara $ groupsOf 100 $ getApps names -- TODO: how about this?

main :: IO () 
main = do 
    fp <- getArgs
    let fn = head fp
    f <- readFile fn
    let names = words f
    system $ cmds names 
    --putStrLn $ cmds names
    return ()
