module Main where
import System.Environment 
import System.Process
import Data.List

sortLines :: [String] -> [String]
sortLines = sortBy compare

--getApp :: String -> IO ExitCode
getApp name = system $ "cabal get " ++ name

cmds = map (" & cabal get "++)

noAmp (a:b:cs) = cs

main :: IO () 
main = do 
    fp <- getArgs
    let fn = head fp
    f <- readFile fn
    let names = words f
    system $ ("cd Apps & " ++ (noAmp $ unwords $ cmds names))
    --putStrLn $ show $ length names
    return ()
