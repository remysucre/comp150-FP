module HasBang (hasBang) where

import System.IO.Unsafe
import Language.Haskell.Exts
import System.FilePath.Find
import System.Environment 
import System.Process
import Data.Functor
import Control.Applicative
import Data.List
import FindHs

hasBang :: String -> IO Bool
hasBang filePath = do 
            program <- readFile filePath
            return $ hasBangDecl $ getDecl (getModule filePath program)

getDecl (Module _ _ _ _ _ _ d) = d

hasBangDecl :: [Decl] -> Bool
hasBangDecl [] = False
hasBangDecl (d:ds) = case d of
                             FunBind ms -> hasBangMatch ms || hasBangDecl ds
                             _ -> hasBangDecl ds              

getPat :: Match -> [Pat]
getPat (Match _ _ p _ _ _) = p 

hasBangMatch :: [Match] -> Bool
hasBangMatch [] = False
hasBangMatch (m:ms) = (hasBangPat $ getPat m) || hasBangMatch ms
                             
hasBangPat :: [Pat] -> Bool
hasBangPat [] = False
hasBangPat (p:ps) = case p of
                        PBangPat _ -> True
                        _          -> hasBangPat ps

getModule :: String -> String -> Module
getModule filePath program = fromParseResult $ parseFileContentsWithMode mode program
                             where
                                      bangPatternsExt = parseExtension "BangPatterns"
                                      mode = ParseMode filePath Haskell2010 [bangPatternsExt] True True Nothing
{-
DirHasStrict dir
-- call shell to get a list of paths
-- run has strict on all paths

cd dir; bash ../AllHs.sh > ../temp
srcs <- read temp
any (\f -> hasBang f $ )(`hasBang` fc)
hasStrict filePath = do
    fileContents <- readFile filePath
    hasBang filePath fileContents

main :: IO ()
main = do
    filePath <- head <$> getArgs
    srcs <- findHs filePath
    putStrLn $ unlines $filter hasBang srcs -- how to separate IO here
--}
