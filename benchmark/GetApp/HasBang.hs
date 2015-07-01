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
--import GetExtns

main = do 
    fp <- head <$> getArgs
    result <- hasBang fp
    putStrLn $ show $ result

extns = ["CPP", "DefaultSignatures", "MagicHash", "BangPatterns", "TypeSynonymInstances", "FlexibleInstances", "InstanceSigs", "FlexibleContexts", "ViewPatterns","TypeOperators","TypeFamilies","TupleSections","TemplateHaskell","StandaloneDeriving","ScopedTypeVariables","Safe","RecordWildCards","PackageImports","NPlusKPatterns","NamedFieldPuns","MultiWayIf","MultiParamTypeClasses","Malformed","LambdaCase","GADTs","FunctionalDependencies","ExplicitForAll","ExistentialQuantification","DataKinds"]

hasBang :: String -> IO Bool
hasBang filePath = do 
            program <- readFile filePath
            return $ hasBangDecl $ getDecl (getModule extns filePath program)


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

--getModule :: String -> String -> Module
--getModule extns filePath program = fromParseResult $ parseFileContentsWithMode mode program
getModule extns filePath program = fromParseResult $ parseFileContentsWithMode mode [if "#" `isPrefixOf` x then "" else x | x <- program]
                             where
                                      bangPatternsExt = map parseExtension extns
                                      mode = ParseMode filePath Haskell2010 bangPatternsExt False False Nothing
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
