module HasBang (hasBang) where

--import System.IO.Unsafe
import Language.Haskell.Exts
import Control.Applicative
import Data.Functor
import System.Environment
--import System.FilePath.Find
--import System.Environment 
--import System.Process
--import Data.Functor
--import Control.Applicative
--import Data.List

extns = ["CPP","DefaultSignatures","MagicHash","BangPatterns", 
         "TypeSynonymInstances","FlexibleInstances","InstanceSigs",
         "FlexibleContexts","ViewPatterns","TypeOperators","TypeFamilies",
         "TupleSections","TemplateHaskell","StandaloneDeriving",
         "ScopedTypeVariables","Safe","RecordWildCards","PackageImports",
         "NPlusKPatterns","NamedFieldPuns","MultiWayIf","MultiParamTypeClasses",
         "Malformed","LambdaCase","GADTs","FunctionalDependencies",
         "ExplicitForAll","ExistentialQuantification","DataKinds"]

hasBang :: String -> IO Bool
hasBang filePath = do 
            program <- readFile filePath
            return $ hasBangDecl $ getDecl (getModule extns filePath program)

getDecl (Module _ _ _ _ _ _ d) = d

getPat (Match _ _ p _ _ _) = p 

hasBangDecl :: [Decl] -> Bool
hasBangDecl [] = False
hasBangDecl (d:ds) = case d of
                             FunBind ms -> hasBangMatch ms || hasBangDecl ds
                             _ -> hasBangDecl ds              


hasBangMatch :: [Match] -> Bool
hasBangMatch [] = False
hasBangMatch (m:ms) = (hasBangPat $ getPat m) || hasBangMatch ms
                             
hasBangPat :: [Pat] -> Bool
hasBangPat [] = False
hasBangPat (p:ps) = case p of
                        PBangPat _ -> True
                        _          -> hasBangPat ps

getModule :: [String] -> String -> String -> Module
getModule extns filePath program = 
        fromParseResult $ parseFileContentsWithMode mode program
            where
                bangPatternsExt = map parseExtension extns
                mode = ParseMode filePath Haskell2010 bangPatternsExt False False Nothing

-- test: comment out module declaration to test

main = do 
    fp <- head <$> getArgs
    result <- hasBang fp
    putStrLn $ show $ result
