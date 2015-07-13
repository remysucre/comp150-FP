--module HasStrict.HasBang (hasBang) where
module HasBang (hasBang) where

--import System.IO.Unsafe
import Language.Haskell.Exts
import Language.Preprocessor.Cpphs
import Control.Applicative
import Data.Functor
import System.Environment
--import HasStrict.WashHash
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

opts = CpphsOptions { infiles = [], outfiles = []
                    , defines = map asTrue oplist, includes = []
                    , preInclude = []
                    , boolopts = defaultBoolOptions }
asTrue x = (x, "True")

oplist = ["MIN_VERSION_Cabal(a,b,c)"
        , "MIN_VERSION_OpenGL(a,b,c)"
        , "MIN_VERSION_accelerate(a,b,c)"
        , "MIN_VERSION_bytestring(a,b,c)"
        , "MIN_VERSION_cprng_aes(a,b,c)"
        , "MIN_VERSION_directory(a,b,c)"
        , "MIN_VERSION_exceptions(a,b,c)"
        , "MIN_VERSION_ghc(a,b,c)"
        , "MIN_VERSION_haskell_src_exts(a,b,c)"
        , "MIN_VERSION_http_conduit(a,b,c)"
        , "MIN_VERSION_mtl(a,b,c)"
        , "MIN_VERSION_optparse_applicative(a,b,c)"
        , "MIN_VERSION_pandoc(a,b,c)"
        , "MIN_VERSION_process(a,b,c)"
        , "MIN_VERSION_stm(a,b,c)"
        , "MIN_VERSION_time(a,b,c)"
        , "MIN_VERSION_tagsoup(a,b,c)"
        , "MIN_VERSION_cipher_aes(a,b,c)"
        , "MIN_VERSION_attoparsec(a,b,c)"
        , "MIN_VERSION_monad_control(a,b,c)"
        , "MIN_VERSION_blaze_html(a,b,c)"
        , "MIN_VERSION_text(a,b,c)"
        , "MIN_VERSION_shelly(a,b,c)"
        , "MIN_VERSION_feed(a,b,c)"
        , "MIN_VERSION_haddock(a,b,c)"
        , "MIN_VERSION_parsec(a,b,c)"
        , "MIN_VERSION_QuickCheck(a,b,c)"
        , "MIN_VERSION_deepseq(a,b,c)"
        , "MIN_VERSION_xmonad_contrib(a,b,c)"
        , "MIN_VERSION_hashable(a,b,c)"
        , "MIN_VERSION_transformers(a,b,c)"
        , "MIN_VERSION_gtk(a,b,c)"
        , "MIN_VERSION_utf8_string(a,b,c)"
        , "MIN_VERSION_template_haskell(2,10,0)"
        , "MIN_VERSION_base(a,b,c)"]

hasBang :: String -> IO Bool
hasBang filePath = do 
--            program <- washHash <$> readFile filePath
--            program <- readFile filePath
            fc <- readFile filePath
            program <- runCpphs opts filePath fc
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
