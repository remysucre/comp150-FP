import Language.Haskell.Exts
import System.IO.Unsafe

showDecl :: Module -> [Decl]
showDecl (Module _ _ _ _ _ _ d) = d


showList' :: Show a => Int -> [a] -> IO ()
showList' _ [] = return ()
showList' i (x:xs) = putStrLn ((show i) ++ ": " ++ (show x)) >> showList' (i+1) xs

showMatches :: Int -> [Match] -> IO ()
showMatches _ [] = return ()
showMatches i (m:ms) = showList' (10 * i) (pats m)
                     where
                        pats (Match _ _ p _ _ _) = p

showFunBinds :: Int -> [Decl] -> IO ()
showFunBinds _ [] = return ()
showFunBinds i (x:xs) = case x of
                             FunBind ls -> showMatches (10 * i) ls >> showFunBinds (i+1) xs
                             otherwise -> showFunBinds i xs

main = putStr $ prettyPrint $ fromParseResult $ result
       where
         --result = lexTokenStreamWithMode mode fileContents
         result = unsafePerformIO $ parseFileWithMode mode filePath
         filePath = "B.hs"
         fileContents = unsafePerformIO $ readFile filePath
         bangPatternsExt = parseExtension "BangPatterns"
         mode = ParseMode filePath Haskell2010 [bangPatternsExt] True True Nothing
{-
# LANGUAGE CPP #
import GHC
import Outputable
 
import GHC.Paths ( libdir )
--GHC.Paths is available via cabal install ghc-paths
 
import DynFlags
targetFile = "B.hs"
 
main = do
   res <- example
#if __GLASGOW_HASKELL__ > 704
   putStrLn $ showSDoc tracingDynFlags ( ppr res )
#else
   putStrLn $ showSDoc ( ppr res )
#endif
 
example = 
#if __GLASGOW_HASKELL__ > 704
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
#else
    defaultErrorHandler defaultLogAction $ do
#endif
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = foldl xopt_set dflags
                            [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
        setSessionDynFlags dflags'
        target <- guessTarget targetFile Nothing
        setTargets [target]
        load LoadAllTargets
        modSum <- getModSummary $ mkModuleName "B"
        p <- parseModule modSum
        t <- typecheckModule p
        d <- desugarModule t
        l <- loadModule d
        n <- getNamesInScope
        c <- return $ coreModule d
 
        g <- getModuleGraph
        mapM showModule g     
        return $ (parsedSource d,"/n-----/n",  typecheckedSource d)
-}

