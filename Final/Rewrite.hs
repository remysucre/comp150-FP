module Rewrite (
    flipBang
    , flipRandomBang
    , placesToStrict
) where

import System.Random
import System.IO.Unsafe
import Language.Haskell.Exts
import Language.Haskell.Exts.Syntax

setDecl :: Module -> [Decl] -> Module
setDecl (Module src name prags warn exp imp _) ds = Module src name prags warn exp imp ds

getDecl :: Module -> [Decl]
getDecl (Module _ _ _ _ _ _ d) = d

countBangVar :: [Pat] -> Int
countBangVar [] = 0
countBangVar (p:ps) = case p of
                         PBangPat _ -> 1 + countBangVar ps
                         PVar _     -> 1 + countBangVar ps
                         otherwise  ->     countBangVar ps

countBangMatch :: [Match] -> Int
countBangMatch [] = 0
countBangMatch (m:ms) = (countBangVar $ getPat m) + countBangMatch ms

countBangDecl :: [Decl] -> Int
countBangDecl [] = 0
countBangDecl (d:ds) = case d of
                            FunBind ms -> countBangMatch ms + countBangDecl ds
                            otherwise  -> countBangDecl ds

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

setPat :: Match -> [Pat] -> Match
setPat (Match loc n _ ty rhs bind) p = Match loc n p ty rhs bind

getPat :: Match -> [Pat]
getPat (Match _ _ p _ _ _) = p

flipBangPat :: Int -> [Pat] -> (Int, [Pat])
flipBangPat x [] = (x, [])
flipBangPat 0 (p:ps) = case p of
                            PBangPat pat -> (-1, pat:ps)
                            PVar n       -> (-1, (PBangPat (PVar n)):ps)
                            otherwise    -> let (x, ps') = flipBangPat 0 ps
                                            in (x, p:ps')
flipBangPat x (p:ps) = (y, p:ps')
                       where
                           (y, ps') = case p of
                                           PBangPat pat -> flipBangPat (x-1) ps
                                           PVar name -> flipBangPat (x-1) ps
                                           otherwise -> flipBangPat x ps

flipBangMatch :: Int -> [Match] -> (Int, [Match])
flipBangMatch x [] = (x, [])
flipBangMatch (-1) ms = (-1, ms)
flipBangMatch x (m:ms) = (z, m':ms')
                         where
                             (y, ps) = flipBangPat x $ getPat m
                             m' = setPat m ps
                             (z, ms') = flipBangMatch y ms
                             


flipBangDecl :: Int -> [Decl] -> [Decl]
flipBangDecl (-1) ds = ds
flipBangDecl _ [] = error "Not enough Decl to Bang"
flipBangDecl x (d:ds) = case d of
                             FunBind ms -> let (y, ms') = flipBangMatch x ms
                                           in (FunBind $ ms'):(flipBangDecl y ds)
                             otherwise -> [d] ++ flipBangDecl x ds                          

flipBang :: String -> String -> Int -> String
flipBang filePath program num = program'
                       where
                           mod = getModule filePath program
                           decl' = flipBangDecl num $ getDecl mod
                           program' = prettyPrint $ setDecl mod decl'

flipRandomBang :: String -> String -> (Int, String)
flipRandomBang filePath program = (num + 1, flipBang filePath program num)
                                  where
                                      range = (0, placesToStrict filePath program)
                                      num = (unsafePerformIO $ getStdRandom (randomR range)) - 1

placesToStrict :: String -> String -> Int
placesToStrict filePath program = num
                                  where
                                      mod = getModule filePath program
                                      num = countBangDecl $ getDecl mod

getModule :: String -> String -> Module
getModule filePath program = fromParseResult $ parseFileContentsWithMode mode program
                             where
                                      bangPatternsExt = parseExtension "BangPatterns"
                                      mode = ParseMode filePath Haskell2010 [bangPatternsExt] True True Nothing

main :: IO ()
main = writeFile tempPath $ snd $ flipRandomBang filePath fileContents
       where
         --result = lexTokenStreamWithMode mode fileContents
         --result = unsafePerformIO $ parseFileWithMode mode filePath
         --mod = fromParseResult result
         --decl' = flipBangDecl 2 $ getDecl mod
         --mod' = setDecl mod decl'
         filePath = "B.hs"
         fileContents = unsafePerformIO $ readFile filePath
         tempPath = "temp.hs"
         --bangPatternsExt = parseExtension "BangPatterns"
         --mode = ParseMode filePath Haskell2010 [bangPatternsExt] True True Nothing
