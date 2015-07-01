{-# OPTIONS_GHC -O2 -fglasgow-exts #-}

import Data.List
import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt
import Language.Haskell.TH.Syntax
import Language.Haskell.Meta.Utils
import Language.Haskell.Derive.Gadt
import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  (opts,_,_) <- return . getOpt Permute options =<< getArgs
  case opts of
    OptHelp:_ -> goHelp
    OptVers:_ -> goVersion
    _ | OptAll `elem` opts
              -> print . ppDoc =<< runQ . doIt allD =<< getContents
    []        -> goHelp
    _         -> print . ppDoc =<< runQ . accumTodo opts =<< getContents

allD :: [[GadtInfo] -> Q [Dec]]
allD = [eqD,ordD,showD,readD]

eqD :: [GadtInfo] -> Q [Dec]
eqD is = concat <$> mapM deriveEqGadtInfo is

ordD :: [GadtInfo] -> Q [Dec]
ordD is = concat <$> mapM deriveOrdGadtInfo is

showD :: [GadtInfo] -> Q [Dec]
showD is = concat <$> mapM deriveShowGadtInfo is

readD :: [GadtInfo] -> Q [Dec]
readD is = concat <$> mapM deriveReadGadtInfo is

accumTodo :: [Opt] -> String -> Q [Dec]
accumTodo opts s = do
  let go OptEq   = eqD
      go OptOrd  = ordD
      go OptShow = showD
      go OptRead = readD
      go _       = const (return [])
      fs = fmap go (nub opts)
  doIt (fmap go opts) s

doIt :: [[GadtInfo] -> Q [Dec]] -> String -> Q [Dec]
doIt fs s = case parseModuleGadts s of
              Left e -> fail e
              Right is -> concat <$> forM fs ($ is)


goHelp :: IO ()
goHelp = do
  let pgm = "derive-gadt" ++ let v = version
                              in if null v then [] else ("-"++v)
  putStrLn "Takes a module on stdin, outputs to stdout."
  putStr   (usageInfo pgm options)

version :: String
version = "0.1.0"

goVersion :: IO ()
goVersion = putStrLn version

data Opt
  = OptHelp
  | OptVers
  | OptAll
  | OptEq
  | OptOrd
  | OptRead
  | OptShow
  deriving(Eq,Ord,Show,Read)

options :: [OptDescr Opt]
options =
  [Option ['h'] ["help"] (NoArg OptHelp) "display help and usage information"
  ,Option ['v','V'] ["version"] (NoArg OptVers) "show version information"
  ,Option ['a'] ["all"] (NoArg OptAll) "derive everything"
  ,Option [] ["Eq"] (NoArg OptEq) "derive Eq"
  ,Option [] ["Ord"] (NoArg OptOrd) "derive Ord"
  ,Option [] ["Show"] (NoArg OptShow) "derive Show"
  ,Option [] ["Read"] (NoArg OptRead) "derive Read"]
