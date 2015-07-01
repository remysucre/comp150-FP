
{- |
  Module      :  hstidy
  Copyright   :  (c) Matt Morrow 2008-2009
  License     :  BSD3
  Maintainer  :  <morrow@moonpatio.com>
  Stability   :  experimental
  Portability :  portable
-}

module Main where
import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Fixity
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Parser(ParseMode(..),ParseResult(..))
import qualified Language.Haskell.Exts.Parser as P
import Language.Haskell.Meta(parseResultToEither,moduleDecls)



main :: IO ()
main = do
  (opts,_,_) <- return . getOpt Permute options =<< getArgs
  case opts of
    OptMod:_  -> case getOut opts of
                  DeclsO -> go parseHsModule (unlines . fmap prettyPrint . moduleDecls)
                  DfltO  -> go parseHsModule prettyPrint
    OptDec:_  -> go parseHsDecl prettyPrint
    OptTyp:_  -> go parseHsType prettyPrint
    OptExp:_  -> go parseHsExp prettyPrint
    OptPat:_  -> go parseHsPat prettyPrint
    OptHelp:_ -> goHelp
    OptVers:_ -> goVersion
    _         -> case getOut opts of
                  DeclsO -> go parseHsModule (unlines . fmap prettyPrint . moduleDecls)
                  DfltO  -> go parseHsModule prettyPrint

go :: (String -> Either String a) -> (a -> String) -> IO ()
go p pp = either  (hPutStrLn stderr)
                  (hPutStrLn stdout . pp) . p =<< getContents

goHelp :: IO ()
goHelp = putStrLn (usageInfo "hstidy" options)

goVersion :: IO ()
goVersion = putStrLn "0.1"

data Opt
  = OptHelp
  | OptVers
  | OptMod
  | OptDec
  | OptTyp
  | OptExp
  | OptPat
  | OptOut Out
  deriving (Eq,Ord,Show,Read)

options :: [OptDescr Opt]
options =
  [Option ['h'] ["help"] (NoArg OptHelp) "display help and usage information"
  ,Option ['v','V'] ["version"] (NoArg OptVers) "show version information"
  ,Option ['m'] ["module"] (NoArg OptMod) "tidy a module from stdin"
  ,Option ['d'] ["decl"] (NoArg OptDec) "tidy a decl from stdin"
  ,Option ['t'] ["type"] (NoArg OptTyp) "tidy a type from stdin"
  ,Option ['e'] ["expression"] (NoArg OptExp) "tidy an expression from stdin"
  ,Option ['p'] ["pattern"] (NoArg OptPat) "tidy a pattern from stdin"
  ,Option [] ["out"] (ReqArg parseOut "WHAT_TO_OUTPUT") "describes what to output"]


parseOut :: String -> Opt
parseOut "decls" = OptOut DeclsO
parseOut  _      = OptOut DfltO

data What
  = AModule
  | ADecl
  | AType
  | AnExp
  | APat
  deriving(Eq,Ord,Read,Show)

data Out
  = DfltO
  | DeclsO
  deriving(Eq,Ord,Read,Show)

getOut :: [Opt] -> Out
getOut opts =
  case [out | OptOut out <- opts] of
    []  -> DfltO
    o:_ -> o

isGADT :: Decl -> Bool
isGADT (GDataDecl{}) = True
isGADT  _            = False

parseHsModule :: String -> Either String Module
parseHsModule = parse P.parseModuleWithMode

parseHsDecl :: String -> Either String Decl
parseHsDecl = parse P.parseDeclWithMode

parseHsType :: String -> Either String Type
parseHsType = parse P.parseTypeWithMode

parseHsExp :: String -> Either String Exp
parseHsExp = parse P.parseExpWithMode

parseHsPat :: String -> Either String Pat
parseHsPat = parse P.parsePatWithMode

parse :: (ParseMode -> String -> ParseResult a) -> String -> Either String a
parse p = parseResultToEither . p myParseMode

myParseMode :: ParseMode
myParseMode = ParseMode
  {parseFilename = []
  ,extensions = myExtensions
  ,ignoreLanguagePragmas = False
  ,fixities = baseFixities}

myExtensions :: [Extension]
myExtensions =
  [ OverlappingInstances
  , UndecidableInstances
  , IncoherentInstances
  , RecursiveDo
  , ParallelListComp
  , MultiParamTypeClasses
  , NoMonomorphismRestriction
  , FunctionalDependencies
  , RankNTypes
  , PolymorphicComponents
  , ExistentialQuantification
  , ScopedTypeVariables
  , ImplicitParams
  , FlexibleContexts
  , FlexibleInstances
  , EmptyDataDecls
  , CPP
  , KindSignatures
  , BangPatterns
  , TypeSynonymInstances
  , TemplateHaskell
  , ForeignFunctionInterface
  , NamedFieldPuns
  , PatternGuards
  , GeneralizedNewtypeDeriving
  , ExtensibleRecords
  , HereDocuments
  , MagicHash
  , TypeFamilies
  , StandaloneDeriving
  , UnicodeSyntax
  , PatternSignatures
  , UnliftedFFITypes
  , LiberalTypeSynonyms
  , TypeOperators
  , RecordWildCards
  , RecordPuns
  , DisambiguateRecordFields
  , OverloadedStrings
  , GADTs
  , RelaxedPolyRec
  , ExtendedDefaultRules
  , UnboxedTuples
  , DeriveDataTypeable
  , ConstrainedClassMethods
  , PackageImports
  , ImpredicativeTypes
  , NewQualifiedOperators
  , PostfixOperators
  , QuasiQuotes
  , TransformListComp
  , ViewPatterns ]



