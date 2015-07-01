{-# LANGUAGE CPP #-}

-- Copyright (c) 2009 Deniz Dogan

module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad.Reader
import Data.List
import Data.Maybe
import HunpParser
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Process
import Text.ParserCombinators.Parsec
import Text.Regex.PCRE.Light
import Text.Regex.PCRE.Light.Base
import qualified Data.ByteString.Char8 as B

#include "VERSION"

data HunpConf = HC { flags :: [Flag]
                   , targets :: [String]
                   , rules :: [Rule Regex]
                   }

type Hunp a = ReaderT HunpConf IO a

type Rule a = ([a], String)

data Flag = Quiet | STFU | Default | Version
            deriving Eq

-- | The different options available.
options :: [OptDescr Flag]
options = [ Option "q" ["quiet"]   (NoArg Quiet)   "suppress stdout"
          , Option "Q" ["stfu"]    (NoArg STFU)    "suppress stdout and stderr"
          , Option "d" ["default"] (NoArg Default) "prints the default config"
          , Option "v" ["version"] (NoArg Version) "prints the version of hunp"
          ]

-- | A string of usage information.
usage :: String
usage = let ls = ["Usage: hunp [options] [files/directories]", ""
                 , "Options:"]
        in unlines ls

-- | Reads the configuration file.  Tries to use the user's custom
--   configuration file.  If that fails, uses the default one.
readConf :: IO [Rule Regex]
readConf = do
  file <- (</> "hunp.conf") <$> getAppUserDataDirectory "hunp"
  exists <- doesFileExist file
  if not exists
    then return defaultConf
    else do p <- readable <$> getPermissions file
            if not p
              then do putErrLn "Cannot read configuration file. Using default configuration."
                      return defaultConf
              else do res <- parseFromFile parser file
                      case res of
                        Left e  -> do putErrLn $ "Configuration file parse failed: " ++ show e
                                      return defaultConf
                        Right x -> return $ map compileRegexen x

-- | Prints the given configuration to stdout.
printConfig :: [Rule Regex] -> IO ()
printConfig []           = return ()
printConfig ((r,cmd):rs) = do
  let parts = map (\(Regex _ x) -> B.unpack x) r
  putStrLn $ (concat $ intersperse "," parts) ++ "\t" ++ cmd
  printConfig rs

main :: IO ()
main = do
  args <- getArgs
  let (fl, tgts, errs) = getOpt Permute options args
  if Default `elem` fl
    then printConfig defaultConf
    else if Version `elem` fl
           then putStrLn hunpVersion
           else if null tgts || not (null errs)
                   then putStr $ usageInfo usage options
                   else do conf <- readConf
                           let c = HC fl tgts conf
                           runReaderT (mapM_ hunp tgts) c

-- | Given a FilePath, determines whether it is pointing to a file or
--   a directory and takes the appropriate action.
hunp :: FilePath -> Hunp ()
hunp fp = do
  isFile <- io $ doesFileExist fp
  isDir <- io $ doesDirectoryExist fp
  when isFile (hunpFile fp)
  when isDir (hunpDir fp)
  unless (isFile || isDir) (talkE $ "Could not find `" ++ fp ++ "'. Skipping.")

-- | Given a filename, tries to find a matching regular expression for
--   it.  If one is found, unpacks it using the corresponding rule.
hunpFile :: FilePath -> Hunp ()
hunpFile fp = do
  conf <- asks rules
  case lookupWith (any (\x -> match' x (B.pack fp) [])) conf of
    Nothing -> talkE $ "Unknown file: `" ++ takeFileName fp ++ "'. Skipping."
    Just cmd -> do
      talk $ "Unpacking `" ++ fp ++ "'..."
      unpack (map (replaceIt fp) $ words cmd) >>=
       \x -> case x of
               ExitSuccess -> talk $ "Successfully unpacked `" ++ takeFileName fp ++ "'."
               _ -> talkE $ "Something went wrong while unpacking `" ++ takeFileName fp ++ "'!"

-- | Hackish way of performing printf substitution.  This was needed
--   for some silly reason that I can't be bothered remembering.
replaceIt :: String -> String -> String
replaceIt fp "%s" = fp
replaceIt _  x    = x

-- | Given a directory path, looks for the first file in the directory
--   matching any of the file regexp rules.  If it finds any file
--   matching any rule, it stops and unpacks the first found file.  It
--   will never unpack several files in a directory.
hunpDir :: FilePath -> Hunp ()
hunpDir fp = do
  cont <- io $ getDirectoryContents fp
  conf <- asks rules
  case find (\f -> isJust $ lookupWith (any (\x -> match' x (B.pack f) [])) conf) cont of
    Just x -> hunpFile (fp </> x)
    Nothing -> talkE $ "Found nothing to unpack in " ++ fp

-- | Exactly like 'Text.Regex.PCRE.Light', but returns @True@ if there
--   was a match, otherwise @False@.
match' :: Regex -> B.ByteString -> [PCREExecOption] -> Bool
match' a b = isJust . match a b

-- | Generalization of 'lookup'.
lookupWith :: (a -> Bool) -> [(a, b)] -> Maybe b
lookupWith _ []          = Nothing
lookupWith p ((x, y):xs) | p x = Just y
                         | otherwise = lookupWith p xs

unpack :: [String] -> Hunp ExitCode
unpack (cmd:args) = io $ rawSystem cmd args
unpack _          = error "Error in configuration file."

-- | Prints an error to @stderr@.
putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

defaultConf :: [Rule Regex]
defaultConf =
  let ls = [ (["\\.rar$", "\\.r00$"],      "unrar x %s")
           , (["\\.tar\\.gz$", "\\.tgz$"], "tar -zxvf %s")
           , (["\\.tar\\.bz2$"],           "tar -jxvf %s")
           , (["\\.bz2$"],                 "bunzip2 %s")
           , (["\\.zip$"],                 "unzip %s")
           , (["\\.arj$"],                 "unarj x %s")
           , (["\\.7z$"],                  "7z x %s")
           , (["\\.ace$"],                 "unace x %s")
           ]
  in map compileRegexen ls

compileRegexen :: Rule String -> Rule Regex
compileRegexen = first $ map (\x -> compile (B.pack x) [])

talk :: String -> Hunp ()
talk s = do
  fl <- asks flags
  unless (STFU `elem` fl || Quiet `elem` fl)
    (io $ putStrLn s)

talkE :: String -> Hunp ()
talkE s = do
  fl <- asks flags
  unless (STFU `elem` fl)
    (io $ hPutStrLn stderr s)

io :: MonadIO m => IO a -> m a
io = liftIO
