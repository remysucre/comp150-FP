{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Pit (
  get,
  getValue,
  set,
  setValue,
  switch
  ) where

import Control.Applicative ((<$>))
import Control.Monad (unless, when)

import qualified Data.ByteString.Char8 as C
import Data.HashMap.Strict (HashMap())
#if !MIN_VERSION_base(4, 6, 0)
import qualified Data.List as L
#endif
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text (Text())
import qualified Data.Text as T
import qualified Data.Yaml as Y

import System.Directory
import System.Environment
import qualified System.FilePath as F
import System.IO
import System.IO.Temp
import System.Process

type Config = HashMap Text Y.Value

pitDirectory :: IO FilePath
pitDirectory = (F.</> ".pit") <$> getHomeDirectory

pitConfigFile :: IO FilePath
pitConfigFile = (F.</> "pit.yaml") <$> pitDirectory

pitProfileFile :: FilePath -> IO FilePath
pitProfileFile profile =
  (\dir -> dir F.</> profile F.<.> "yaml") <$> pitDirectory

writeDefaultConfig :: IO ()
writeDefaultConfig = switch "default"

loadProfile :: Text -> IO (Maybe Config)
loadProfile profile' = do
  let profile = T.unpack profile'
  file <- pitProfileFile profile
  exist <- doesFileExist file
  if exist then Y.decodeFile file else return Nothing

getProfile :: IO Text
getProfile = do
  file <- pitConfigFile
  conf <- fromJust <$> Y.decodeFile file
  return . fromJust $ H.lookup ("profile" :: Text) conf

-- If '~/.pit' directory or 'pit.yaml' file don't exist, make them.
initialize :: IO ()
initialize = do
  dir <- pitDirectory
  createDirectoryIfMissing False dir
  existsConf <- pitConfigFile >>= doesFileExist
  unless existsConf writeDefaultConfig

openEditorAndGetValue :: Maybe Y.Value -> IO (Maybe Y.Value)
openEditorAndGetValue def = do
#if MIN_VERSION_base(4, 6, 0)
  editor' <- lookupEnv "EDITOR"
#else
  env <- getEnvironment
  let editor' = L.lookup "EDITOR" env
#endif
  isTty <- hIsTerminalDevice stdout
  if isJust editor' && isTty
    then withSystemTempFile "new.yaml" $ \path h -> do
    hClose h
    when (isJust def) $ do
      let content = C.unpack $ Y.encode $ fromJust def
      writeFile path content
    _ <- callCommand (fromJust editor' ++ " " ++ path)
    Y.decodeFile path
    else return Nothing

-- | Tries to get the data by a key.
-- If the data associated with the key is not found,
-- open $EDITOR with the default value.
get :: Text -- ^ a key
       -> Y.Value -- ^ default value
       -> IO Y.Value
get key v = do
  v' <- getValue key
  case v' of
   Nothing -> do
     v'' <- openEditorAndGetValue $ Just v
     case v'' of
      Nothing -> error "Failed to set the value."
      Just v''' -> do
        setValue key v'''
        return v'''
   Just v'' -> return v''

-- | Gets the data by a key.
-- If current profile is set to 'dev', this function tries to
-- get the data from '~/.pit/dev.yaml'.
getValue :: (Y.FromJSON a)
       => Text -- ^ a key
       -> IO (Maybe a)
getValue name = do
  initialize
  prof <- getProfile
  conf <- loadProfile prof
  case conf of
   Nothing -> return Nothing
   Just c -> case H.lookup name c of
     Nothing -> return Nothing
     Just v -> return $ Y.parseMaybe Y.parseJSON v

-- | Sets new data.
-- Open $EDITOR with the current value.
set :: Text -- ^ a key
       -> IO ()
set key = do
  v <- getValue key :: IO (Maybe Y.Value)
  putStrLn $ show v
  v' <- openEditorAndGetValue v
  case v' of
   Nothing -> error "Failed to set the value."
   Just v'' -> do
     setValue key v''

-- | Sets new data.
setValue :: (Y.ToJSON a)
       => Text -- ^ a key
       -> a -- ^ new data
       -> IO ()
setValue name value = do
  initialize
  prof <- getProfile
  conf <- fromMaybe H.empty <$> loadProfile prof
  let newConf = H.insert name (Y.toJSON value) conf
  file <- pitProfileFile $ T.unpack prof
  Y.encodeFile file newConf

-- | Switches the profile.
-- The current profile is stored in '~/.pit/pit.yaml'.
-- This function rewrites it.
switch :: Text -- ^ new profile
          -> IO ()
switch newProf = do
  let newConf = Y.object ["profile" Y..= Y.String newProf]
  file <- pitConfigFile
  Y.encodeFile file newConf
