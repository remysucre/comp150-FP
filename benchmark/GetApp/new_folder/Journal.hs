{- |
Module       : PuffyTools.Journal
Description  : A Journal-keeping thing
Copyright    : 2014, Peter Harpending
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : Linux

-}

module PuffyTools.Journal where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Monoid
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as B
import           Data.List.Utils
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           PuffyTools.Slug
import           System.Directory
import           System.IO

programName :: String
programName = "puffytools"

journalExt :: String
journalExt = ".journal"

-- |A Journal is really a wrapper around a list of entries
data Journal = Journal { journalSlug :: Slug
                       , journalTitle :: Text
                       , journalLastEdited :: UTCTime
                       , journalCreated :: UTCTime
                       , journalDescription :: Maybe Text
                       , journalEntries :: Vector Entry
                       }
  deriving (Show, Eq)

-- |Entries
data Entry = Entry { entrySummary :: Text
                   , entryCreated :: UTCTime
                   , entryLastEdited :: UTCTime
                   }
  deriving (Show, Eq)

instance FromJSON Journal where
  parseJSON (Object v) = Journal <$> v .: "slug"
                                 <*> v .: "title"
                                 <*> v .: "last-edited"
                                 <*> v .: "created"
                                 <*> v .: "description"
                                 <*> v .: "entries"
  parseJSON _ = fail "Must be an object"

instance ToJSON Journal where
  toJSON (Journal s t le cr des ent) = object
                                         [ "slug" .= s
                                         , "title" .= t
                                         , "last-edited" .= le
                                         , "created" .= cr
                                         , "description" .= des
                                         , "entries" .= ent
                                         ]

instance FromJSON Entry where
  parseJSON (Object v) = Entry <$> v .: "summary"
                               <*> v .: "created"
                               <*> v .: "last-edited"
  parseJSON _ = fail "Not an object"

instance ToJSON Entry where
  toJSON e = object [ "summary" .= entrySummary e
                    , "created" .= entryCreated e
                    , "last-edited" .= entryLastEdited e
                    ]

addEntry :: Journal -> Entry -> Journal
addEntry j e = j { journalEntries = newEntries }
  where newEntries = journalEntries j `V.snoc` e

mkEntry :: Text -> IO Entry
mkEntry entryText = Entry entryText <$> getCurrentTime <*> getCurrentTime

-- |Makes a journal, given a slug
mkJournal :: Slug -> IO Journal
mkJournal s = getCurrentTime >>= \t -> return $ Journal s mempty t t mempty mempty

-- |Makes a journal, given a slug
mkJournal' :: Text -> IO Journal
mkJournal' s = mkSlugIO s >>= mkJournal

-- |Figures out the file path for a journal
generateJournalPath :: Journal -> IO FilePath
generateJournalPath j = do
  dataDir <- getAppUserDataDirectory programName
  return $ mconcat [dataDir, "/",  (T.unpack . unSlug . journalSlug) j,  journalExt]

generateSlugPath :: Slug -> IO FilePath
generateSlugPath slg = do
  ddir <- getAppUserDataDirectory programName
  let fullPath = mconcat [ddir, "/", T.unpack $ unSlug slg, journalExt]
  return fullPath

-- |Writes a journal to a file path
writeJournal :: Journal -> IO ()
writeJournal j = do pth <- generateJournalPath j; B.writeFile pth $ encodePretty j

-- |Reads a journal from the default file path (~/.puffytools/journal-title.json)
readJournalName :: Text -> IO Journal
readJournalName name = do
  slg <- case mkSlugEither name of
           Left err -> fail err
           Right s  -> return s
  generateSlugPath slg >>= readJournalFromFile

-- |Reads a journal from the default file path (~/.puffytools/journal-title.json)
readJournalDef :: Slug -> IO Journal
readJournalDef slg = generateSlugPath slg >>= readJournalFromFile

-- |Reads a journal, given a file path
readJournalFromFile :: FilePath -> IO Journal
readJournalFromFile fp = openFile fp ReadMode >>= readJournalFromHandle

-- |Reads a journal from a handle, close handle
readJournalFromHandle :: Handle -> IO Journal
readJournalFromHandle h = do
  handleBytes <- Bs.hGetContents h
  case eitherDecodeStrict' handleBytes of
    Left err -> fail err
    Right j  -> return j
  
{-# DEPRECATED listJournals "use listJournalFiles instead"#-}
listJournals :: IO [FilePath]
listJournals = listJournalFiles

-- |List all of the journal file paths
listJournalFiles :: IO [FilePath]
listJournalFiles = do
  ddr <- ddir
  fps <- filter (endswith ".journal") <$> allDataFiles
  return $ map (\fp -> mconcat [ddr, "/", fp]) fps

  where
    ddir = getAppUserDataDirectory programName
    allDataFiles = do
      d <- ddir
      d `seq` createDirectoryIfMissing True d
      getDirectoryContents d

-- |List all of the Journal slugs
listJournalSlugs :: IO [Text]
listJournalSlugs = do
  fps <- listJournalFiles
  journals <- mapM readJournalFromFile fps
  let slugs = map (unSlug . journalSlug) journals
  pure slugs

-- |Perform some action if a given journal exists
ifJournal :: Text -> IO () -> IO ()
ifJournal slg doStuff = do
  jss <- listJournalSlugs
  if slg `elem` jss
    then doStuff
    else return ()

-- |Perform some action if a given journal does not exist
unlessJournal :: Text -> IO () -> IO ()
unlessJournal slg doStuff = do
  jss <- listJournalSlugs
  if not (slg `elem` jss)
    then doStuff
    else return ()
