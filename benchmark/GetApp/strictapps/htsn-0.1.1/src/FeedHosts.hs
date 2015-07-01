{-# LANGUAGE DeriveDataTypeable #-}

-- | A newtype around a list of Strings which represent the feed
--   hosts. This is all to avoid an orphan instance of Configured for
--   [String] if we had defined one in e.g. 'OptionalConfiguration'.
--
--   This was placed under the \"TSN\" namespace because its Default
--   instance is specific to TSN, even though otherwise it's just a
--   list of strings.
--
module FeedHosts
where

-- DC is needed only for the DCT.Configured instance of String.
import qualified Data.Configurator as DC()
import qualified Data.Configurator.Types as DCT (
  Configured,
  Value( List ),
  convert )
import Data.Data (Data)
import System.Console.CmdArgs.Default (Default(..))
import Data.Typeable (Typeable)


-- | A (wrapper around a) list of hostnames that supply the XML feed.
--
newtype FeedHosts =
  FeedHosts { get_feed_hosts :: [String] }
    deriving (Data, Show, Typeable)


-- | The default list of feed hosts. These were found by checking
--   PTR records in the neighborhood of the IP address in use. There
--   is a feed4.sportsnetwork.com, but it was not operational when
--   this was written.
instance Default FeedHosts where
  def = FeedHosts ["feed1.sportsnetwork.com",
                   "feed2.sportsnetwork.com",
                   "feed3.sportsnetwork.com"]


instance DCT.Configured FeedHosts where
  -- | This allows us to read a FeedHosts object out of a Configurator
  --   config file. By default Configurator wouldn't know what to do,
  --   so we have to tell it that we expect a list, and if that list
  --   has strings in it, we can apply the FeedHosts constructor to
  --   it.
  convert (DCT.List xs) =
    -- mapM gives us a Maybe [String] here.
    fmap FeedHosts (mapM convert_string xs)
    where
      convert_string :: DCT.Value -> Maybe String
      convert_string = DCT.convert

  -- If we read anything other than a list of values out of the file,
  -- fail.
  convert _ = Nothing
