{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | An OptionalConfiguration is just like a 'Configuration', except
--   all of its fields are optional. The user can set options in two
--   places: the command-line, and a configuration file. Obviously if
--   a parameter is set in one place, it doesn't need to be set in the
--   other. Thus, the latter needs to be optional.
--

module OptionalConfiguration (
  OptionalConfiguration(..),
  from_rc,
  merge_maybes )
where

import qualified Data.Configurator as DC (
  Worth(Optional),
  load,
  lookup )
import qualified Data.Configurator.Types as DCT (
  Configured,
  Value( String ),
  convert )
import Data.Data ( Data )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( Monoid(..) )
import Data.Typeable ( Typeable )
import Paths_htsn ( getSysconfDir )
import System.Directory ( getHomeDirectory )
import System.FilePath ( (</>) )
import System.IO.Error ( catchIOError )
import System.Log ( Priority(..) )

import FeedHosts ( FeedHosts(..) )
import Network.Services.TSN.Report ( report_error )


-- Derive standalone instances of Data and Typeable for Priority. This
-- is necessary for OptionalConfiguration (which contains a Maybe
-- Priority) to derive Data and Typeable.
deriving instance Data Priority
deriving instance Typeable Priority

-- | The same as Configuration, except everything is optional. It's easy to
--   merge two of these by simply dropping the Nothings in favor of
--   the Justs. The 'feed_hosts' are left un-maybed so that cmdargs
--   can parse more than one of them.
--
data OptionalConfiguration =
  OptionalConfiguration {
    daemonize        :: Maybe Bool,
    feed_hosts       :: FeedHosts,
    log_file         :: Maybe FilePath,
    log_level        :: Maybe Priority,
    output_directory :: Maybe FilePath,
    password         :: Maybe String,
    pidfile          :: Maybe FilePath,
    run_as_group     :: Maybe String,
    run_as_user      :: Maybe String,
    syslog           :: Maybe Bool,
    username         :: Maybe String }
    deriving (Show, Data, Typeable)


-- | Combine two Maybes into one, essentially mashing them
--   together. We give precedence to the second argument when both are
--   Justs.
merge_maybes :: (Maybe a) -> (Maybe a) -> (Maybe a)
merge_maybes Nothing Nothing   = Nothing
merge_maybes (Just x) Nothing  = Just x
merge_maybes Nothing (Just x)  = Just x
merge_maybes (Just _) (Just y) = Just y


-- | The Monoid instance for these lets us "combine" two
--   OptionalConfigurations. The "combine" operation that we'd like to
--   perform is, essentially, to mash them together. So if we have two
--   OptionalConfigurations, each half full, we could combine them
--   into one big one.
--
--   This is used to merge command-line and config-file settings.
--
instance Monoid OptionalConfiguration where
  -- | An empty OptionalConfiguration.
  mempty = OptionalConfiguration
             Nothing
             (FeedHosts [])
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing


  -- | Combine @cfg1@ and @cfg2@, giving precedence to @cfg2@.
  cfg1 `mappend` cfg2 =
    OptionalConfiguration
      (merge_maybes (daemonize cfg1) (daemonize cfg2))
      all_feed_hosts
      (merge_maybes (log_file cfg1) (log_file cfg2))
      (merge_maybes (log_level cfg1) (log_level cfg2))
      (merge_maybes (output_directory cfg1) (output_directory cfg2))
      (merge_maybes (password cfg1) (password cfg2))
      (merge_maybes (pidfile cfg1) (pidfile cfg2))
      (merge_maybes (run_as_group cfg1) (run_as_group cfg2))
      (merge_maybes (run_as_user cfg1) (run_as_user cfg2))
      (merge_maybes (syslog cfg1) (syslog cfg2))
      (merge_maybes (username cfg1) (username cfg2))
    where
      -- Use only the latter feed_hosts if there are any.
      all_feed_hosts =
        feed_hosts $ if (null (get_feed_hosts (feed_hosts cfg2)))
                    then cfg1
                    else cfg2


instance DCT.Configured Priority where
  -- | This allows us to read a Priority level out of a Configurator
  --   config file. By default Configurator wouldn't know what to do,
  --   so we have to tell it that we expect one of the valid Priority
  --   constructors.
  convert (DCT.String "INFO") = Just INFO
  convert (DCT.String "WARNING") = Just WARNING
  convert (DCT.String "ERROR") = Just ERROR
  convert _ = Nothing


-- | Obtain an OptionalConfiguration from htsnrc in either the global
--   configuration directory or the user's home directory. The one in
--   $HOME is prefixed by a dot so that it is hidden.
--
--   We make an attempt at cross-platform compatibility; we will try
--   to find the correct directory even on Windows. But if the calls
--   to getHomeDirectory/getSysconfDir fail for whatever reason, we
--   fall back to using the Unix-specific /etc and $HOME.
--
from_rc :: IO OptionalConfiguration
from_rc = do
  etc  <- catchIOError getSysconfDir (\e -> do
                                        report_error (show e)
                                        return "/etc")
  home <- catchIOError getHomeDirectory (\e -> do
                                           report_error (show e)
                                           return "$(HOME)")
  let global_config_path = etc </> "htsnrc"
  let user_config_path = home </> ".htsnrc"
  cfg <- DC.load [ DC.Optional global_config_path,
                   DC.Optional user_config_path ]
  cfg_daemonize <- DC.lookup cfg "daemonize"
  cfg_feed_hosts <- DC.lookup cfg "feed_hosts"
  cfg_log_file <- DC.lookup cfg "log_file"
  cfg_log_level <- DC.lookup cfg "log_level"
  cfg_output_directory <- DC.lookup cfg "output_directory"
  cfg_password <- DC.lookup cfg "password"
  cfg_pidfile <- DC.lookup cfg "pidfile"
  cfg_run_as_group <- DC.lookup cfg "run_as_group"
  cfg_run_as_user <- DC.lookup cfg "run_as_user"
  cfg_syslog <- DC.lookup cfg "syslog"
  cfg_username <- DC.lookup cfg "username"

  return $ OptionalConfiguration
             cfg_daemonize
             (fromMaybe (FeedHosts []) cfg_feed_hosts)
             cfg_log_file
             cfg_log_level
             cfg_output_directory
             cfg_password
             cfg_pidfile
             cfg_run_as_group
             cfg_run_as_user
             cfg_syslog
             cfg_username
