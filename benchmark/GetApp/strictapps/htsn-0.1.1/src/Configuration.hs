-- | This module defines the 'Configuration' type, which is just a
--   wrapper around all of the configuration options we accept on the
--   command line.
--
module Configuration (
  Configuration(..),
  merge_optional )
where

import System.Console.CmdArgs.Default ( Default(..) )
import System.Log ( Priority( INFO ) )

import qualified OptionalConfiguration as OC (
  OptionalConfiguration(..),
  merge_maybes )
import FeedHosts (FeedHosts(..))

-- | The main configuration data type. This will be passed to most of
--   the important functions once it has been created.
data Configuration =
  Configuration {
    daemonize        :: Bool,
    feed_hosts       :: FeedHosts,
    log_file         :: Maybe FilePath,
    log_level        :: Priority,
    output_directory :: FilePath,
    password         :: String,
    pidfile          :: FilePath,
    run_as_group     :: Maybe String,
    run_as_user      :: Maybe String,
    syslog           :: Bool,
    username         :: String }
    deriving (Show)

-- | A Configuration with all of its fields set to their default
--   values.
instance Default Configuration where
  def = Configuration {
          daemonize        = def,
          feed_hosts       = def,
          log_file         = def,
          log_level        = INFO,
          output_directory = ".",
          password         = def,
          pidfile          = "/run/htsn/htsn.pid",
          run_as_group     = def,
          run_as_user      = def,
          syslog           = def,
          username         = def }


-- | Merge a 'Configuration' with an 'OptionalConfiguration'. This is
--   more or less the Monoid instance for 'OptionalConfiguration', but
--   since the two types are different, we have to repeat ourselves.
merge_optional :: Configuration
               -> OC.OptionalConfiguration
               -> Configuration
merge_optional cfg opt_cfg =
  Configuration
    (merge (daemonize cfg) (OC.daemonize opt_cfg))
    all_feed_hosts
    (OC.merge_maybes (log_file cfg) (OC.log_file opt_cfg))
    (merge (log_level cfg) (OC.log_level opt_cfg))
    (merge (output_directory cfg) (OC.output_directory opt_cfg))
    (merge (password cfg) (OC.password opt_cfg))
    (merge (pidfile cfg) (OC.pidfile opt_cfg))
    (OC.merge_maybes (run_as_group cfg) (OC.run_as_group opt_cfg))
    (OC.merge_maybes (run_as_user cfg) (OC.run_as_user opt_cfg))
    (merge (syslog cfg) (OC.syslog opt_cfg))
    (merge (username cfg) (OC.username opt_cfg))
  where
    -- | If the thing on the right is Just something, return that
    --   something, otherwise return the thing on the left.
    merge :: a -> Maybe a -> a
    merge x Nothing  = x
    merge _ (Just y) = y

    -- If there are any optional usernames, use only those.
    all_feed_hosts = if (null (get_feed_hosts (OC.feed_hosts opt_cfg)))
                     then (feed_hosts cfg)
                     else (OC.feed_hosts opt_cfg)

