-- | All exit codes that the program can return (excepting
--   'ExitSuccess').
--
module ExitCodes (
  exit_no_feed_hosts,
  exit_no_password,
  exit_no_username,
  exit_pidfile_exists )
where

-- | No feed hosts were given on the command line or in the config file.
--
exit_no_feed_hosts :: Int
exit_no_feed_hosts = 1

-- | No password was given on the command line or in the config file.
--
exit_no_password :: Int
exit_no_password = 2

-- | No username was given on the command line or in the config file.
--
exit_no_username :: Int
exit_no_username = 3

-- | When running as a daemon, the existence of a fixed PID file is
--   used to determine whether or not the daemon is already
--   running. If the PID file already exists, we shouldn't start.
--
exit_pidfile_exists :: Int
exit_pidfile_exists = 4
