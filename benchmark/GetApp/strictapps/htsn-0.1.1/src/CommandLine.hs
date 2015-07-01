-- | Parse the command-line options, and display help text if
--   necessary.
module CommandLine (
  get_args )
where

import System.Console.CmdArgs (
  (&=),
  args,
  cmdArgs,
  def,
  details,
  help,
  program,
  summary,
  typ,
  typFile,
  typDir )

-- This let's us get the version from Cabal.
import Paths_htsn (version)
import Data.Version (showVersion)

import FeedHosts ( FeedHosts(..) )
import OptionalConfiguration ( OptionalConfiguration(..) )

-- | The description of the program, displayed as part of the help.
description :: String
description = "Parse XML files from The Sports Network feed."

-- | The name of this program.
program_name :: String
program_name = "htsn"

-- | A summary string output as part of the help.
my_summary :: String
my_summary = program_name ++ "-" ++ (showVersion version)


-- | A description of the "daemonize" option.
daemonize_help :: String
daemonize_help =
  "Run as a daemon, in the background."

-- | A description of the "log_file" option.
log_file_help :: String
log_file_help =
  "Log to the given file."

-- | A description of the "log_level" option.
log_level_help :: String
log_level_help =
  "How verbose should the logs be? One of INFO, WARNING, ERROR."

-- | A description of the "output_directory" option.
output_directory_help :: String
output_directory_help =
  "Directory in which to output the XML files; must be writable."

-- | A description of the "password" option.
password_help :: String
password_help =
  "Password to use when connecting to the feed."

-- | A description of the "pidfile" option.
pidfile_help :: String
pidfile_help =
  "Location to create PID file (daemon only)."

-- | A description of the "run_as_group" option.
run_as_group_help :: String
run_as_group_help =
  "System group to run as (daemon only)."

-- | A description of the "run_as_user" option.
run_as_user_help :: String
run_as_user_help =
  "System user to run under (daemon only)."

-- | A description of the "syslog" option.
syslog_help :: String
syslog_help =
  "Enable logging to syslog."

-- | A description of the "username" option.
username_help :: String
username_help =
  "Username to use when connecting to the feed."

-- | A data structure representing the possible command-line
--   options. The CmdArgs library is doing heavy magic beneath the
--   hood here.
arg_spec :: OptionalConfiguration
arg_spec =
  OptionalConfiguration {
    daemonize        = def &= typ "BOOL"      &= help daemonize_help,

    -- Use an empty list for feed_hosts since cmdargs will appends to
    -- the default when the user supplies feed hosts. If he specifies
    -- any, those are all we should use.
    feed_hosts       = FeedHosts [] &= typ "HOSTNAMES" &= args,

    log_file         = def &= typFile         &= help log_file_help,
    log_level        = def &= typ "LEVEL"     &= help log_level_help,
    output_directory = def &= typDir          &= help output_directory_help,
    password         = def &= typ "PASSWORD"  &= help password_help,
    pidfile          = def &= typFile         &= help pidfile_help,
    run_as_group     = def &= typ "GROUP"     &= help run_as_group_help,
    run_as_user      = def &= typ "USER"      &= help run_as_user_help,
    syslog           = def &= typ "BOOL"      &= help syslog_help,
    username         = def &= typ "USERNAME"  &= help username_help }
  &= program program_name
  &= summary my_summary
  &= details [description]


-- | A convenience function; our only export. Meant to be used in
--   'main' to retrieve the command-line arguments.
get_args :: IO OptionalConfiguration
get_args = cmdArgs arg_spec
