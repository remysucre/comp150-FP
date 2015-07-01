-- | Non-portable code for daemonizing on unix.
--
module Unix
where

import Control.Concurrent ( ThreadId, myThreadId )
import Control.Exception ( throwTo )
import Control.Monad ( unless )
import System.Directory ( createDirectory, doesDirectoryExist )
import System.Exit ( ExitCode( ExitSuccess ) )
import System.FilePath ( dropFileName, dropTrailingPathSeparator )
import System.IO.Error ( catchIOError )
import System.Posix (
  GroupEntry ( groupID ),
  GroupID,
  Handler ( Catch ),
  UserEntry ( userID ),
  UserID,
  exitImmediately,
  getGroupEntryForName,
  getProcessID,
  getRealGroupID,
  getRealUserID,
  getUserEntryForName,
  installHandler,
  removeLink,
  setFileCreationMask,
  setGroupID,
  setOwnerAndGroup,
  setUserID,
  sigTERM )
import System.Posix.Daemonize ( daemonize )

import Configuration (
  Configuration( pidfile,
                 run_as_group,
                 run_as_user ))
import Network.Services.TSN.Report ( report_error, report_info )

-- | Retrieve the uid associated with the given system user name. We
--   take a Maybe String as an argument so the user name can be passed
--   in directly from the config.
--
get_user_id :: Maybe String -> IO UserID
get_user_id Nothing  = getRealUserID
get_user_id (Just s) = fmap userID (getUserEntryForName s)


-- | Retrieve the gid associated with the given system group name. We
--   take a Maybe String as an argument so the group name can be
--   passed in directly from the config.
--
get_group_id :: Maybe String -> IO GroupID
get_group_id Nothing  = getRealGroupID
get_group_id (Just s) = fmap groupID (getGroupEntryForName s)


-- | This function will be called in response to a SIGTERM; i.e. when
--   someone tries to kill our process. We simply delete the PID file
--   and signal our parent thread to quit (successfully).
--
--   If that doesn't work, report the error and quit rudely.
--
graceful_shutdown :: Configuration -> ThreadId -> IO ()
graceful_shutdown cfg main_thread_id = do
  report_info "SIGTERM received, removing PID file and shutting down."
  catchIOError try_nicely (\e -> do
                             report_error (show e)
                             exitImmediately ExitSuccess )
  where
    try_nicely = do
      removeLink (pidfile cfg)
      throwTo main_thread_id ExitSuccess


-- | Create the directory in which we intend to store the PID
--   file. This will *not* create any parent directories. The PID
--   directory will have its owner/group changed to the user/group
--   under which we'll be running. No permissions will be set; the
--   system's umask must allow owner-write.
--
--   This is intended to create one level beneath either /var/run or
--   /run which often do not survive a reboot.
--
--   If the directory already exists, it is left alone; that is, we
--   don't change its owner/group.
--
create_pid_directory :: FilePath -- ^ The directory to contain the PID file.
                     -> UserID   -- ^ Owner of the new directory if created.
                     -> GroupID  -- ^ Group of the new directory if created.
                     -> IO ()
create_pid_directory pid_directory uid gid = do
  it_exists <- doesDirectoryExist pid_directory
  unless it_exists $ do
    report_info $ "Creating PID directory " ++ pid_directory ++ "."
    createDirectory pid_directory
    report_info $ "Changing owner/group of " ++ pid_directory ++
                    " to " ++ (show uid) ++ "/" ++ (show gid) ++ "."
    setOwnerAndGroup pid_directory uid gid

-- | Write a PID file, install a SIGTERM handler, drop privileges, and
--   finally do the daemonization dance.
--
full_daemonize :: Configuration -> IO () -> IO ()
full_daemonize cfg program = do
  uid <- get_user_id (run_as_user cfg)
  gid <- get_group_id (run_as_group cfg)

  -- This will have to be done as root and the result chowned to our
  -- user/group, so it must happen before daemonizing.
  let pid_directory = dropTrailingPathSeparator $ dropFileName $ pidfile cfg
  create_pid_directory pid_directory uid gid

  -- The call to 'daemonize' will set the umask to zero, but we want
  -- to retain it. So, we set the umask to zero before 'daemonize'
  -- can, so that we can record the previous umask value (returned by
  -- setFileCreationMask).
  orig_umask <- setFileCreationMask 0

  -- This is the 'daemonize' from System.Posix.Daemonize.
  daemonize (program' orig_umask uid gid)
  where
    -- We need to do all this stuff *after* we daemonize.
    program' orig_umask uid gid = do
      -- First we install a signal handler for sigTERM. We need to
      -- pass the thread ID to the signal handler so it knows which
      -- process to "exit."
      tid <- myThreadId
      _ <- installHandler sigTERM (Catch (graceful_shutdown cfg tid)) Nothing

      -- Next we drop privileges. Group ID has to go first, otherwise
      -- you ain't root to change groups.
      setGroupID gid
      setUserID uid

      -- Now we create the PID file.
      pid <- getProcessID

      -- The PID file needs to be read-only for anyone but its
      -- owner. Hopefully the umask accomplishes this!
      _ <- setFileCreationMask orig_umask

      -- When we later attempt to delete the PID file, it requires
      -- write permission to the parent directory and not to the PID
      -- file itself. Therefore, if that's going to work, this has to
      -- work, even as a limited user.
      writeFile (pidfile cfg) (show pid)

      -- Finally run the program we were asked to.
      program
