{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Main
where

-- System imports.
import Control.Applicative ( (<$>) )
import Control.Concurrent ( threadDelay )
import Control.Exception ( bracket, throw )
import Control.Monad ( when )
import Data.List ( isPrefixOf )
import Data.Maybe ( isNothing )
import Data.Monoid ( (<>) )
import Network (
  connectTo,
  PortID (PortNumber) )
import Network.Services.TSN.Logging ( init_logging )
import Network.Services.TSN.Report (
  report_debug,
  report_info,
  report_warning,
  report_error )
import Network.Services.TSN.Terminal ( display_sent )
import System.Console.CmdArgs ( def )
import System.Directory ( doesFileExist )
import System.Exit ( ExitCode(..), exitWith )
import System.FilePath ( (</>) )
import System.IO (
  BufferMode (NoBuffering),
  Handle,
  hClose,
  hGetChar,
  hGetLine,
  hPutStr,
  hSetBuffering,
  stderr,
  stdout )
import System.IO.Error ( catchIOError )
import System.Timeout ( timeout )

-- Local imports.
import CommandLine ( get_args )
import Configuration ( Configuration(..), merge_optional )
import ExitCodes (
  exit_no_feed_hosts,
  exit_no_password,
  exit_no_username,
  exit_pidfile_exists )
import FeedHosts ( FeedHosts(..) )
import qualified OptionalConfiguration as OC (
  OptionalConfiguration(..),
  from_rc )
import Xml ( parse_xmlfid )
import Unix ( full_daemonize )


-- | Receive a single line of text from a 'Handle', and record it for
--   debugging purposes.
--
recv_line :: Handle -> IO String
recv_line h = do
  line <- hGetLine h
  report_debug (line ++ "\n")
  return line


-- | Takes a 'Configuration', and an XML document (as a 'String'). The
--   XML document is written to the output directory, as specified by
--   the 'Configuration'.
--
--   This can fail, but we don't purposefully throw any exceptions. If
--   something goes wrong, we would rather log it and keep going.
--
--   And in fact the only \"error\" that can occur is from
--   'parse_xmlfid' if TSN sends us a non-integer XML File ID. But
--   this is expected from time to time, and is merely unsupported. So
--   we report any failures as info instead of as errors.
--
save_document :: Configuration
              -> String -- ^ String representation of an XML document
              -> IO ()
save_document cfg doc =
  case either_path of
    Left err -> report_info err -- Can only be non-integer XML File ID
    Right path -> do
      already_exists <- doesFileExist path
      when already_exists $ do
        let msg = "File " ++ path ++ " already exists, overwriting."
        report_warning msg
      writeFile path doc
      report_info $ "Wrote file: " ++ path ++ "."
  where
    -- All the <$> are because we're working inside an Either.
    xmlfid = show <$> (parse_xmlfid doc)
    filename = (++ ".xml") <$> xmlfid
    either_path = ((output_directory cfg) </>) <$> filename


-- | Loop forever, writing the @buffer@ to file whenever a
--   \</message\> tag is seen. This is the low-level \"loop forever\"
--   function that we stay in as long as we are connected to one feed.
--
--   The documentation at
--   <http://www.sportsnetworkdata.com/feeds/xml-levels.asp> states
--   that \<message\> will always be the root element of the XML
--   documents, and \</message\> will be the final line transmitted
--   for a given document. We therefore rely on this to simplify
--   processing.
--
--   The bang pattern at least on @buffer@ is necessary for
--   performance reasons.
--
--   We specify a timeout of fifteen minutes on the 'recv_line'
--   function, after which we will return to our caller. This should
--   cause the connection to be dropped, and a new one initiated. The
--   timeout is in response to observed behavior where the feed
--   eventually stops transmitting data entirely without closing the
--   connection.
--
loop :: Configuration
     -> Handle -- ^ Handle to the feed (network connection)
     -> [String] -- ^ Current XML document buffer, line-by-line, in reverse
     -> IO ()
loop !cfg !h !buffer = do
  line' <- timeout fifteen_minutes $ recv_line h
  case line' of
    -- If we haven't received anything in fifteen minutes, return back
    -- to the calling function. This should only happen in the case of
    -- an error, and our caller should be prepared to handle it.
    Nothing -> report_warning "No data received for 15 minutes."

    Just line -> do
      -- If the recv didn't timeout, proceed normally.
      let new_buffer = line : buffer

      -- Use isPrefixOf to avoid line-ending issues. Hopefully they won't
      -- send invalid junk (on the same line) after closing the root
      -- element.
      if "</message>" `isPrefixOf` line
      then do
        -- The buffer is in reverse (newest first) order, though, so we
        -- have to reverse it first. We then concatenate all of its lines
        -- into one big string.
        let document = concat $ reverse new_buffer
        save_document cfg document
        loop cfg h [] -- Empty the buffer before looping again.
      else
        -- Append line to the head of the buffer and loop.
        loop cfg h new_buffer
  where
    fifteen_minutes :: Int
    fifteen_minutes = 15 * 60 * 1000000


-- | Once we're connected to a feed, we need to log in. There's no
--   protocol for this (the docs don't mention one), but we have
--   (apparently) successfully guessed it.
--
--   The first thing TSN sends once we've connected is the string
--   \"Username: \", containing 10 ASCII characters. We then send a
--   username, followed by a newline. If TSN likes the username, the
--   second they'll send is the string \"Password: \", also containing
--   10 ASCII characters, to which we reply in kind.
--
--   Assuming the above will always hold, it is implemented as follows:
--
--     1. Receive 10 chars
--
--     2. Send username if we got the username prompt
--
--     3. Receive 10 chars
--
--     4. Send password if we got the password prompt
--
--   If TSN likes the password as well, they send the string \"The
--   Sports Network\" before finally beginning to stream the feed.
--
log_in :: Configuration -> Handle -> IO ()
log_in cfg h = do
  prompt1 <- recv_prompt h

  if prompt1 /= username_prompt then
    report_error "Didn't receive username prompt."
  else do
    send_cred h (username cfg)
    prompt2 <- recv_prompt h

    if prompt2 /= password_prompt then
      report_error "Didn't receive password prompt."
    else do
      send_cred h (password cfg)
      _ <- recv_line h -- "The Sports Network"
      report_info $ "Logged in as " ++ (username cfg) ++ "."
      return ()
  where
    username_prompt = "Username: "
    password_prompt = "Password: "

    send_cred :: Handle -> String -> IO ()
    send_cred h' s = do
      -- The carriage return is super important!
      let line = s ++ "\r\n"
      hPutStr h' line
      display_sent line -- Don't log the username/password!

    recv_chars :: Int -> Handle -> IO String
    recv_chars n h' = do
      s <- sequence [ hGetChar h' | _ <- [1..n] ]
      report_debug s
      return s

    recv_prompt :: Handle -> IO String
    recv_prompt = recv_chars 10


-- | Connect to @host@ and attempt to parse the feed. As long as we
--   stay connected and nothing bad happens, the program will remain in
--   this function. If anything goes wrong, then the current invocation
--   of connect_and_parse will return, and get called again later
--   (probably with a different @host@).
--
--  Steps:
--
--    1. Connect to @host@ on the XML feed port.
--
--    2. Log in.
--
--    3. Go into the eternal read/save loop.
--
connect_and_parse :: Configuration
                  -> String -- ^ Hostname to connect to
                  -> IO ()
connect_and_parse cfg host =
  bracket acquire_handle release_handle action
  where
    five_seconds :: Int
    five_seconds = 5 * 1000000

    acquire_handle = do
      report_info $ "Connecting to " ++ host ++ "."
      connectTo host (PortNumber 4500)

    release_handle h = do
      report_info $ "Closing connection to " ++ host ++ "."
      hClose h

    action h = do
      -- No buffering anywhere.
      hSetBuffering h NoBuffering

      -- The feed is often unresponsive after we send out username. It
      -- happens in a telnet session, too (albeit less frequently?),
      -- so there might be a bug on their end.
      --
      -- If we dump the packets with tcpdump, it looks like their
      -- software is getting confused: they send us some XML in
      -- the middle of the log-in procedure.
      --
      -- On the other hand, the documentation at
      -- <http://www.sportsnetworkdata.com/feeds/xml-levels.asp>
      -- states that you can only make one connection per username to
      -- a given host. So maybe they're simply rejecting the username
      -- in an unfriendly fashion. In any case, the easiest fix is to
      -- disconnect and try again.
      --
      login_worked <- timeout five_seconds $ log_in cfg h
      case login_worked of
        Nothing -> report_info $ "Login timed out (5 seconds). "
                                   ++ "Waiting 5 seconds to reconnect."

        -- If loop returns (due to its timeout), it will pop out right
        -- here and the action will terminate causing 'release_handle'
        -- to trigger.
        Just _ ->  loop cfg h []


-- | A wrapper around threadDelay which takes seconds instead of
--   microseconds as its argument.
--
thread_sleep :: Int -- ^ Number of seconds for which to sleep.
             -> IO ()
thread_sleep seconds = do
  let microseconds = seconds * (10 ^ (6 :: Int))
  threadDelay microseconds


-- | The entry point of the program.
--
main :: IO ()
main = do
  rc_cfg <- OC.from_rc
  cmd_cfg <- get_args

  -- Merge the config file options with the command-line ones,
  -- prefering the command-line ones.
  let opt_config = rc_cfg <> cmd_cfg

  -- Update a default config with any options that have been set in
  -- either the config file or on the command-line.  We initialize
  -- logging before the missing parameter checks below so that we can
  -- log the errors.
  let cfg = (def :: Configuration) `merge_optional` opt_config
  init_logging (log_level cfg) (log_file cfg) (syslog cfg)

  -- Check the optional config for missing required options.
  when (isNothing (OC.password opt_config)) $ do
    report_error "No password supplied."
    exitWith (ExitFailure exit_no_password)

  when (isNothing (OC.username opt_config)) $ do
    report_error "No username supplied."
    exitWith (ExitFailure exit_no_username)

  -- This should be impossible. We had a choice to make: since the
  -- command-line feed_hosts are usually not supplied, we don't want
  -- to take the empty list supplied on the command-line and use
  -- that. But that means that we need to do the same thing if the
  -- user supplies an empty list in the config file. That "same thing"
  -- is to use the default list. So, this should never be empty,
  -- because if the optional config has no feed hosts, we use the
  -- default list.
  when (null $ get_feed_hosts (feed_hosts cfg)) $ do
    report_error "No feed hosts supplied."
    exitWith (ExitFailure exit_no_feed_hosts)

  when (daemonize cfg) $ do
    -- Old PID files can be left around after an unclean shutdown. We
    -- only care if we're running as a daemon.
    pidfile_exists <- doesFileExist (pidfile cfg)
    when pidfile_exists $ do
      report_error $ "PID file " ++ (pidfile cfg) ++ " already exists. "
                       ++ "Refusing to start."
      exitWith (ExitFailure exit_pidfile_exists)

  -- This may be superstition (and I believe stderr is unbuffered),
  -- but it can't hurt.
  hSetBuffering stderr NoBuffering
  hSetBuffering stdout NoBuffering

  -- The rest of the program is kicked off by the following line which
  -- begins connecting to our feed hosts, starting with the first one,
  -- and proceeds in a round-robin fashion.
  let run_program = round_robin cfg 0

  -- If we were asked to daemonize, do that; otherwise just run the thing.
  if (daemonize cfg)
  then try_daemonize cfg run_program
  else run_program

  where
    -- | This is the top-level \"loop forever\" function. If an
    --   exception is thrown, it will propagate up to this point, where
    --   it will be logged and ignored in style.
    --
    --   Afterwards, we recurse (call ourself) again to loop more forevers.
    --
    round_robin :: Configuration -> Int -> IO ()
    round_robin cfg feed_host_idx = do
      let hosts = get_feed_hosts $ feed_hosts cfg
      let host = hosts !! feed_host_idx
      catchIOError (connect_and_parse cfg host) (report_error . show)
      thread_sleep 5 -- Wait 5s before attempting to reconnect.
      round_robin cfg $ (feed_host_idx + 1) `mod` (length hosts)


    -- | A exception handler around full_daemonize. If full_daemonize
    --   doesn't work, we report the error and crash. This is fine; we
    --   only need the program to be resilient once it actually starts.
    --
    try_daemonize :: Configuration -> IO () -> IO ()
    try_daemonize cfg program =
      catchIOError
        (full_daemonize cfg program)
        (\e -> do
          report_error (show e)
          throw e)
