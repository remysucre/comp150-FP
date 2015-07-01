-- 
-- mhailist-receive - process mail for the list
--
-- Usage: mhailist-receive <list-file-dir> <sendmail-path>
--
-- This program expects the mail to be processed to be passed to it on STDIN.
--
-- Messages to listname-command@listdomain execute a command on
-- listname@listdomain for the sender of the message; commands understood
-- are "subscribe" and "unsubscribe".
--
-- Otherwise, messages to listname@listdomain are forwarded to the list
-- if the sender is subscribed to the list. If the list doesn't exist
-- we print an error and exit with an error code.
--
-- The list of subscribed addresses is stored in a file named
-- listname@listdomain in the directory given as the <list-file-dir>
-- command line argument.
--
-- Bugs:
--
--

module Mhailist.Receive (main) where

import Mhailist.Address
import Mhailist.BuildMessage (mkHeader)
import Mhailist.Error
import Mhailist.List
import Mhailist.Message
import Mhailist.Transaction

import Control.Concurrent
import Data.List
import Monad
import System
import System.Directory
import System.IO
import System.Process


----------------------------------------------------------------------

main :: IO ()
main = do result <- runErrorT processMessage
          case result of
               Left err -> error err
               Right () -> return ()

processMessage :: ErrorT String IO ()
processMessage =
       do [listFileDir, sendmailPath] <- liftIO getArgs
          input               <- liftIO getContents
          let message         = parseMessage input
          (list,action)       <- liftError $ mkList listFileDir message
          from                <- liftError $ fromAddress message
          let addrFile        = listAddrFile list
          assertFileExists $! addrFile
          addresses           <- subscribers addrFile
          liftIO $ updateSubscribers addrFile action from
          let listAddr        = (listAddress list)
              listIDHeader    = mkHeader "List-Id" (listID list)
          (addressees, msg)   <- return $
              case action of
                SendToList  -> (addresses, addHeader listIDHeader message)
                Subscribe   -> ([from], mkMessage listAddr from
                                 "Subscription confirmation"
                                 "As of now, you are subscribed.")
                Unsubscribe -> ([from], mkMessage listAddr from
                                 "Unsubscription confirmation"
                                 "As of now, you are unsubscribed.")
          liftIO $ sendMail sendmailPath addressees (rfc2822Format msg)
    where
        assertFileExists path = do x <- liftIO $ doesFileExist path
                                   unless x $ throwError "no list ID found"

sendMail :: FilePath -> [Address] -> String -> IO ()
sendMail cmd addresses mail = do
    (pin, pout, perr, procHandle) <- runInteractiveProcess
                                         cmd                   -- command
                                         (map email addresses) -- arguments
                                         Nothing Nothing       -- cwd, env
    waitIn <- alertWhenFinished (hPutStr pin mail >> hClose pin)
    waitOut <- alertWhenFinished (putStr =<< hGetContents pout)
    waitErr <- alertWhenFinished (putStr =<< hGetContents perr)
    mapM_ takeMVar [waitIn, waitOut, waitErr]
    status <- waitForProcess procHandle
    case status of
         ExitSuccess   -> return ()
         ExitFailure n -> error $ cmd ++ " failed with status " ++ show n

alertWhenFinished :: IO () -> IO (MVar ())
alertWhenFinished cmds =
    do done <- newEmptyMVar
       forkIO (cmds >> putMVar done ())
       return done
