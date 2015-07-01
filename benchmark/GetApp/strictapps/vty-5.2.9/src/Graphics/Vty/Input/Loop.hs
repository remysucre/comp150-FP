{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
-- | The input layer used to be a single function that correctly accounted for the non-threaded
-- runtime by emulating the terminal VMIN adn VTIME handling. This has been removed and replace with
-- a more straightforward parser. The non-threaded runtime is no longer supported.
--
-- This is an example of an algorithm where code coverage could be high, even 100%, but the
-- behavior is still under tested. I should collect more of these examples...
--
-- reference: http://www.unixwiz.net/techtips/termios-vmin-vtime.html
module Graphics.Vty.Input.Loop where

import Graphics.Vty.Config
import Graphics.Vty.Input.Classify
import Graphics.Vty.Input.Events

import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Monad (when, mzero, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT(..), evalStateT)
import Control.Monad.Trans.Reader (ReaderT(..))

import Data.Char
import Data.IORef
import Data.Word (Word8)

import Foreign ( allocaArray, peekArray, Ptr )
import Foreign.C.Types (CInt(..))

import System.IO
import System.Posix.IO (fdReadBuf, setFdOption, FdOption(..))
import System.Posix.Terminal
import System.Posix.Types (Fd(..))

import Text.Printf (hPrintf)

data Input = Input
    { -- | Channel of events direct from input processing. Unlike 'nextEvent' this will not refresh
      -- the display if the next event is an 'EvResize'.
      _eventChannel  :: Chan Event
      -- | Shuts down the input processing. This should return the terminal input state to before
      -- the input initialized.
    , shutdownInput :: IO ()
      -- | Changes to this value are reflected after the next event.
    , _configRef :: IORef Config
      -- | input debug log
    , _inputDebug :: Maybe Handle
    }

makeLenses ''Input

data InputBuffer = InputBuffer
    { _ptr :: Ptr Word8
    , _size :: Int
    }

makeLenses ''InputBuffer

data InputState = InputState
    { _unprocessedBytes :: String
    , _appliedConfig :: Config
    , _inputBuffer :: InputBuffer
    , _stopRequestRef :: IORef Bool
    , _classifier :: String -> KClass
    }

makeLenses ''InputState

type InputM a = StateT InputState (ReaderT Input IO) a

logMsg :: String -> InputM ()
logMsg msg = do
    d <- view inputDebug
    case d of
        Nothing -> return ()
        Just h -> liftIO $ hPutStrLn h msg >> hFlush h

-- this must be run on an OS thread dedicated to this input handling.
-- otherwise the terminal timing read behavior will block the execution of the lightweight threads.
loopInputProcessor :: InputM ()
loopInputProcessor = do
    readFromDevice >>= addBytesToProcess
    validEvents <- many parseEvent
    forM_ validEvents emit
    dropInvalid
    stopIfRequested <|> loopInputProcessor

addBytesToProcess :: String -> InputM ()
addBytesToProcess block = unprocessedBytes <>= block

emit :: Event -> InputM ()
emit event = do
    logMsg $ "parsed event: " ++ show event
    view eventChannel >>= liftIO . flip writeChan event

-- The timing requirements are assured by the VMIN and VTIME set for the device.
--
-- Precondition: Under the threaded runtime. Only current use is from a forkOS thread. That case
-- satisfies precondition.
-- TODO: When under the non-threaded runtime emulate VMIN and VTIME
readFromDevice :: InputM String
readFromDevice = do
    newConfig <- view configRef >>= liftIO . readIORef
    oldConfig <- use appliedConfig
    let Just fd = inputFd newConfig
    when (newConfig /= oldConfig) $ do
        logMsg $ "new config: " ++ show newConfig
        liftIO $ applyConfig fd newConfig
        appliedConfig .= newConfig
    bufferPtr <- use $ inputBuffer.ptr
    maxBytes  <- use $ inputBuffer.size
    stringRep <- liftIO $ do
        bytesRead <- fdReadBuf fd bufferPtr (fromIntegral maxBytes)
        if bytesRead > 0
        then fmap (map $ chr . fromIntegral) $ peekArray (fromIntegral bytesRead) bufferPtr
        else return []
    when (not $ null stringRep) $ logMsg $ "input bytes: " ++ show stringRep
    return stringRep

applyConfig :: Fd -> Config -> IO ()
applyConfig fd (Config{ vmin = Just theVmin, vtime = Just theVtime })
    = setTermTiming fd theVmin (theVtime `div` 100)
applyConfig _ _ = fail "(vty) applyConfig was not provided a complete configuration"

parseEvent :: InputM Event
parseEvent = do
    c <- use classifier
    b <- use unprocessedBytes
    case c b of
        Valid e remaining -> do
            logMsg $ "valid parse: " ++ show e
            logMsg $ "remaining: " ++ show remaining
            unprocessedBytes .= remaining
            return e
        _                   -> mzero 

dropInvalid :: InputM ()
dropInvalid = do
    c <- use classifier
    b <- use unprocessedBytes
    when (c b == Invalid) $ do
        logMsg "dropping input bytes"
        unprocessedBytes .= []

stopIfRequested :: InputM ()
stopIfRequested = do
    True <- (liftIO . readIORef) =<< use stopRequestRef
    return ()

runInputProcessorLoop :: ClassifyMap -> Input -> IORef Bool -> IO ()
runInputProcessorLoop classifyTable input stopFlag = do
    let bufferSize = 1024
    allocaArray bufferSize $ \(bufferPtr :: Ptr Word8) -> do
        s0 <- InputState [] <$> readIORef (_configRef input)
                            <*> pure (InputBuffer bufferPtr bufferSize)
                            <*> pure stopFlag
                            <*> pure (classify classifyTable)
        runReaderT (evalStateT loopInputProcessor s0) input

attributeControl :: Fd -> IO (IO (), IO ())
attributeControl fd = do
    original <- getTerminalAttributes fd
    let vtyMode = foldl withoutMode original [ StartStopOutput, KeyboardInterrupts
                                             , EnableEcho, ProcessInput, ExtendedFunctions
                                             ]
    let setAttrs = setTerminalAttributes fd vtyMode Immediately
        unsetAttrs = setTerminalAttributes fd original Immediately
    return (setAttrs,unsetAttrs)

logInitialInputState :: Input -> ClassifyMap -> IO()
logInitialInputState input classifyTable = case _inputDebug input of
    Nothing -> return ()
    Just h  -> do
        Config{ vmin = Just theVmin
              , vtime = Just theVtime
              , termName = Just theTerm, .. } <- readIORef $ _configRef input
        _ <- hPrintf h "initial (vmin,vtime): %s\n" (show (theVmin, theVtime))
        forM_ classifyTable $ \i -> case i of
            (inBytes, EvKey k mods) -> hPrintf h "map %s %s %s %s\n" (show theTerm)
                                                                     (show inBytes)
                                                                     (show k)
                                                                     (show mods)
            _ -> return ()

initInput :: Config -> ClassifyMap -> IO Input
initInput config classifyTable = do
    let Just fd = inputFd config
    setFdOption fd NonBlockingRead False
    applyConfig fd config
    stopFlag <- newIORef False
    input <- Input <$> newChan
                   <*> pure (writeIORef stopFlag True)
                   <*> newIORef config
                   <*> maybe (return Nothing)
                             (\f -> Just <$> openFile f AppendMode)
                             (debugLog config)
    logInitialInputState input classifyTable
    _ <- forkOS $ runInputProcessorLoop classifyTable input stopFlag
    return input

foreign import ccall "vty_set_term_timing" setTermTiming :: Fd -> Int -> Int -> IO ()
