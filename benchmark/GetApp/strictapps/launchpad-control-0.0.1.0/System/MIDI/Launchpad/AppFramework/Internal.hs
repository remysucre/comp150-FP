
{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
module System.MIDI.Launchpad.AppFramework.Internal where

--------------------------------------------------------------------------------

-- import Data.List

import Control.Monad
import System.MIDI
import System.MIDI.Utility

import Control.Concurrent
import Control.Concurrent.MVar ()

import System.IO.Unsafe as Unsafe

import System.MIDI.Launchpad.Control

--------------------------------------------------------------------------------
-- * simple colors 

red, green, amber, yellow, orange :: Color
red    = Color Red   Full
green  = Color Green Full
amber  = Color Amber Full
yellow = Color Yellow Full
orange = RedGreen Full Low

-- | Default color of the control buttons (session, user modes, mixer), which is amber
controlColor :: Color
controlColor = amber

--------------------------------------------------------------------------------
-- * pure interface

-- | We suppose an application can have different modes (similarly as 
-- in Ableton one can have session, session overview, different mixer modes,
-- etc), which are basically different \"screens\" on the Launchpad; and also
-- a global state. See the example applications how it is intended to be used.
--
data PureApp cfg mode state = PureApp
  { pAppConfig    :: cfg                                                      -- ^ application-specific configuration
  , pAppIniState  :: (mode,state)                                             -- ^ initial state of the application
  , pAppStartStop :: cfg -> Bool -> (state -> state)                          -- ^ what to do when get start or stop playing MIDI signal
  , pAppRender    :: cfg -> mode -> state -> Maybe Int -> [(Button,Color)]    -- ^ render the screen (it will optimized, don't worry)
  , pAppButton    :: cfg -> ButtonPress -> (mode,state) -> ((mode,state),[MidiMessage'])       -- ^ the user presses a button
  , pAppSync      :: cfg -> mode -> state -> Int -> (state,[MidiMessage'])    -- ^ external MIDI sync signal (24 times per quarter note)
  } 

--------------------------------------------------------------------------------
-- * render only the difference between old and new display

safeRenderDiff :: [(Button,Color)] -> [(Button,Color)] -> Messages
safeRenderDiff old new = unsafeRenderDiff (sortNubMap old) (sortNubMap new)

-- | Optimized led update. We assume that the inputs are sorted.
unsafeRenderDiff :: [(Button,Color)] -> [(Button,Color)] -> Messages
unsafeRenderDiff old new = stuff where

  stuff = if length diff > 40
    then rapidLedUpdateList new
    else setColor diff
  
  diff = go old new
  
  go old []  = [ (b,None) | (b,c)<-old, c/=None ]
  go []  new = [ (b,c   ) | (b,c)<-new, c/=None ]
  go oos@((ob,oc):os) nns@((nb,nc):ns) = case compare ob nb of
    LT -> (ob,None) : go os  nns
    GT -> (nb,nc  ) : go oos ns
    EQ -> if nc/=oc 
      then  (nb,nc) : go os  ns
      else            go os  ns
        
--------------------------------------------------------------------------------
-- * global variables

-- | 24 tick per quarter note
theSyncCounter :: MVar Int
theSyncCounter = Unsafe.unsafePerformIO $ newMVar 0

{-
theSyncBuffer :: MVar [Double]
theSyncBuffer = unsafePerformIO $ newMVar []

-- | Estimated BPM. 
theBPM :: MVar Double
theBPM = unsafePerformIO $ newMVar 120
-}

thePlayingFlag :: MVar Bool
thePlayingFlag = Unsafe.unsafePerformIO $ newMVar False

-- | We should only use "user2" mode to be compatible with Ableton.
-- We default to session mode (a hack, but who cares :) 
theLaunchpadMode :: MVar Control
theLaunchpadMode = Unsafe.unsafePerformIO $ newMVar Session
  
theLedUpdateBuffer :: MVar [Messages]
theLedUpdateBuffer = Unsafe.unsafePerformIO $ newMVar []

theLastScreen :: MVar [(Button,Color)]
theLastScreen = Unsafe.unsafePerformIO $ newMVar []

--------------------------------------------------------------------------------      
-- * helper functions

whenUser2 :: GlobalConfig -> IO () -> IO ()
whenUser2 globalConfig action = 
  if (onlyUserMode2 globalConfig) 
    then whenUser2' action
    else action
    
whenUser2' :: IO () -> IO ()
whenUser2' action = do
  readMVar theLaunchpadMode >>= \mode -> when (mode == User2) action

pushUpdates :: Messages -> IO ()
pushUpdates new = do
  old <- takeMVar theLedUpdateBuffer
  putMVar theLedUpdateBuffer (forceList new : old)

replaceMVar :: MVar a -> a -> IO () 
replaceMVar mv !x = do
  tryTakeMVar mv
  putMVar mv x
  
forceList :: [a] -> [a]
forceList (!x:xs) = x : forceList xs
forceList [] = []
    
--------------------------------------------------------------------------------      
-- * update loop

appUpdateLoop :: IO ()
appUpdateLoop = go where

  go = do
    buf <- takeMVar theLedUpdateBuffer
    sendMsg $ concat (reverse buf)
    putMVar theLedUpdateBuffer []
    
    threadDelay (1000)  -- 1 msec
    go

--------------------------------------------------------------------------------      
-- * run applications

-- | Global configuration of an app
data GlobalConfig = GlobalConfig
  { defaultLaunchpadDevice  :: String  -- ^ Should be probably \"Launchpad\"
  , defaultMidiOutputDevice :: String  -- ^ default output device name (eg. \"IAC Bus 1\")
  , outputChannel           :: Int     -- ^ The midi channel we send the messages (towards the DAW or synth) 
  , onlyUserMode2           :: Bool    -- ^ If we want to be Ableton-compatible, we should only do anything in \"User mode 2\"
  } 
  
--------------------------------------------------------------------------------      

selectDevice :: String -> String -> IO (Source,Destination)
selectDevice prompt defaultName = do
  srclist <- enumerateSources
  src <- selectInputDevice (prompt ++ " (input):") (Just defaultName)
  dstlist <- enumerateDestinations
  dst <- selectOutputDevice (prompt ++ " (output):") (Just defaultName)
  return (src,dst)

--------------------------------------------------------------------------------      

-- | Executes a pure application
runPureApp :: {- Show state => -} GlobalConfig -> PureApp cfg mode state -> IO ()
runPureApp globalConfig clientApp = do

  let (iniMode,iniState) = pAppIniState clientApp
  -- let appConfig = pAppConfig clientApp
  appMode  <- newMVar iniMode  -- :: IO (MVar mode )
  appState <- newMVar iniState -- :: IO (MVar state)
  
  (src1,dst1) <- selectDevice "\nplease select the Launchpad midi device" (defaultLaunchpadDevice  globalConfig)
  (src2,dst2) <- selectDevice "\nplease select the target midi device"    (defaultMidiOutputDevice globalConfig)   -- "IAC Bus 1"
  
  outconn1 <- openDestination dst1
  outconn2 <- openDestination dst2

  inconn1  <- openSource src1 $ Just $ appLaunchpadCallback (globalConfig,clientApp,appMode,appState) outconn2
  inconn2  <- openSource src2 $ Just $ appSyncHandler       (globalConfig,clientApp,appMode,appState) outconn2
  
  putStrLn "\nconnected" 
  initializeLaunchpad inconn1 outconn1
  
  start inconn1 ; start inconn2
  putStrLn "started. Press 'ENTER' to exit."

  putStrLn "\n================================\n"
  
  resetLaunchpad False -- True
  forkIO $ appUpdateLoop -- outconn1 outconn2
  getLine
  
  stop  inconn1 ; stop  inconn2 ; putStrLn "stopped."  
  close inconn1 ; close inconn2 ; putStrLn "closed."

  close outconn1 ; close outconn2
  
--------------------------------------------------------------------------------      

appLaunchpadCallback 
  :: {- Show state => -} 
  (GlobalConfig, PureApp cfg mode state, MVar mode, MVar state) -> Connection -> MidiEvent -> IO ()
appLaunchpadCallback 
  app@(globalConfig,clientApp,appMode,appState) 
  outconn2 
  event@(MidiEvent _ fullmsg@(MidiMessage chn msg)) 
  = case (decodeLaunchpadMessage' fullmsg) of
    Nothing -> return ()
    Just press -> do 
      -- putStrLn (show press)
  
      case press of
        Release _      -> return ()
        Press   button -> case button of
      
          Ctrl ctrl -> do
            oldctrl <- takeMVar theLaunchpadMode 
            putMVar theLaunchpadMode ctrl
            when (oldctrl/=ctrl) $ do
              putStrLn $ "mode = " ++ show ctrl
              pushUpdates (turnOff1 (Ctrl oldctrl) ++ setColor1 (Ctrl ctrl) controlColor) 
              when (ctrl == User2) $ do
                 threadDelay (100*1000)   --  Ableton also wants to erase the launchpad. 50 msec does not seem to be always enough
                 sendMsg resetMsg
                 fullRender app
          
          _ -> return ()
      
      whenUser2 globalConfig $ do            
        mode  <- takeMVar appMode
        state <- takeMVar appState
        let cfg = pAppConfig clientApp
        let ((mode',state'),messages) = (pAppButton clientApp) cfg press (mode,state)    
        
        -- print messages
        -- print state'
        
        putMVar appMode  mode'
        putMVar appState state'  
    
        mapM_ (send outconn2) $ map (MidiMessage (outputChannel globalConfig)) messages  
        
        diffRender app
      
appLaunchpadCallback _ _ _ = return ()

--------------------------------------------------------------------------------

appSyncHandler :: (GlobalConfig, PureApp cfg mode state, MVar mode, MVar state) -> Connection -> MidiEvent -> IO ()
appSyncHandler app@(globalConfig,clientApp,appMode,appState) outconn2 event@(MidiEvent time msg) = case msg of
  
  SRTStart -> do
    -- replaceMVar theSyncBuffer []  
    replaceMVar thePlayingFlag True
    replaceMVar theSyncCounter (-1)
    
    state <- takeMVar appState
    let cfg = pAppConfig clientApp
    let !state' = (pAppStartStop clientApp) cfg True state
    putMVar appState state'

    whenUser2 globalConfig $ diffRender' (Just 0) app
    
  SRTClock -> do
    oldn <- takeMVar theSyncCounter
    let counter = oldn + 1
    putMVar theSyncCounter counter
    
    readMVar thePlayingFlag >>= \b -> when b $ do
      
      mode  <- readMVar appMode
      state <- takeMVar appState
      let cfg = pAppConfig clientApp
      let (!state',messages) = (pAppSync clientApp) cfg mode state counter
      putMVar appState state'
      
      mapM_ (send outconn2) $ map (MidiMessage (outputChannel globalConfig)) messages
      
      whenUser2 globalConfig $ diffRender' (Just counter) app
        
{- 
-- we don't really need this      
    xs <- takeMVar theSyncBuffer
    let t = fromIntegral time :: Double
        ys = (t:xs)
    putMVar theSyncBuffer (take 24 ys)
    let avg = foldl' (+) 0 (zipWith (-) ys xs) / fromIntegral (length xs)
        bpm = 2500 / avg
    replaceMVar theBPM bpm
    putStrLn $ "estimated bpm = " ++ show bpm
-}
    
  SRTStop  -> do
    replaceMVar thePlayingFlag False

    state <- takeMVar appState
    let cfg = pAppConfig clientApp
    let !state' = (pAppStartStop clientApp) cfg False state
    putMVar appState state'

    whenUser2 globalConfig $ diffRender' Nothing app

  _ -> return ()

-------------------------------------------------------------------------------- 
-- * render the buttons

diffRender :: (GlobalConfig, PureApp cfg mode state, MVar mode, MVar state) -> IO ()
diffRender app = do
  b <- readMVar thePlayingFlag
  n <- readMVar theSyncCounter
  let mcnt = if b then Just n else Nothing
  diffRender' mcnt app

fullRender :: (GlobalConfig, PureApp cfg mode state, MVar mode, MVar state) -> IO ()
fullRender app = do
  b <- readMVar thePlayingFlag
  n <- readMVar theSyncCounter
  let mcnt = if b then Just n else Nothing
  fullRender' mcnt app
  
diffRender' :: Maybe Int -> (GlobalConfig, PureApp cfg mode state, MVar mode, MVar state) -> IO ()
diffRender' mcounter (globalConfig, clientApp, appMode, appState) = do
  mode  <- readMVar appMode
  state <- readMVar appState
  let cfg = pAppConfig clientApp 
  let newScreen = sortNubMap $ (Ctrl User2, controlColor) : (pAppRender clientApp) cfg mode state mcounter
  oldScreen <- takeMVar theLastScreen
  putMVar theLastScreen newScreen
  let diff = unsafeRenderDiff oldScreen newScreen      
  pushUpdates diff
  
fullRender' :: Maybe Int -> (GlobalConfig, PureApp cfg mode state, MVar mode, MVar state) -> IO ()
fullRender' mcounter (globalConfig, clientApp, appMode, appState) = do
  mode  <- readMVar appMode
  state <- readMVar appState
  let cfg = pAppConfig clientApp 
  let newScreen = sortNubMap $ (Ctrl User2, controlColor) : (pAppRender clientApp) cfg mode state mcounter
  _ <- takeMVar theLastScreen
  putMVar theLastScreen newScreen
  let full = rapidLedUpdateList newScreen      
  pushUpdates full
  
-------------------------------------------------------------------------------- 

  