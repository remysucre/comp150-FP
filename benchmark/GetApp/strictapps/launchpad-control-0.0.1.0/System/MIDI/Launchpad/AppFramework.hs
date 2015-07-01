
-- | A framework to create functional Launchpad \"apps\".
--
-- See the modules below @System.MIDI.Launchpad.Apps@ for examples.
--
-- Notes:
--
--  * Both Ableton and the Launchpad embedded software seems to be somewhat
--    buggy... If you experience issues, try resetting the Launchpad, try to launch 
--    Ableton and your app in the opposite order, etc...
--
--  * /ALWAYS/ compile with the threaded runtime (ghc option -threaded)
--
--  * When the programs start, the Launchpad is reseted, and Session mode
--    is assumed. Press User mode 2 to start playing with the app. Sometimes you have to 
--    press Session mode \/ User mode 2 a few times so that Ableton and the launchpad
--    app thinks the same thing about the state...
--
--  * How to setup Ableton: Use a loopback device (eg. IAC Bus 1 on OSX) to
--    communicate between the app and Ableton. In Ableton midi setup,
--    enable the track and remote MIDI /input/ for the loopback device, and
--    enable the sync MIDI /output/ for the loopback device; disable everything else.
--    Also disable all Launchpad MIDI inputs and outputs (it can remain a control
--    surface).
--

{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
module System.MIDI.Launchpad.AppFramework
  ( -- * simple colors
    red , green , amber , yellow , orange
    -- * pure interface
  , PureApp(..)
  , runPureApp  
    -- * monadic interface
  , MonadicApp(..) , runMonadicApp
  , RenderMonad , ButtonMonad , SyncMonad
  , setButtonColor , setButtonColor' , setButtonColors
  , getMode , setMode 
  , CanChangeState , setState , getState , modifyState
  , CanSendMessage , sendMessage , sendMessages
    -- * global configuration
  , GlobalConfig(..) , defaultGlobalConfig
  )
  where

--------------------------------------------------------------------------------

-- import Data.List

import Control.Monad
import System.MIDI
import System.MIDI.Utility

import Control.Concurrent
import Control.Concurrent.MVar ()

import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State

-- import Data.Array.IO
import System.IO.Unsafe as Unsafe

import System.MIDI.Launchpad.Control
import System.MIDI.Launchpad.AppFramework.Internal

--------------------------------------------------------------------------------
-- * monadic interface

-- | Monadic application (equivalent to the above pure application, 
-- but may be more convenient to use)
data MonadicApp cfg mode state = MonadicApp
  { mAppConfig    :: cfg
  , mAppIniState  :: (mode,state)
  , mAppStartStop :: cfg -> Bool -> (state -> state)                        -- ^ start or stop playing
  , mAppRender    :: cfg -> mode -> state -> Maybe Int -> RenderMonad ()    -- ^ render the screen (it will optimized, don't worry); the @Maybe Int@ is the sync signal
  , mAppButton    :: cfg -> ButtonPress -> ButtonMonad mode state ()        -- ^ the user presses a button
  , mAppSync      :: cfg -> mode -> Int -> SyncMonad state ()               -- ^ external MIDI sync signal (24 times per quarter note)
  } 

newtype RenderMonad a 
  = RM { unRM :: WriterT [(Button,Color)] Identity  a } 
  deriving Monad

newtype ButtonMonad mode state a 
  = BM { unBM :: StateT (mode,state) (WriterT [ MidiMessage' ] Identity) a } 
  deriving Monad

newtype SyncMonad state a 
  = SM { unSM :: StateT state (WriterT [ MidiMessage' ] Identity) a } 
  deriving Monad

---------------------

setButtonColor :: (Button,Color) -> RenderMonad ()
setButtonColor bc = RM $ tell [bc]

setButtonColor' :: Button -> Color -> RenderMonad ()
setButtonColor' b c = setButtonColor (b,c)

setButtonColors :: [(Button,Color)] -> RenderMonad ()
setButtonColors bcs = RM $ tell bcs

---------------------

getMode :: ButtonMonad mode state mode
getMode = BM $ do
  (mode,_) <- get
  return mode

setMode :: mode -> ButtonMonad mode state ()
setMode newmode = BM $ do
  (_,b) <- get
  put (newmode,b)  

---------------------

class Monad m => CanSendMessage m where
  sendMessages :: [MidiMessage'] -> m ()
  sendMessage  ::  MidiMessage'  -> m ()
  sendMessage msg = sendMessages [msg]

instance CanSendMessage (ButtonMonad mode state) where
  sendMessages ms = BM $ lift $ tell ms

instance CanSendMessage (SyncMonad state) where
  sendMessages ms = SM $ lift $ tell ms

---------------------

class CanChangeState m where
  getState :: Monad (m state) => m state state 
  setState :: Monad (m state) => state -> m state ()
  
modifyState :: (CanChangeState m, Monad (m state)) => (state -> state) -> m state ()
modifyState f = do
  old <- getState   
  setState $! f old
    
instance CanChangeState (ButtonMonad mode) where
  getState = BM $ do
    (_,state) <- get
    return state  
  setState newstate = BM $ do
    (a,_) <- get
    put (a,newstate)

instance CanChangeState SyncMonad where
  getState   = SM $ get
  setState s = SM $ put $! s
   
--------------------------------------------------------------------------------
-- * conversion

monadicAppToPureApp :: MonadicApp cfg mode state -> PureApp cfg mode state
monadicAppToPureApp mApp@(MonadicApp cfg ini startstop render button sync) = pApp where
  pApp = PureApp
    { pAppConfig    = cfg
    , pAppIniState  = ini
    , pAppStartStop = startstop
    , pAppRender    = \c m  s i -> runIdentity $ execWriterT             (unRM $ render c m s i)
    , pAppButton    = \c b ms   -> runIdentity $ runWriterT  (execStateT (unBM $ button c b    ) ms)
    , pAppSync      = \c m  s i -> runIdentity $ runWriterT  (execStateT (unSM $ sync   c m i  )  s)
    } 

-- pureAppToMonadicApp :: PureApp mode state -> MonadicApp mode state
  
--------------------------------------------------------------------------------      
-- * run applications

-- | A default global state
defaultGlobalConfig :: GlobalConfig  
defaultGlobalConfig = GlobalConfig
  { defaultLaunchpadDevice  = "Launchpad"
  , defaultMidiOutputDevice = "IAC Bus 1"
  , outputChannel           = 1
  , onlyUserMode2           = True
  }    
  
--------------------------------------------------------------------------------      

-- | Executes a monadic application
runMonadicApp :: {- Show state => -} GlobalConfig -> MonadicApp cfg mode state -> IO ()
runMonadicApp globalConfig mApp = runPureApp globalConfig (monadicAppToPureApp mApp)

--------------------------------------------------------------------------------      

 