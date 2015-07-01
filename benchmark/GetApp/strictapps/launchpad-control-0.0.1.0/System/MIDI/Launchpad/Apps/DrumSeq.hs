

-- | A simple drum sequencer app.
--
-- Each row plays a different note. 
-- Each note can have a velocity; to set this, press the triangle button on the
-- right corresponding to the given row; then the columns represent velocities.
--
-- When there are more than 8 step, you can scroll with the left/right buttons
-- (jumping 8 steps).

-- Example usage:
--
-- > main = runMonadicApp defaultGlobalConfig $ drumSequencer defaultCfg
--

{-# LANGUAGE BangPatterns #-}
module System.MIDI.Launchpad.Apps.DrumSeq where

--------------------------------------------------------------------------------

import Data.List

import Control.Monad
import System.MIDI

import Data.Array.Unboxed
import Data.Array.IArray

import System.MIDI.Launchpad.Control
import System.MIDI.Launchpad.AppFramework

--------------------------------------------------------------------------------
  
data Cfg = Cfg 
  { seqSteps        :: !Int   -- ^ How many steps we have (it can be more than 8!)
  , stepResolution  :: !Int   -- ^ Length of a step. 24 is quarter note, 12 is 1/8th, etc.
  , midiFrom        :: !Int   -- ^ which note should be the lowest (MIDI notes, for example 36 or 48 or 60 are C notes)
  , defaultVelocity :: !Int   -- ^ default velocity of a note (0..7)
  }
  deriving Show
  
-- | 8 steps by default, and 1/8th note per step
defaultCfg :: Cfg
defaultCfg = Cfg 
  { seqSteps         = 8 
  , stepResolution   = 12  
  , midiFrom         = 36    -- in ableton, drum racks typically start here?
  , defaultVelocity  = 5    
  }

--------------------------------------------------------------------------------

data Mode 
  = Pattern 
  | Velocities !Int
  deriving (Eq,Ord,Show)
  
data State = State 
  { _playing    :: !Bool
  , _screenPos  :: !Int
  , _notes      :: !(UArray (Int,Int) Int)    -- ^ encoding both velocities and notes
  , _playNotes  :: [PlayNote]
  }
  deriving (Eq,Ord,Show)

-- | Notes played at the moment
data PlayNote = PlayNote
  { _note     :: !Int
  , _stopAt   :: !Int
  }
  deriving (Eq,Ord,Show)

-- | A drum sequencer app.     
drumSequencer :: Cfg -> MonadicApp Cfg Mode State
drumSequencer cfg = MonadicApp
  { mAppConfig    = cfg
  , mAppIniState  = (Pattern, initialState cfg)
  , mAppStartStop = startStop
  , mAppRender    = render
  , mAppButton    = button
  , mAppSync      = sync
  } 

--------------------------------------------------------------------------------
    
initialState :: Cfg -> State    
initialState cfg@(Cfg seqSteps stepResolution _ _) = State 
  { _playing   = False
  , _screenPos = 0
  , _notes     = listArray ((0,0),(seqSteps+7,7)) (repeat (-1))
  , _playNotes = []
  }
  where
    rep = replicate seqSteps
  
startStop :: Cfg -> Bool -> State -> State
startStop cfg playing state = state { _playing = playing }

--------------------------------------------------------------------------------

button :: Cfg -> ButtonPress -> ButtonMonad Mode State ()
button _                        (Release _) = return ()
button cfg@(Cfg seqSteps _ _ _) (Press but) = do

  mode  <- getMode
  state <- getState 

  let pos     = _screenPos state
      notes   = _notes  state

  let lastScreenPos = 8 * div (seqSteps - 1) 8

  case but of

    Dir d -> case d of
      L -> setState $ state { _screenPos = max (pos-8) 0             } 
      R -> setState $ state { _screenPos = min (pos+8) lastScreenPos }
      _ -> return ()

    Side k -> case mode of
      Pattern      -> setMode $ Velocities k
      Velocities u -> setMode $ if u/=k then Velocities k else Pattern 
      
    Pad x y -> case mode of

      Pattern -> do
        let old = notes!(pos+x,y)
            new = if old>=0 then -1 else (defaultVelocity cfg) 
        setState $ state { _notes = notes // [((pos+x,y),new)] } 
        return ()
{-        
        -- also give a sound? but who will stop it?
        when (not $ _playing state) $ do
          sendMessage $ noteOnOff cfg True (7-y) (defaultVelocity cfg)
-}
            
      Velocities u -> when (notes!(pos+x,u) >= 0) $
                        setState $ state { _notes = notes // [((pos+x,u), 7-y)] } where
    
    _ -> return ()
       

--------------------------------------------------------------------------------

counterStep :: Cfg -> Int -> Int 
counterStep (Cfg seqSteps stepResolution _ _) cnt = ((div cnt stepResolution) `mod` seqSteps)

invCounterStep :: Cfg -> Int -> Int
invCounterStep (Cfg seqSteps stepResolution _ _) step = step*stepResolution

totalTicks :: Cfg -> Int
totalTicks (Cfg seqSteps stepResolution _ _) = stepResolution * seqSteps

-- velo 0..7 (ahol 0 nem nulla hanem -1 az igazi csondes)
noteOnOff :: Cfg -> Bool -> Int -> Int -> MidiMessage'
noteOnOff cfg True  y velo = NoteOn  (midiFrom cfg + 7-y) ((velo+1)*16-1)
noteOnOff cfg False y velo = NoteOff (midiFrom cfg + 7-y) 64
    
--------------------------------------------------------------------------------

sync :: Cfg -> Mode -> Int -> SyncMonad State ()    
sync cfg@(Cfg seqSteps stepResolution midiFrom _) mode counter = do
  state <- getState  

  let notes  = _notes  state

  let newIdx = [ (x,y) | x <- [0..seqSteps-1], y<-[0..7], let v = notes!(x,y), v>=0
                       , invCounterStep cfg x == mod counter (totalTicks cfg) ]

  let newNotes = [ PlayNote y (counter + stepResolution) 
                 | (x,y) <- newIdx ]
   
  let (stopNotes, contNotes) = partition (\(PlayNote note stop) -> stop == counter) (_playNotes state)
       
  sendMessages [ noteOnOff cfg False stop 64  | PlayNote stop _  <- stopNotes ]
  sendMessages [ noteOnOff cfg True  y    vel | (x,y) <- newIdx, let vel = notes!(x,y) ]
  
  setState $ state { _playNotes = newNotes ++ contNotes }


--------------------------------------------------------------------------------

renderArrows :: Cfg -> State -> [(Button,Color)]
renderArrows cfg state = concat
  [ if _screenPos state > 0                 then [(Dir L, green)] else []
  , if _screenPos state < (seqSteps cfg)-8  then [(Dir R, green)] else [] 
  ]
  
render :: Cfg -> Mode -> State -> Maybe Int -> RenderMonad ()
render cfg mode state msync = 
  do
  
    setButtonColors $ renderArrows cfg state 
    setButtonColors $ stuff 
  
  where

    pos = _screenPos state
    notes  = _notes  state
    
    steps = seqSteps cfg
    
    column = case msync of
      Nothing  -> (-1)
      Just cnt -> counterStep cfg cnt - pos
      
    stuff = case mode of
    
      Pattern -> time ++ note where
        time = if column >= 0 && column < 8 then [ (Pad column y, amber) | y<-[0..7] ] else [] 
        note = [ (Pad x y, color) | x<-[0..7], y<-[0..7], let v = notes!(pos+x,y), pos+x<steps, v>=0
                                  , let color = if column==x then orange else red ]
   
      Velocities u -> time ++ par ++ side where
        time = if column >= 0 && column < 8 then [ (Pad column 0, amber) ] else []
        par  = [ (Pad x y, color) | x<-[0..7], let p = notes!(pos+x,u), p>=0, pos+x<steps, y<-[7-p..7]
                                  , let color = if column==x && y==0 then yellow else green ] 
        side = [ (Side u, green) ]
      
--------------------------------------------------------------------------------
                                
                                