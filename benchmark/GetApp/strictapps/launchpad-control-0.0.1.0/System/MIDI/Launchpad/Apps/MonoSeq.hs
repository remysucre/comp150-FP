
-- | A monophonic sequencer app.
--
-- Each note can have a velocity, length, offset (small delay relative to 
-- the grid position), and 5 custom CC commands.
-- The parameters can be set up using the 8 triangle buttons (top one is 
-- velocity, second is length, etc).
--
-- When there are more than 8 step, you can scroll with the left/right buttons
-- (jumping 8 steps).
--
-- Example usage:
--
-- > main = runPureApp defaultGlobalConfig $ monoSequencer defaultCfg
--

{-# LANGUAGE BangPatterns #-}
module System.MIDI.Launchpad.Apps.MonoSeq where

--------------------------------------------------------------------------------

import Data.List

import Control.Monad
import System.MIDI

import Data.Array.Unboxed
import Data.Array.IArray

import System.MIDI.Launchpad.Control
import System.MIDI.Launchpad.AppFramework

--------------------------------------------------------------------------------

data Scale 
  = Chromatic 
  | Pentatonic
  | CMinor 
  | CMajor
  deriving (Eq,Show)
  
data Cfg = Cfg 
  { seqSteps       :: !Int   -- ^ How many steps we have (it can be more than 8!)
  , stepResolution :: !Int   -- ^ Length of a step. 24 is quarter note, 12 is 1/8th, etc.
  , midiFrom       :: !Int   -- ^ which note should be the lowest (MIDI notes, for example 36 or 48 or 60 are C notes)
  , midiScale      :: !Scale
  , ccFrom         :: !Int   -- ^ where to start to 5 consecutive CC commands
  }
  deriving Show
  
-- | 8 steps by default, and 1/8th note per step
defaultCfg :: Cfg
defaultCfg = Cfg 
  { seqSteps       = 8 
  , stepResolution = 12  
  , midiFrom       = 48
  , midiScale      = Pentatonic
  , ccFrom         = 90
  }

--------------------------------------------------------------------------------

data Mode 
  = Notes 
  | Params !Int
  deriving (Eq,Ord,Show)
  
data State = State 
  { _playing   :: !Bool
  , _screenPos :: !Int
  , _notes     :: !(UArray Int Int)
  , _params    :: !(UArray (Int,Int) Int)
  , _playNotes :: [PlayNote]
  }
  deriving (Eq,Ord,Show)

-- | Notes played at the moment
data PlayNote = PlayNote
  { _note     :: !Int
  , _stopAt   :: !Int
  }
  deriving (Eq,Ord,Show)

-- | A monophonic sequencer app.     
monoSequencer :: Cfg -> PureApp Cfg Mode State
monoSequencer cfg = PureApp
  { pAppConfig    = cfg
  , pAppIniState  = (Notes, initialState cfg)
  , pAppStartStop = startStop
  , pAppRender    = render
  , pAppButton    = button
  , pAppSync      = sync
  } 

--------------------------------------------------------------------------------
    
initialState :: Cfg -> State    
initialState cfg@(Cfg seqSteps stepResolution _ _ _) = State 
  { _playing   = False
  , _screenPos = 0
  , _notes     = listArray (0,seqSteps+7)   (repeat (-1))
  , _params    = listArray ((0,0),(seqSteps+7,7)) 
               $ concat $ transpose $ 
               [ rep 5 , rep 3 , rep 0 , rep 4
               , rep 4 , rep 4 , rep 4 , rep 4 ]
  , _playNotes = []
  }
  where
    rep = replicate seqSteps
  
startStop :: Cfg -> Bool -> State -> State
startStop cfg playing state = state { _playing = playing }

--------------------------------------------------------------------------------

button :: Cfg -> ButtonPress -> (Mode,State) -> ((Mode,State),[MidiMessage'])
button cfg@(Cfg seqSteps stepResolution _ _ _) (Release _) old = (old,[])
button cfg@(Cfg seqSteps stepResolution _ _ _) (Press but) (mode,state) = 

  case but of

    Dir d -> case d of
      L -> ((mode, state { _screenPos = max (pos-8) 0             }) , []) 
      R -> ((mode, state { _screenPos = min (pos+8) lastScreenPos }) , [])
      _ -> ((mode,state),[])

    Side k -> case mode of
      Notes    -> ( ( Params k , state ) , [] )
      Params u -> ( ( if u/=k then Params k else Notes , state ) 
                  , if _playing state then [] else [CC (ccNumber cfg k) 64] )
      
    Pad x y -> case mode of

      Notes -> ( ( mode, state { _notes = notes // [(pos+x, new)] } ) , [] ) where
        new = if old==y then -1 else y 
        old = notes!(pos+x)
      
      Params u -> ( ( mode, state { _params = params // [((pos+x,u), 7-y)] } ) , [] ) where
        old = params!(pos+x,u)
    
    _ -> ((mode,state),[])
       
  where 

    lastScreenPos = 8 * div (seqSteps - 1) 8

    pos    = _screenPos state
    notes  = _notes  state
    params = _params state

--------------------------------------------------------------------------------

counterStep :: Cfg -> Int -> Int 
counterStep (Cfg seqSteps stepResolution _ _ _) cnt = ((div cnt stepResolution) `mod` seqSteps)

invCounterStep :: Cfg -> Int -> Int
invCounterStep (Cfg seqSteps stepResolution _ _ _) step = step*stepResolution

totalTicks :: Cfg -> Int
totalTicks (Cfg seqSteps stepResolution _ _ _) = stepResolution * seqSteps

ccNumber :: Cfg -> Int -> Int
ccNumber (Cfg _ _ _ _ ccFrom) y = ccFrom + y - 3

noteNumber :: Cfg -> Int -> Int
noteNumber (Cfg _ _ midiFrom midiScale _) y = 
  case midiScale of
    Chromatic  -> midiFrom + y 
    Pentatonic -> midiFrom + penta  !! y 
    CMajor     -> midiFrom + cmajor !! y 
    CMinor     -> midiFrom + cminor !! y 
  where
    penta  = [ 0,2,4,  7,9,    12,14,16,   19,21   ] 
    cmajor = [ 0,2,4,5,7,9,11, 12,14,16,17,19,21,23] 
    cminor = [ 0,2,3,5,7,8,10, 12,14,15,17,19,20,22] 
    
--------------------------------------------------------------------------------

sync :: Cfg -> Mode -> State -> Int -> (State,[MidiMessage'])    
sync cfg@(Cfg seqSteps stepResolution midiFrom midiScale ccFrom) mode state counter = (state',msgs) where

  state'  = state { _playNotes = newNotes ++ contNotes }
  newNotes = [ PlayNote note (counter + 2*(len+1)) 
             | x <- newIdx, let len = (div stepResolution 4) * (1+params!(x,1))
             , let note = notes!x, note>=0 ]

  newIdx  = [ x | x <- [0..seqSteps-1], let y = notes!x, y>=0
                , invCounterStep cfg x + delay x == mod counter (totalTicks cfg) ]

  (stopNotes, contNotes) = partition (\(PlayNote note stop) -> stop == counter) (_playNotes state)
   
  notes  = _notes  state
  params = _params state
  delay x = params!(x,2)    --  0 is velocity, 1 is length, 2 is delay
  
  msgs =  [ NoteOff (toNote stop)  64  | PlayNote stop _  <- stopNotes ]
       ++ [ CC      (ccNumber cfg y) value | x <- newIdx, let start = notes!x
                                           , y <-[3..7] , let value = (params!(x,y))*16 ]
       ++ [ NoteOn  (toNote start) vel | x <- newIdx, let start = notes!x
                                       , let vel = (params!(x,0)+1)*16-1 ] 
 
  toNote y = if y>=0 then noteNumber cfg (7-y) else error "MonoSeq/sync/toNote: shouldn't happen"


--------------------------------------------------------------------------------

renderArrows :: Cfg -> State -> [(Button,Color)]
renderArrows cfg@(Cfg seqSteps _ _ _ _) state = concat
  [ if _screenPos state > 0           then [(Dir L, green)] else []
  , if _screenPos state < seqSteps-8  then [(Dir R, green)] else [] 
  ]
  
render :: Cfg -> Mode -> State -> Maybe Int -> [(Button,Color)]
render cfg mode state msync = renderArrows cfg state ++ stuff where

  pos = _screenPos state
  notes  = _notes  state
  params = _params state 
  
  steps = seqSteps cfg
  
  column = case msync of
    Nothing  -> (-1)
    Just cnt -> counterStep cfg cnt - pos
     
  stuff = case mode of
  
    Notes -> time ++ note where
      time = if column >= 0 && column < 8 then [ (Pad column y, amber) | y<-[0..7] ] else [] 
      note = [ (Pad x y, color) | x<-[0..7], let y = notes!(pos+x), pos+x<steps, y>=0
                                , let color = if column==x then orange else red ]
 
    Params u -> time ++ par ++ side where
      time = if column >= 0 && column < 8 then [ (Pad column 0, amber) ] else []
      par  = [ (Pad x y, color) | x<-[0..7], let p = params!(pos+x,u), pos+x<steps, p>0, y<-[7-p..7]
                                , let color = if column==x && y==0 then yellow else green ] 
      side = [ (Side u, green) ]
      
--------------------------------------------------------------------------------
                                
                                