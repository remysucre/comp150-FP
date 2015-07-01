
-- | A very simple generic live FX control surface.
--
-- There are 8 CCs (the 8 columns of the grid), and 8 trigger buttons
-- (the side buttons).
--
-- The trigger buttons can function either as an on/off switch or 
-- as gate (active only during pressed). This is selectable by pressing 
-- down the side button(s) and at the same time the up/down button.
-- By default, the top four are switched, the bottom four gate.
--
-- Example usage:
--
-- > main = runPureApp defaultGlobalConfig $ fxControl defaultCfg
--

{-# LANGUAGE BangPatterns #-}
module System.MIDI.Launchpad.Apps.FXControl where

--------------------------------------------------------------------------------

import Data.List

import Control.Monad
import System.MIDI

import Data.Array.Unboxed
import Data.Array.IArray

import System.MIDI.Launchpad.Control
import System.MIDI.Launchpad.AppFramework

--------------------------------------------------------------------------------

data Cfg  = Cfg
  { ccFrom    :: !Int      -- ^ the first CC number, corresponding to the first column on the Launchpad
  , onOffFrom :: !Int      -- ^ the first note (used as on/off switch), corresponding the topmost triangle button on Launchpad
  }
  deriving Show
  
defaultCfg :: Cfg
defaultCfg = Cfg 
  { ccFrom    = 100
  , onOffFrom = 100
  }
  
--------------------------------------------------------------------------------

data Mode = FX deriving (Eq,Ord,Show)
  
data Trigger = Switch | Gate deriving (Eq,Ord,Show) {- InvGate -}

data State = State 
  { _onOff       :: !(UArray Int Bool)
  , _isPressed   :: !(UArray Int Bool)
  , _triggerMode :: !(Array  Int Trigger) 
  , _params      :: !(UArray Int Int)
  , _playing     :: !Bool
  }
  deriving (Eq,Ord,Show)
     
-- | A very simple generic live FX control surface
fxControl :: Cfg -> PureApp Cfg Mode State
fxControl cfg = PureApp 
  { pAppConfig    = cfg
  , pAppIniState  = (FX,initialState)
  , pAppRender    = render
  , pAppButton    = button
  , pAppStartStop = startStop
  , pAppSync      = sync
  } 

--------------------------------------------------------------------------------
    
initialState :: State    
initialState = State 
  { _onOff       = listArray (0,7) (repeat False)
  , _isPressed   = listArray (0,7) (repeat False)
  , _triggerMode = listArray (0,7) (replicate 4 Switch ++ replicate 4 Gate)
  , _params      = listArray (0,7) (repeat 4)
  , _playing     = False
  }
  
startStop :: Cfg -> Bool -> State -> State
startStop _ playing state = state { _playing = playing }

--------------------------------------------------------------------------------

button :: Cfg -> ButtonPress -> (Mode,State) -> ((Mode,State),[MidiMessage'])
button (Cfg ccfrom notefrom) press (mode,state) = 
  case but of

    Dir d -> case d of
      U -> ((mode, trigger' Switch),[])
      D -> ((mode, trigger' Gate  ),[])
      _ -> ((mode,state),[])     

    Side k -> case (trigger!k) of
      Switch -> if not down 
                  then ( ( mode ,  state' k ) , [] )
                  else ( ( mode , (state' k) { _onOff = onOff // [(k,new)] } ) , [ noteOnOff k new ] )
                    where old = onOff ! k
                          new = not old 
      Gate   -> ( ( mode , (state' k) { _onOff = onOff // [(k,down)] } ) , [ noteOnOff k down ] )
      
    Pad x y -> if down
                  then ( ( mode, state { _params = params // [(x, 7-y)] } ) , [CC (ccNumber x) ((7-y)*16)] ) 
                  else ((mode,state),[])
                  
    _ -> ((mode,state),[])
       
  where 
    state' k = state { _isPressed = isPressed // [(k,down)] }

    trigger' Gate   = state { _triggerMode = trigger // [ (k,Gate  ) | (k,True) <- pressedList ] 
                            , _onOff       = onOff   // [ (k,True  ) | (k,True) <- pressedList ]  
                            }
    trigger' Switch = state { _triggerMode = trigger // [ (k,Switch) | (k,True) <- pressedList ]
                            , _onOff       = onOff   // [ (k,True  ) | (k,True) <- pressedList, trigger!k == Gate ]  
                            }
    
    noteOnOff k b = if b 
      then NoteOn (notefrom+k) 127
      else NoteOn (notefrom+k) 127    -- hmm, Ableton seems to work like that (only changes on NoteOn)

    ccNumber x = ccfrom + x
       
    onOff   = _onOff  state
    params  = _params state
    trigger   = _triggerMode state
    isPressed = _isPressed   state
    pressedList = assocs isPressed

    (but,down) = case press of
      Press   b -> (b,True )
      Release b -> (b,False)

      
--------------------------------------------------------------------------------

sync :: Cfg -> Mode -> State -> Int -> (State,[MidiMessage'])    
sync _ mode state counter = (state,[]) 

--------------------------------------------------------------------------------
  
render :: Cfg -> Mode -> State -> Maybe Int -> [(Button,Color)]
render _ mode state msync = stuff where

  onOff   = _onOff  state
  params  = _params state
  trigger   = _triggerMode state
  isPressed = _isPressed   state
  
  stuff = par ++ side
 
  par  = [ (Pad x y, color) | x<-[0..7], let p = params!x, p>0, y<-[7-p..7]
                            , let color = green ] 

  side = [ (Side y, color) | y<-[0..7], let b = onOff!y, let color = trigColor y b ] 

  trigColor y b = case trigger!y of
    Switch  -> if b then amber else None
    Gate    -> if b then red   else None
    -- InvGate -> if not b then green   else None
                             
--------------------------------------------------------------------------------
                                
                                