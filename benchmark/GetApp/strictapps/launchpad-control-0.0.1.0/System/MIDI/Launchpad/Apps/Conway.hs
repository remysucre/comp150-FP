
-- | Conway's game of life on a 8x8 torus grid, outputting sound.
--
-- Press buttons to turn turn them on. The simulation is running only if 
-- there is external MIDI sync signal coming (that is, press play in your DAW
-- of choice).
--
-- The triangle side buttons trigger predefined patterns (the bottom one 
-- erasing the grid).
-- 
-- The directional buttons choose between four different modes of 
-- associating notes to the grid cells.
--
-- Example usage:
--
-- > main = runPureApp defaultGlobalConfig $ conway defaultCfg
--

{-# LANGUAGE BangPatterns #-}
module System.MIDI.Launchpad.Apps.Conway where

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
  { noteFrom      :: !Int      -- ^ the first note (eg. 60 is middle C)
  , midiScale     :: !Scale    -- ^ the musical scale to use 
  , stepFrequency :: !Int      -- ^ speed of the simulation (larger is slower)
  }
  deriving Show
  
defaultCfg :: Cfg
defaultCfg = Cfg 
  { noteFrom  = 0
  , midiScale = Chromatic -- Pentatonic
  , stepFrequency = 12
  }
  
--------------------------------------------------------------------------------

data Scale 
  = Chromatic 
  | Pentatonic
  | CMinor 
  | CMajor
  deriving (Eq,Show)

noteNumber :: Cfg -> Int -> Int
noteNumber (Cfg midiFrom midiScale _) y = 
  case midiScale of
    Chromatic  -> midiFrom + y 
    Pentatonic -> midiFrom + penta  !! y 
    CMajor     -> midiFrom + cmajor !! y 
    CMinor     -> midiFrom + cminor !! y 
  where
    penta  = [ 0,2,4,  7,9,    12,14,16,   19,21   ] 
    cmajor = [ 0,2,4,5,7,9,11, 12,14,16,17,19,21,23] 
    cminor = [ 0,2,3,5,7,8,10, 12,14,15,17,19,20,22] 
    
gridNote :: Cfg -> Dir -> (Int,Int) -> Int
gridNote cfg notemode (x,y) = flip mod 128 $ case notemode of
  U -> noteNumber cfg    y  + 12*x
  D -> noteNumber cfg (7-y) + 12*x   
  L -> noteNumber cfg    x  + 12*y
  R -> noteNumber cfg (7-x) + 12*y
  
--------------------------------------------------------------------------------

data Mode = Conway deriving (Eq,Ord,Show)
  
data State = State 
  { _table       :: !(UArray (Int,Int) Bool)
  , _playing     :: !Bool
  , _screen      :: !Int
  , _noteMode    :: !Dir      -- ^ four different ways to associate notes to the grid cells
  }
  deriving (Eq,Ord,Show)

                 
initialState :: State 
initialState = State 
  { _table    = predefinedTable 0
  , _playing  = False
  , _screen   = 0
  , _noteMode = U
  }

--------------------------------------------------------------------------------

readTable :: [String] -> UArray (Int,Int) Bool
readTable lines = table where
  table = accumArray (flip const) False ((0,0),(7,7)) elems
  elems = [ ((x,y), c/=' ') | (y,line) <- zip [0..] lines, (x,c) <- zip [0..] line ]

-- | A \"block-laying switch engine\"
table0 :: [String]
table0 =
  [ ""
  , " xxx x"
  , " x    "
  , "    xx"
  , "  xx x"
  , " x x x"
  ]
  
-- | The famous \"glider\"
table1 :: [String]
table1 =
  [ "" 
  , "  x "
  , "   x"
  , " xxx"
  ]
  
-- | The \"Lightweight spaceship\"
table2 :: [String]
table2 = 
  [ ""
  , " x  x"
  , "     x"
  , " x   x"
  , "  xxxx"
  ]
  
-- | \"Toad\" (period 2 oscillator)
table3 :: [String]
table3 = 
  [ ""
  , "   x "
  , " x  x"
  , " x  x"
  , "  x  "
  ]

-- | \"Acorn\" 
table4 :: [String]
table4 = 
  [ ""
  , ""
  , " x     "
  , "   x   "
  , "xx  xxx"
  ]

-- | Almost a \"Loaf\" (stationary), but added 1 extra cell to have something
table5 :: [String]
table5 = 
  [ "      x" 
  , "  xx   "
  , " x xx  "
  , "  x x  "
  , "   x   "
  ]

-- | \"R-pentonimo\"
table6 :: [String]
table6 = 
  [ "    " 
  , "  xx"
  , " xx "
  , "  x "
  ]
  
predefinedTable :: Int -> UArray (Int,Int) Bool
predefinedTable k = readTable $ if k==7 then [] else (cycle allTables) !! k where
  allTables = 
    [ table0
    , table1
    , table2
    , table3
    , table4
    , table5
    , table6
    ]

--------------------------------------------------------------------------------  
       
-- | Conway's game of life on a 8x8 grid
conway :: Cfg -> MonadicApp Cfg Mode State
conway cfg = MonadicApp 
  { mAppConfig    = cfg
  , mAppIniState  = (Conway,initialState)
  , mAppRender    = render
  , mAppButton    = button
  , mAppStartStop = startStop
  , mAppSync      = sync
  } 

--------------------------------------------------------------------------------  

neighbours :: (Int,Int) -> [(Int,Int)]
neighbours (x,y) = [(x-1,y  ),(x+1,y  ),(x  ,y-1),(x  ,y+1)
                   ,(x-1,y-1),(x+1,y-1),(x-1,y+1),(x+1,y+1)
                   ]

rule :: Bool -> Int -> Bool
rule True  k = (k==2) || (k==3)
rule False 3 = True
rule _     _ = False
  
step :: State -> State
step state = state { _table = newtable } where
  oldtable = _table state 
  newtable = array ((0,0),(7,7)) [ (xy, rule (lkp xy) (countNeighbours xy)) | x<-[0..7], y<-[0..7], let xy=(x,y) ]
  lkp (x,y) = oldtable ! (mod x 8, mod y 8)
  countNeighbours xy = length $ filter id $ map lkp $ neighbours xy

--------------------------------------------------------------------------------  
 
startStop :: Cfg -> Bool -> State -> State
startStop _ playing state = state { _playing = playing }

--------------------------------------------------------------------------------

button :: Cfg -> ButtonPress -> ButtonMonad Mode State ()
button cfg press = do
  case but of
    Side k   -> modifyState $ \old -> old { _table = predefinedTable k , _screen = k }     
    Dir  d   -> when down $ modifyState $ \old -> old { _noteMode = d } 
    Pad  x y -> when down $ do
                  oldstate <- getState :: ButtonMonad Mode State State
                  let table = _table oldstate
                      new = not (table!(x,y))
                  setState $ oldstate { _table = table // [ ( (x,y), new ) ] }                
                  when (_playing oldstate) $ do 
                    let k = gridNote cfg (_noteMode oldstate) (x,y) 
                    sendMessage $ noteOnOff k new                                   
    _ -> return ()
    
  where
    (but,down) = case press of
      Press   b -> (b,True )
      Release b -> (b,False)          
         
--------------------------------------------------------------------------------

noteOnOff :: Int -> Bool -> MidiMessage'
noteOnOff k b = if b then NoteOn k 127 else NoteOff k 64

sync :: Cfg -> Mode -> Int -> SyncMonad State ()    
sync cfg@(Cfg _ _ stepFrequency) mode counter = 
  when (mod counter stepFrequency == 0) $ do 
    oldstate <- getState 
    let newstate = step oldstate
        oldtable = _table oldstate
        newtable = _table newstate
    setState $ newstate
    sendMessages [ noteOnOff k (bnew /= bold) 
                 | (xy,bnew) <- assocs newtable
                 , let bold = oldtable!xy
                 , let k = gridNote cfg (_noteMode oldstate) xy 
                 ] 
  
--------------------------------------------------------------------------------
  
render :: Cfg -> Mode -> State -> Maybe Int -> RenderMonad ()
render cfg mode state msync = do

  let sidecol = case msync of
        Just k  -> if odd (div k (stepFrequency cfg)) then red else amber
        Nothing -> green
  setButtonColor (Side (_screen state) , sidecol)  
  setButtonColor (Dir (_noteMode state) , red)
  setButtonColors [ (Pad x y, if b then yellow else None) | ((x,y),b) <- assocs (_table state) ]
                                 
--------------------------------------------------------------------------------
                                
                                