
-- | Low-level interface to the Novation Launchpad.

module System.MIDI.Launchpad.Control where

--------------------------------------------------------------------------------

import Data.Array
import Data.Bits

import Control.Concurrent
import Control.Concurrent.MVar ()
import Control.Monad

-- import Data.Set (Set) ; import qualified Data.Set as Set
import Data.Map (Map) ; import qualified Data.Map as Map

import System.IO.Unsafe as Unsafe

import System.MIDI

-- import Debug.Trace

--------------------------------------------------------------------------------
-- * definitions

-- | A button of the launchpad. Numbering starts from zero. 
-- 
-- (Note that the derived ordering is the same as the \"rapid LED update\" order!)
--
data Button
  = Pad  { _padX    :: !Int , _padY :: !Int }  -- ^ the 64 buttons in the grid
  | Side { _sideCol :: !Int }       -- ^ the 8 buttons on the right side 
  | Dir  { _unDir   :: !Dir }       -- ^ the left  4 buttons in the control row
  | Ctrl { _unCtrl  :: !Control }   -- ^ the right 4 buttons in the control row
  deriving (Eq,{-Ord,-}Show)
  
instance Ord Button where

  compare (Pad x1 y1) (Pad x2 y2) = compare (y1,x1) (y2,x2)    -- !!
  compare (Pad _ _  ) _           = LT
  compare _           (Pad _ _  ) = GT

  compare (Side a)    (Side b)    = compare a b
  compare (Side _)    _           = LT
  compare _           (Side _)    = GT
  
  compare (Dir d1)    (Dir d2)    = compare d1 d2
  compare (Dir _ )    _           = LT
  compare _           (Dir _ )    = GT
  
  compare (Ctrl c1)   (Ctrl c2)   = compare c1 c2  

--  compare x y = trace "jajj" $ trace (show x ++ " | " ++ show y) $ error (show x ++ " | " ++ show y)
  
--------------------------------------------------------------------------------  
  
-- | A direction, also the left top 4 control buttons in the top row.
data Dir = U | D | L | R deriving (Eq,Ord,Show)

-- | A control button (right 4 in the top row)
data Control 
  = Session 
  | User1 
  | User2 
  | Mixer
  deriving (Eq,Ord,Show)
  
-- | Double-buffering.
data Buffer = Front | Back deriving (Eq,Ord,Show) 

-- | Note: there is some overlap between 'Yellow' and 'Amber'.
data FullColor = Red | Amber | Yellow | Green deriving (Eq,Ord,Show)

-- | Note: there is some overlap between 'Off' and 'None',
data Brightness = Off | Low | Medium | Full deriving (Eq,Ord,Show)

-- | A color. There are two possible specifications:
--
--  * either a predefined color with a brightness;
--
--  * or exact control of the red and greed leds.
--
data Color 
  = None
  | Color    !FullColor !Brightness
  | RedGreen !Brightness !Brightness
--  | Flash !FulLColor
  deriving (Eq,Ord,Show)
   
--------------------------------------------------------------------------------
-- * basic midi 

type Message  = MidiMessage
type Messages = [Message]

noteOn, noteOff, cc :: Int -> Int -> Message
noteOn  k v  = MidiMessage 1 (NoteOn  k v)
noteOff k v  = MidiMessage 1 (NoteOff k v)
cc      k v  = MidiMessage 1 (CC      k v)

-- | in-connection, out-connection
theGlobalConnections :: MVar (Connection,Connection)
theGlobalConnections = Unsafe.unsafePerformIO newEmptyMVar 

initializeLaunchpad :: Connection -> Connection -> IO ()
initializeLaunchpad inconn outconn = do
  _ <- tryTakeMVar theGlobalConnections
  putMVar theGlobalConnections (inconn,outconn)
  handShake -- ??????????
  
sendMsg :: Messages -> IO ()
sendMsg msgs = do
  (inconn,outconn) <- readMVar theGlobalConnections
  mapM_ (send outconn) msgs 
  
--------------------------------------------------------------------------------
-- * encoding colors

encodeColor :: Color -> Int
encodeColor None = 12
encodeColor (Color full br) = colorTable full br
encodeColor (RedGreen a b) = encodeBrightness a + 12 + 16 * encodeBrightness b 
-- encodeColor (Flash full   ) = flashColor full

encodeBrightness :: Brightness -> Int
encodeBrightness br = case br of
  Off -> 0
  Low -> 1
  Medium -> 2 
  Full -> 3  

colorTable :: FullColor -> Brightness -> Int
colorTable _      Off = 12
colorTable Red    br  = case br of { Low -> 13 ; Medium -> 14 ; Full -> 15 ; Off -> 12 }
colorTable Amber  br  = case br of { Low -> 29 ; Medium -> 46 ; Full -> 63 ; Off -> 12 }
colorTable Yellow br  = case br of { Low -> 45 ; Medium -> 45 ; Full -> 62 ; Off -> 12 }
colorTable Green  br  = case br of { Low -> 28 ; Medium -> 44 ; Full -> 60 ; Off -> 12  }

flashColor :: FullColor -> Int
flashColor c = case c of
  Red    -> 11
  Amber  -> 59
  Yellow -> 58
  Green  -> 56

{-  
xyLayout :: Button -> Int
xyLayout b = case b of
  Pad x y -> x + y*16
  Side y  -> 8 + y*16
  Ctrl _  -> error "xyLayout"
-}

--------------------------------------------------------------------------------
-- * setting single leds

setColor1 :: Button -> Color -> Messages
setColor1 but col = [setColor' but (encodeColor col)]

turnOff1 :: Button -> Messages
turnOff1 but = setColor1 but None

turnOff :: [Button] -> Messages
turnOff buts = setColor $ zip buts (repeat None)

setColor :: [(Button,Color)] -> Messages
setColor bcs = map f bcs where f (b,c) = setColor' b (encodeColor c)

setColor' :: Button -> Int -> Message
setColor' but dat = case but of
  Pad x y -> noteOn (x + y*16) dat
  Side y  -> noteOn (8 + y*16) dat
  _       -> cc (marshalControl but) dat
  
--------------------------------------------------------------------------------
-- * control buttons

marshalControl :: Button -> Int
marshalControl (Dir  d) = case d of { U -> 104 ; D -> 105 ; L -> 106 ; R -> 107 }
marshalControl (Ctrl c) = case c of 
  Session -> 108
  User1   -> 109
  User2   -> 110
  Mixer   -> 111
marshalControl _ = error "marshalControl"

unmarshalControl' :: Int -> Maybe Button
unmarshalControl' key = case key of
  104 -> Just $ Dir U
  105 -> Just $ Dir D
  106 -> Just $ Dir L
  107 -> Just $ Dir R
  108 -> Just $ Ctrl Session
  109 -> Just $ Ctrl User1
  110 -> Just $ Ctrl User2
  111 -> Just $ Ctrl Mixer
  _   -> Nothing

unmarshalControl :: Int -> Button  
unmarshalControl key = case unmarshalControl' key of
  Just but -> but
  Nothing  -> error ("unmarshalControl: " ++ show key)
 
--------------------------------------------------------------------------------
-- * initialization

-- | Officially, reset is simply @CC 0 0@. But the Launchpad implementation
-- is rather strange and somewhat stupid, see
-- <http://linuxaudio.org/mailarchive/lau/2012/7/12/191303>
--
-- This convoluted reset sequence may or may not help...
resetMsg :: Messages
resetMsg = 
  [ cc 0 2, noteOn 64 12       -- just do something with both cc and noteon
  , cc 0 1, noteOn 0  12
  , noteOff 0 0                -- just to be safe??????
  , cc 0 0                     -- reset
  , cc 0 48                    -- double buffering control      
  ]
   
-- | Turns on all leds
turnOnAll :: Brightness -> Messages
turnOnAll Off    = []
turnOnAll Low    = [cc 0 125]
turnOnAll Medium = [cc 0 126]
turnOnAll Full   = [cc 0 127]

-- | The argument controls if we want to flash all the leds for a moment
resetLaunchpad :: Bool -> IO ()
resetLaunchpad b = do
  putStrLn "reset launchpad"
  
--  wait
--  fake
  wait
  sendMsg resetMsg       -- reset
  wait
--  fake
--  wait
  when b $ do
    putStrLn "flashing all leds"  
    sendMsg (turnOnAll Low)
    threadDelay (100*1000)
    sendMsg resetMsg           -- reset
    threadDelay (200*1000)
--    fake 
--    wait
  where
    fake = sendMsg (turnOff1 (Pad 0 0))
    wait = threadDelay 5000     -- it seems that Launchpad needs some time after a reset

--------------------------------------------------------------------------------

-- Launchpad is stupid... see http://linuxaudio.org/mailarchive/lau/2012/7/12/191303
-- this causes all kinds of problems


-- some serious hacking here
-- from http://grrrue.midimidimidi.org/launchpad/reversepad/index.php   
handShake :: IO ()
handShake = do
  wait
  sendMsg [Reset]     -- midi reset?
  wait
{-
  sendMsg
    [ cc 16 1
    , cc 17 103
    , cc 18 29
    , cc 19 83
    , cc 20 23
    ]
  wait
-}
  where
    wait = threadDelay 5000   
    

--------------------------------------------------------------------------------
-- * button presses

-- | A button is pressed or released
data ButtonPress 
  = Press   !Button
  | Release !Button
  deriving (Eq,Ord,Show)

-- | Constructor  
buttonPress :: Bool -> Button -> ButtonPress
buttonPress True  b = Press   b
buttonPress False b = Release b
  
decodeLaunchpadMessage' :: Message -> Maybe ButtonPress
decodeLaunchpadMessage' (MidiMessage chn msg) = {- trace (show msg) $ -} case msg of
  NoteOn key vel -> Just $ buttonPress (vel>0) button where
    button = case x of { 8 -> Side y ; _ -> Pad x y } 
    (y,x) = divMod key 16 
  NoteOff key vel -> Just $ buttonPress False button where                 -- hmidi translates velocity 0 to noteoff
    button = case x of { 8 -> Side y ; _ -> Pad x y } 
    (y,x) = divMod key 16 
  CC key vel -> liftM (buttonPress (vel>0)) (unmarshalControl' key) 
  _ -> Nothing
decodeLaunchpadMessage' _ = Nothing
  
-- | Unsafe decoding, may throw error
decodeLaunchpadMessage :: Message -> ButtonPress
decodeLaunchpadMessage msg = case decodeLaunchpadMessage' msg of
  Just p -> p
  Nothing -> error ("decodeLaunchpadMessage: " ++ show msg)
  
--------------------------------------------------------------------------------
-- * led update

-- | Sorted list of all Launchpad buttons (sort order is the \"rapid led update\" order)
allButtons :: [Button]
allButtons  
  =  [ Pad x y | y<-[0..7] , x<-[0..7] ]
  ++ [ Side y  | y<-[0..7] ]
  ++ [ Dir U, Dir D, Dir L, Dir R]
  ++ [ Ctrl Session, Ctrl User1, Ctrl User2, Ctrl Mixer ]

-- |    
data Grid = Grid
  { _gridMain :: Array (Int,Int) Color     -- ^ 8x8 array of the main grid
  , _gridSide :: Array Int Color           -- ^ length 8 array of the right column
  , _gridCtrl :: Array Int Color           -- ^ length 8 array of the top row
  }
  deriving (Show)

-- | Actually this is at the moment empty.
ledUpdateInit :: Messages
ledUpdateInit = [ ] 

-- | We have to exit the rapid led update mode before the next update!
-- Setting the grid coordinate mode to XY should do the trick.
ledUpdateClose :: Messages
ledUpdateClose = [ cc 0 1 ]  

-- | Untested (the grid may be trransposed??)   
rapidLedUpdateArr :: Grid -> Messages
rapidLedUpdateArr (Grid main side ctrl) = ledUpdateInit ++ msg1 ++ msg2 ++ msg3 ++ ledUpdateClose where
  msg1 = [ MidiMessage 3 (xxNoteOn p q) | (p,q) <- pairs (elems main) ]
  msg2 = [ MidiMessage 3 (xxNoteOn p q) | (p,q) <- pairs (elems side) ]
  msg3 = [ MidiMessage 3 (xxNoteOn p q) | (p,q) <- pairs (elems ctrl) ]

rapidLedUpdateList :: [(Button,Color)] -> Messages
rapidLedUpdateList stuff = ledUpdateInit ++ list ++ ledUpdateClose where

  list = [ MidiMessage 3 (xxNoteOn p q) | (p,q) <- pairs (go stuff1 allButtons) ] 
  stuff1 = sortNubMap stuff
  
  go :: [(Button,Color)] -> [Button] -> [Color]
  go [] ns = [ None | n <- ns ]
  go bcbcs@((b,c):bcs) (n:ns) = if b<=n 
    then (c   ) : go bcs   ns
    else (None) : go bcbcs ns
  go _ [] = error "rapidLedUpdate: shouldn't happen"

--------------------------------------------------------------------------------
-- * helper functions

pairs :: [a] -> [(a,a)]  
pairs (x:y:rest) = (x,y) : pairs rest
pairs []  = []
pairs [x] = []

xxNoteOn :: Color -> Color -> MidiMessage'    
xxNoteOn p q = NoteOn (f $ encodeColor p) (f $ encodeColor q) where
  f x = (x .&. 0x37) .|. 12 {- 4 -}    -- erase clear bit, set copy bit?
     
sortNubMap :: [(Button,Color)] -> [(Button,Color)]
sortNubMap = Map.toList . Map.fromList

--------------------------------------------------------------------------------
    