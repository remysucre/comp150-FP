{- The Eden Trace Viewer (or simply EdenTV) is a tool that can generate diagrams
   to visualize the behaviour of Eden programs.
   Copyright (C) 2005-2012  Philipps Universitaet Marburg

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

-}

{-# LANGUAGE CPP #-}
-- JB: adapted instances (see end of file)
module EdenTvType where

import Data.Tree
import Data.Word
import qualified Data.Map as M
import qualified Data.Sequence as S
--import DeepSeq
import GHC.RTS.Events (ThreadStopStatus(..))
import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.UI.Gtk.Gdk.GC



-- A State to carry around:
-- What has to be drawn and how?
data ViewerState = VS
        { selRow  :: [Double]   -- currently selected row, [] if none
        , selView :: Int      -- machine- (0), process- (1) or threadView (2)
        , locTime :: Bool     -- all machines starting simultaneously
        , showMsg :: Bool     -- draw messages?
        , matrixM :: [Double] -- \ :-) 
        , matrixP :: [Double] -- these lists control sorting in diffrent views
        , matrixT :: [Double] -- / 
        , matrixGP :: [Double]
        , ommitRedraw :: Bool

        , clicked :: Bool
        , deleteSel :: Bool
        , noDND :: Bool
        , oldView :: Maybe Pixbuf

        , confMachines :: [(MachineID, (Bool,Bool))] -- show in/out Messages Machines
        , confProcesses :: [(ProcessID, (Bool,Bool))] -- show in/out Messages Procs

        , autoTicks :: Bool
        , tickSkip :: Seconds
        , tickMark :: Int

        , filename :: String        -- the trace file the data was read from
        , ignoreMessages :: Bool    -- indicates if the trace file was parsed without messages
        } deriving Show

instance Show Pixbuf where
         show x = "pixbuf"

data EdenTvState = ES
        { lastPath :: String
        , colors :: Colors
        } deriving Show

data ColorRGBA = RGBA
                { rgbColor :: Color
                , alpha :: Word16
                } deriving Show

data Colors = Colors
                { statusRunning :: ColorRGBA
                , statusSuspended :: ColorRGBA
                , statusBlocked :: ColorRGBA
                , statusIdle :: ColorRGBA

                , messagesSystem :: ColorRGBA
                , messagesHead :: ColorRGBA
                , messagesData :: ColorRGBA
                , messagesHeadLocal :: ColorRGBA
                , messagesDataLocal :: ColorRGBA
                , messagesBlock :: ColorRGBA
                , messagesReceive :: ColorRGBA

                , markerLine :: ColorRGBA
                , markerLabel :: ColorRGBA
                , markerStartup :: ColorRGBA

                , chartBackground :: ColorRGBA
                , chartAxes :: ColorRGBA
                , chartAxesLabel :: ColorRGBA
                } deriving Show

rgba :: Word16 -> Word16 -> Word16 -> Word16 -> ColorRGBA
rgba r g b a = RGBA {rgbColor = (Color r g b), alpha = a}

rgb :: Word16 -> Word16 -> Word16 -> ColorRGBA
rgb r g b = rgba r g b 65535

type Seconds   = Double
type InportID  = Int
type OutportID = Int
class EdenEvent e where
        getEventTime :: e -> Seconds
        setEventTime :: e -> Seconds -> e

-- A machine consists of an id and a list of related events
type MachineID = Int
--type Machine = (MachineID,Int,Int,(Int,Int,Int),[MachineEvent])
--             (mID,    allP,blkP,(#Proc,#sent,#rcv),events)
data Machine = Machine {
    getIdM            :: {-# UNPACK #-} !MachineID,
    aliveProcesses    :: {-# UNPACK #-} !Int,
    runningProcesses  :: {-# UNPACK #-} !Int,
    blockedProcesses  :: {-# UNPACK #-} !Int,
    totalProcesses    :: {-# UNPACK #-} !Int,
    sentMessagesM     :: {-# UNPACK #-} !Int,
    receivedMessagesM :: {-# UNPACK #-} !Int,
    eventlistM        :: [MachineEvent]
}
newMachine :: MachineID -> Machine
newMachine mId = Machine mId 0 0 0 0 0 0 []

data MachineEvent
        = StartMachine     {-# UNPACK #-} !Seconds                     -- Event 137
        | EndMachine       {-# UNPACK #-} !Seconds                     -- Event 145
        | GCMachine        {-# UNPACK #-} !Seconds {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int -- Event 849 (Garbage Collection)
        | IdleMachine      {-# UNPACK #-} !Seconds -- virtual event (no processes available)
        | RunningMachine   {-# UNPACK #-} !Seconds -- virtual event (process running)
        | SuspendedMachine {-# UNPACK #-} !Seconds -- virtual event (no process running but runnable waiting)
        | BlockedMachine   {-# UNPACK #-} !Seconds -- virtual event (all processes blocked)
        | MNewProcess      {-# UNPACK #-} !Seconds -- other virtual events triggert by ProcessEvents
        | MKillRProcess    {-# UNPACK #-} !Seconds
        | MKillSProcess    {-# UNPACK #-} !Seconds
        | MKillBProcess    {-# UNPACK #-} !Seconds
        | MRunProcess      {-# UNPACK #-} !Seconds
        | MSuspendProcess  {-# UNPACK #-} !Seconds
        | MBlockProcess    {-# UNPACK #-} !Seconds
	| MDeblockProcess  {-# UNPACK #-} !Seconds
        | MIdleProcess     {-# UNPACK #-} !Seconds
        deriving (Show,Eq)
instance EdenEvent MachineEvent where
        getEventTime (StartMachine s)      = s
        getEventTime (EndMachine s)        = s
        getEventTime (GCMachine s _ _ _ _) = s
        getEventTime (IdleMachine s)       = s
        getEventTime (RunningMachine s)    = s
        getEventTime (SuspendedMachine s)  = s
        getEventTime (BlockedMachine s)    = s
        getEventTime (MNewProcess s)       = s
        getEventTime (MKillBProcess s)     = s
        getEventTime (MRunProcess s)       = s
        getEventTime (MSuspendProcess s)   = s
        getEventTime (MBlockProcess s)     = s
        getEventTime (MIdleProcess s)      = s
        getEventTime (MKillSProcess s)     = s
        getEventTime (MKillRProcess s)     = s
        --getEventTime e                     = error (show e)

        setEventTime (StartMachine _)      s = StartMachine s
        setEventTime (EndMachine _)        s = EndMachine s
        setEventTime (GCMachine _ g a c l) s = GCMachine s g a c l
        setEventTime (IdleMachine _)       s = IdleMachine s
        setEventTime (RunningMachine _)    s = RunningMachine s
        setEventTime (SuspendedMachine _)  s = SuspendedMachine s
        setEventTime (BlockedMachine _)    s = BlockedMachine s
        setEventTime (MNewProcess _)       s = MNewProcess s
        setEventTime (MKillBProcess _)     s = MKillBProcess s
        setEventTime (MRunProcess _)       s = MRunProcess s
        setEventTime (MSuspendProcess _)   s = MSuspendProcess s
        setEventTime (MBlockProcess _)     s = MBlockProcess s
        setEventTime (MIdleProcess _)      s = MIdleProcess s
{-instance DeepSeq MachineEvent where
        deepSeq (StartMachine sec) y = deepSeq sec y
        deepSeq (EndMachine   sec) y = deepSeq sec y
-}

-- A process is identified by its own id and the machine's id
-- it is running on, completed with it's events
--type ProcessID = (MachineID,Int)         -- (MachineID,ProcessID)
data ProcessID = UserProcess { 
                    pId2mId :: {-# UNPACK #-} !MachineID, 
                    pId ::     {-# UNPACK #-} !Int
                 } |
                 System {
                    pId2mId :: {-# UNPACK #-} !MachineID
                 } deriving (Eq, Ord)

instance Show ProcessID where
    show (UserProcess m p) = (show m) ++ ':':(show p)
    show (System m)        = (show m) ++ ":Sys"

isSystemProcess :: ProcessID -> Bool
isSystemProcess (System {}) = True
isSystemProcess _           = False

--type Process   = (ProcessID,Int,Int,(Int,Int,Int),[ProcessEvent])
--             (pID,allT alive,blockedT,(#Threads total,#sent,#rec),events)
data Process = Process {
    getIdP            :: !ProcessID,
    aliveThreads      :: {-# UNPACK #-} !Int,
    runningThreads    :: {-# UNPACK #-} !Int,
    blockedThreads    :: {-# UNPACK #-} !Int,
    totalThreads      :: {-# UNPACK #-} !Int,
    sentMessagesP     :: {-# UNPACK #-} !Int,
    receivedMessagesP :: {-# UNPACK #-} !Int,
    eventlistP        :: ![ProcessEvent]
}

newProcess :: ProcessID -> Process
newProcess pid = Process pid 0 0 0 0 0 0 []

getMIdFromP :: Process -> MachineID
getMIdFromP = pId2mId . getIdP

data ProcessEvent
        = NewProcess       {-# UNPACK #-} !Seconds                      -- Event 153
        | LabelProcess     {-# UNPACK #-} !Seconds String
        | KillProcess      {-# UNPACK #-} !Seconds (Int,Int,Int)        -- Event 161
        | GCProcess        {-# UNPACK #-} !Seconds !Int !Int !Int !Int
        | IdleProcess      {-# UNPACK #-} !Seconds -- virtual event (no threads available)
        | RunningProcess   {-# UNPACK #-} !Seconds -- virtual event (thread running)
        | SuspendedProcess {-# UNPACK #-} !Seconds -- virtual event (no thread running but runnable waiting)
        | BlockedProcess   {-# UNPACK #-} !Seconds -- virtual event (all threads blocked)
        | PNewThread       {-# UNPACK #-} !Seconds
        | PKillRThread     {-# UNPACK #-} !Seconds
        | PKillSThread     {-# UNPACK #-} !Seconds
        | PKillBThread     {-# UNPACK #-} !Seconds
        | PRunThread       {-# UNPACK #-} !Seconds
        | PSuspendThread   {-# UNPACK #-} !Seconds
        | PBlockThread     {-# UNPACK #-} !Seconds
        | PDeblockThread   {-# UNPACK #-} !Seconds
        deriving (Show,Eq)
instance EdenEvent ProcessEvent where
        getEventTime (NewProcess s)        = s
        getEventTime (LabelProcess s _)    = s
        getEventTime (KillProcess s _)     = s
        getEventTime (GCProcess s _ _ _ _) = s
        getEventTime (IdleProcess s)       = s
        getEventTime (RunningProcess s)    = s
        getEventTime (SuspendedProcess s)  = s
        getEventTime (BlockedProcess s)    = s
        getEventTime (PNewThread s)        = s
        getEventTime (PKillRThread s)      = s
        getEventTime (PKillSThread s)      = s
        getEventTime (PKillBThread s)      = s
        getEventTime (PRunThread s)        = s
        getEventTime (PSuspendThread s)    = s
        getEventTime (PBlockThread s)      = s
        getEventTime (PDeblockThread s)    = s

        setEventTime (NewProcess _) s        = NewProcess s
        setEventTime (LabelProcess _ l) s    = LabelProcess s l
        setEventTime (KillProcess _ i) s     = KillProcess s i
        setEventTime (GCProcess _ g a c l) s = GCProcess s g a c l
        setEventTime (IdleProcess _) s       = IdleProcess s
        setEventTime (RunningProcess _) s    = RunningProcess s
        setEventTime (SuspendedProcess _) s  = SuspendedProcess s
        setEventTime (BlockedProcess _) s    = BlockedProcess s
        setEventTime (PNewThread _) s        = PNewThread s
        setEventTime (PKillRThread _) s      = PKillRThread s
        setEventTime (PKillSThread _) s      = PKillSThread s
        setEventTime (PKillBThread _) s      = PKillBThread s
        setEventTime (PRunThread _) s        = PRunThread s
        setEventTime (PSuspendThread _) s    = PSuspendThread s
        setEventTime (PBlockThread _) s      = PBlockThread s
        setEventTime (PDeblockThread _) s    = PDeblockThread s

-- Threads also have events and an identifier.
type ThreadID   = (ProcessID,Int)        -- (MachineID,ProcessID,ThreadID)
type Thread     = (ThreadID,[ThreadEvent])
type OpenThread = (MachineID,(ThreadID,ThreadEvent),[Thread])
data ThreadEvent
        = NewThread     {-# UNPACK #-} !Seconds {-# UNPACK #-} !OutportID              -- Event 169
        | KillThread    {-# UNPACK #-} !Seconds                         -- Event 177
        | GCThread      {-# UNPACK #-} !Seconds {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
        | RunThread     {-# UNPACK #-} !Seconds                         -- Event 185
        | SuspendThread {-# UNPACK #-} !Seconds                         -- Event 193
        | BlockThread   {-# UNPACK #-} !Seconds {-# UNPACK #-} !InportID !BlockReason       -- Event 201
        | DeblockThread {-# UNPACK #-} !Seconds                         -- Event 209
        | DummyThread
        deriving (Show,Eq)
instance EdenEvent ThreadEvent where
        getEventTime (NewThread s _)      = s
        getEventTime (KillThread s)       = s
        getEventTime (GCThread s _ _ _ _) = s
        getEventTime (RunThread s)        = s
        getEventTime (SuspendThread s)    = s
        getEventTime (BlockThread s _ _)  = s
        getEventTime (DeblockThread s)    = s

        setEventTime (NewThread _ o)      s = NewThread s o
        setEventTime (KillThread _)       s = KillThread s
        setEventTime (GCThread _ g a c l) s = GCThread s g a c l
        setEventTime (RunThread _)        s = RunThread s
        setEventTime (SuspendThread _)    s = SuspendThread s
        setEventTime (BlockThread _ i r)  s = BlockThread s i r
        setEventTime (DeblockThread _)    s = DeblockThread s

{-instance DeepSeq ThreadEvent where
        deepSeq (NewThread     sec outPort)    y = deepSeq sec $ deepSeq outPort y
        deepSeq (KillThread    sec)            y = deepSeq sec y
        deepSeq (RunThread     sec)            y = deepSeq sec y
        deepSeq (SuspendThread sec)            y = deepSeq sec y
        deepSeq (BlockThread   sec inPort tag) y = deepSeq sec $ deepSeq inPort $ deepSeq tag y
        deepSeq (DeblockThread sec)            y = deepSeq sec y
-}

type BlockReason = GHC.RTS.Events.ThreadStopStatus

instance Eq ThreadStopStatus where
    NoStatus       == NoStatus       = True
    HeapOverflow   == HeapOverflow   = True
    StackOverflow  == StackOverflow  = True
    ThreadYielding == ThreadYielding = True
    ThreadBlocked  == ThreadBlocked  = True
    ThreadFinished == ThreadFinished = True
    ForeignCall    == ForeignCall    = True
    BlockedOnMVar  == BlockedOnMVar  = True
    BlockedOnMVarRead  == BlockedOnMVarRead  = True
    -- since GHC-7.8.2/ghc-events-0.4.3.1
    BlockedOnBlackHole == BlockedOnBlackHole = True
    BlockedOnRead  == BlockedOnRead  = True
    BlockedOnWrite == BlockedOnWrite = True
    BlockedOnDelay == BlockedOnDelay = True
    BlockedOnSTM   == BlockedOnSTM   = True
    BlockedOnDoProc == BlockedOnDoProc = True
    BlockedOnCCall == BlockedOnCCall = True
    BlockedOnCCall_NoUnblockExc == BlockedOnCCall_NoUnblockExc = True
    BlockedOnMsgThrowTo == BlockedOnMsgThrowTo = True
    ThreadMigrating == ThreadMigrating = True
    BlockedOnMsgGlobalise == BlockedOnMsgGlobalise = True
    (BlockedOnBlackHoleOwnedBy _) == (BlockedOnBlackHoleOwnedBy _) = True
    _ == _  = False 


-- Messages:
data ReasonType =
                RFork |       -- legacy code: 85
                Connect |     -- legacy code: 86 
                DataMes |     -- legacy code: 87 
                Head |        -- legacy code: 88
                Constr |      -- legacy code: 89 
                Part |        -- legacy code: 90 
                Terminate |   -- legacy code: 91
                Default |     -- legacy code: -1
                BlockReason | -- legacy code: 1
                LocalHead |   -- new: optimized message on local machine
                LocalDataMes  -- new: optimized message on local machine
        deriving (Show, Eq, Ord)

type Reason     = ReasonType
type Size       = Int

-- Map of open messages per process.
--
-- Key:         sender process id
-- Value:       all message events (send and receive) that were
--              triggered for messages sent by this process
--
type OpenMessagesPerProcess = M.Map ProcessID OpenMessages

-- Map of open messages for a process. The key used in this map (`OpenMessageKey`)
-- is not unique for a message event. It groups messages sent to the same process 
-- through the same channel (inport/outport) and with the same type and reason. 
-- These messages with the same key are organized in a sequence/queue, so that 
-- the message events can be matched in the right order.
--
type OpenMessages = M.Map OpenMessageKey (S.Seq SmallOpenMessageEvent)

type OpenMessageKey = (OpenMessageType, ProcessID, OutportID, InportID, Reason)
data OpenMessageType = TORM | TOSM deriving (Show, Eq, Ord)

-- This type is used to store an open message as value in `OpenMessages`.
-- All other information of the message is already contained in the map key.
--
data SmallOpenMessageEvent
        = SmallORM {-# UNPACK #-} !Seconds {-# UNPACK #-} !Size
        | SmallOSM {-# UNPACK #-} !Seconds
        deriving (Show,Eq)

-- Data type for open message events for messages sent from a process.
-- The process id for both ORM and OSM is always the the receiver process id.
data OpenMessageEvent
        = ORM {-# UNPACK #-} !Seconds !ProcessID {-# UNPACK #-} !OutportID {-# UNPACK #-} !InportID !Reason {-# UNPACK #-} !Size
        | OSM {-# UNPACK #-} !Seconds !ProcessID {-# UNPACK #-} !OutportID {-# UNPACK #-} !InportID !Reason
        deriving (Show,Eq)


{-instance DeepSeq OpenMessageEvent where
        deepSeq (ORM sec proc ports info) y = deepSeq sec $ deepSeq proc $ deepSeq ports $ deepSeq info y
        deepSeq (OSM sec proc ports info) y = deepSeq sec $ deepSeq proc $ deepSeq ports $ deepSeq info y
-}

-- Message: Complete message with send- and receive information
type ChannelID = (ProcessID,OutportID,ProcessID,InportID)
data Message = MSG {-# UNPACK #-} !ChannelID {-# UNPACK #-} !Seconds {-# UNPACK #-} !Seconds !Reason {-# UNPACK #-} !Size
--                 channel    stime    rtime    tag     size
        deriving (Show,Eq)
{-instance DeepSeq Message where
        deepSeq (MSG times procs ports info) y =  deepSeq times $ deepSeq procs $ deepSeq ports $ deepSeq info y
-}

-- the time it took to receive a message
type ReceiveLength = ((MachineID, [(Int,Seconds)]), Seconds, Seconds)

-- HeadMessage: Messages of type 100
type Count = Int
type DSize = Double
type OpenHeadMessage = (ChannelID,Size,Count,[Seconds],[Seconds])
--                        channel, sum of sizes, quantity, sending/receiving times
type HeadMessage = (ChannelID,(Seconds,Seconds,Seconds,Seconds),DSize,Count)
--                    channel   stimeA,rtimeA   stimeO,rtimeO    size  num
type OpenProcMessage = (MachineID,[(ProcessID,OpenMessageEvent)],[(ProcessID,OpenMessageEvent)],[ProcessID])
--                       rcvMach     sent Messages      rcvd Messages    NewProcess
type OpenMessageList = (OpenMessagesPerProcess,[Message],([OpenProcMessage],ProcessList,ProcessTree),([OpenHeadMessage],Double,[HeadMessage]))

type MessageList = (
        [Message],             --
        [Message],             -- additional messages: messages sent in a stream (bulk messages)
        [HeadMessage],         -- msgs from
        ProcessTree,
        [ReceiveLength]        -- time to receive the messages
        )

type ProcessList = [(ProcessID,[ProcessID])]
type ProcessTree  = (Tree ProcessID) -- Node ProcessID [ProcessTree])

-- (Trace-)Events:
type EventID = Int
type NewEvent = ((EventID,Seconds,[Int]),Maybe String)
-- datatype used within the calculation
type OpenEvents = (
    ([Machine], [Process], [OpenThread]),       -- list of machines/processes/threads
    [(MachineID,Double)],                       -- machine starttimes
    OpenMessageList,                            -- o/c Msgs
    (Seconds,Seconds,Int,Int))                  -- min_t   max_t    #P  maxLD

-- The main datatype for the generated list of information
-- type Events = (
--     ([Machine], [Process], [Thread]),       -- list of machines/processes/threads
--     [(MachineID, Double)],                   -- start-times per machine     
--     (Seconds, [(MachineID, Seconds)]),   -- maxStartup msgs/heads
--     MessageList,
--     (   Seconds,                            -- min_t
--         Seconds,                            -- max_t
--         Seconds,                            -- max_t_diff
--         Double,                             -- maxMsgSize
--         Double),                            -- MaxLD
--     (Int, Int, Int)                         -- number of machines/processes/threads
--     )
data Events = Events {
  machinelist        :: [Machine],
  processlist        :: [Process],
  threadlist         :: [Thread],
  starttimeByMachine :: [(MachineID, Double)],
  maxStartup         :: {-# UNPACK #-} !Seconds,
  startupOffsets     :: [(MachineID, Seconds)],
  messagelist        :: MessageList,
  min_t              :: {-# UNPACK #-} !Seconds,
  max_t              :: {-# UNPACK #-} !Seconds,
  max_t_diff         :: {-# UNPACK #-} !Seconds,
  maxMsgSize         :: {-# UNPACK #-} !Double,
  maxLD              :: {-# UNPACK #-} !Double,
  noOfMachines       :: {-# UNPACK #-} !Int,
  noOfProcesses      :: {-# UNPACK #-} !Int,
  noOfThreads        :: {-# UNPACK #-} !Int
}

#if __GLASGOW_HASKELL__ < 606
#warning __GLASGOW_HASKELL__
instance (Show a,Show b,Show c,Show d,Show e,Show f) => Show (a,b,c,d,e,f)
instance (Show a,Show b,Show c,Show d,Show e,Show f,Show g) => Show (a,b,c,d,e,f,g)
instance (Show a,Show b,Show c,Show d,Show e,Show f,Show g,Show h) => Show (a,b,c,d,e,f,g,h)
instance (Show a,Show b,Show c,Show d,Show e,Show f,Show g,Show h,Show i) => Show (a,b,c,d,e,f,g,h,i)
instance (Show a,Show b,Show c,Show d,Show e,Show f,Show g,Show h,Show i,Show j) => Show (a,b,c,d,e,f,g,h,i,j)
#endif
