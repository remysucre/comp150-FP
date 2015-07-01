
module Main where

--------------------------------------------------------------------------------

import System.IO

import System.MIDI.Launchpad.AppFramework

import qualified System.MIDI.Launchpad.Apps.DrumSeq   as DrumSeq
import qualified System.MIDI.Launchpad.Apps.MonoSeq   as MonoSeq
import qualified System.MIDI.Launchpad.Apps.FXControl as FXControl
import qualified System.MIDI.Launchpad.Apps.Conway    as Conway

--------------------------------------------------------------------------------

appList :: [String]
appList = 
  [ "1: Drum sequencer"
  , "2: Monomorphic sequencer"
  , "3: Effect control"
  , "4: Conway's game of life"
  ]
  
main :: IO ()
main = do
  putStrLn "\nplease select the application (default is the first one):\n"
  mapM_ putStrLn appList
  putStr "your selection: "
  hFlush stdout
  l <- getLine
  let k0 = 1
  let k = case maybeRead l of
            Nothing -> k0
            Just m  -> if m<1 || m>3 then k0 else m
  putStrLn $ "\nyou selected " ++ appList!!(k-1)
  putStrLn "\n------------------------------------------\n"
  case k of
    1 -> runMonadicApp defaultGlobalConfig $ DrumSeq.drumSequencer $ DrumSeq.defaultCfg  -- { DrumSeq.seqSteps = 16 }
    2 -> runPureApp    defaultGlobalConfig $ MonoSeq.monoSequencer $ MonoSeq.defaultCfg  -- { MonoSeq.seqSteps = 16 }
    3 -> runPureApp    defaultGlobalConfig $ FXControl.fxControl   $ FXControl.defaultCfg
    4 -> runMonadicApp defaultGlobalConfig $ Conway.conway         $ Conway.defaultCfg
    _ -> error "shouldn't happen"
      
--------------------------------------------------------------------------------
   
maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of 
  [(x,"")] -> Just x
  _        -> Nothing  
  
--------------------------------------------------------------------------------
