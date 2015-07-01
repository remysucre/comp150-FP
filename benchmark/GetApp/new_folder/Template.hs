{-# LANGUAGE UnicodeSyntax, CPP #-}
{-# OPTIONS_GHC -pgmP cpp #-}
-- | Usage: Hit /q/ to abort the match
module Main where

import SoccerFun.UI.GL
import Control.Monad
import System.Exit
import Prelude.Unicode
import Data.Binary
import SoccerFun.MatchGame
import SoccerFun.Tape
import qualified TEAM1.Team as Team1
import qualified TEAM2.Team as Team2

#define MKSTRING(x) #x
#define TOSTRING(x) MKSTRING(x)

main ∷ IO ()
main = do
	initialise
	match ← setupMatch Team1.team Team2.team
	playTape $ recordMatch match
