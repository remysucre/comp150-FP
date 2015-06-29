module FindHs (findHs) where

import System.FilePath.Find
import System.Environment 
import Data.Functor
import Control.Applicative

findHs = System.FilePath.Find.find always (extension ==? ".hs") 
