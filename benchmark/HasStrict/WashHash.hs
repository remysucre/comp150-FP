module HasStrict.WashHash (washHash) where

import System.Environment
import Control.Applicative
import Data.Functor

washHash :: String -> String
washHash src = unlines $ filter (not . isMacro) $ lines src

isMacro :: String -> Bool
isMacro ('#':_) = True
isMacro _          = False

main = do 
    fp <- head <$> getArgs
    fc <- readFile fp
    putStr $ washHash fc
