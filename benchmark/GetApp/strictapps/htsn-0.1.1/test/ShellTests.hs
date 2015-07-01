module Main
where

import System.Process ( system )
import System.Exit ( exitWith )

main :: IO ()
main = do
  result <- system "shelltest test/shell/*.test"
  exitWith result
