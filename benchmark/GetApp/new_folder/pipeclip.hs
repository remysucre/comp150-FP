{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack, unpack)
import Data.List
import Paths_pipeclip
import Safe
import System.Hclip
import System.Environment
import System.IO
import Text.Editor

#if !(MIN_VERSION_base (4,8,0))
import Data.Monoid
#endif

data Args =
  Args {help :: Bool
       ,license :: Bool
       ,toStdout :: Bool
       ,template :: String}

main :: IO ()
main =
  do stdargs <- getArgs
     runArgs (Args (or ["--help" `elem` stdargs,"-h" `elem` stdargs])
                   ("--license" `elem` stdargs)
                   ("--stdout" `elem` stdargs)
                   (case findIndex (\x ->
                                      (x == "--template") ||
                                      (x == "-t"))
                                   stdargs of
                      Just i ->
                        at stdargs (i + 1)
                      Nothing -> "txt"))

runArgs :: Args -> IO ()
runArgs (Args help_ license_ sto templ) =
  if |  license_ ->
       do licensePath <-
            getDataFileName "LICENSE"
          licenseBytes <- B.readFile licensePath
          B.hPut stdout licenseBytes
     |  not help_ -> runClip sto templ
     |  otherwise ->
       B.hPut stdout
              (pack (mappend (unlines ["pipeclip v.0.1.0.1"
                                      ,"Copyright (c) 2015, Peter Harpending."
                                      ,"Licensed under the FreeBSD license. See --license for info."
                                      ,mempty
                                      ,"OPTIONS"])
                             (unlines (fmap (mappend (replicate 4 ' '))
                                            ["-h,--help             Show this page"
                                            ,"--license             Print out the license."
                                            ,"--stdout              Print output to stdout instead of piping to the clipboard."
                                            ,"-t,--template EXT     Name the temporary file the editor edits tmp.EXT so that "
                                            ,"                      it has highlighting and stuff."]))))

runClip :: Bool -> String -> IO ()
runClip sto templ =
  do bs <-
       runUserEditorDWIM (mkTemplate templ)
                         "Edit this here text."
     if sto
        then B.hPut stdout bs
        else setClipboard (unpack bs)
