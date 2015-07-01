module Verify.Graphics.Vty.Output where

import Graphics.Vty.Output.Mock

import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.String.UTF8 as UTF8

import Test.QuickCheck.Property

-- A list of terminals that should be supported.
-- This started with a list of terminals ubuntu supported. Then those terminals that really could
-- not be supported were removed. Then a few more were pruned until a reasonable looking set was
-- made.
terminalsOfInterest :: [String]
terminalsOfInterest = 
    [ "vt100"
    , "vt220"
    , "vt102"
    , "xterm-r5"
    , "xterm-xfree86"
    , "xterm-r6"
    , "xterm-256color"
    , "xterm-vt220"
    , "xterm-debian"
    , "xterm-mono"
    , "xterm-color"
    , "xterm"
    , "mach"
    , "mach-bold"
    , "mach-color"
    , "linux"
    , "ansi"
    , "hurd"
    , "Eterm"
    , "pcansi"
    , "screen-256color"
    , "screen-bce"
    , "screen-s"
    , "screen-w"
    , "screen"
    , "screen-256color-bce"
    , "sun"
    , "rxvt"
    , "rxvt-unicode"
    , "rxvt-basic"
    , "cygwin"
    ]

compareMockOutput :: MockData -> String -> IO Result
compareMockOutput mockData expectedStr = do
    outBytes <- readIORef mockData >>= return . UTF8.toRep
    let expectedBytes :: BS.ByteString = UTF8.toRep $ UTF8.fromString expectedStr
    if outBytes /=  expectedBytes
        then return $ failed { reason = "bytes\n" ++ show outBytes
                                      ++ "\nare not the expected bytes\n"
                                      ++ show expectedBytes
                             }
        else return succeeded

