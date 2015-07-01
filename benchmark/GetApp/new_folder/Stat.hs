{-# LANGUAGE CPP #-}
-- | Portably test if a given path is a file, directory, or something else.
module Stat (
    Stat(..)
  , stat
) where

import Prelude hiding (catch)
import Control.Applicative
import Control.Exception

#ifdef mingw32_HOST_OS
import Data.Bits
import qualified System.Win32 as Win32
#else
import qualified System.Posix as Posix
#endif

data Stat = File | Directory | Other

stat :: FilePath -> IO Stat
stat path = stat_ path `catch` handler
    where
        handler :: IOException -> IO Stat
        handler _ = return Other

stat_ :: FilePath -> IO Stat

#ifdef mingw32_HOST_OS
stat_ path = decode <$> Win32.getFileAttributes path
    where
        decode flags | flags .&. Win32.fILE_ATTRIBUTE_DIRECTORY /= 0 = Directory
                        -- TODO: Handle non-file non-directory files properly
                     | otherwise = File
#else
stat_ path = decode <$> Posix.getFileStatus path
    where
        decode s | Posix.isRegularFile s  = File
                 | Posix.isDirectory s    = Directory
                 | otherwise              = Other
#endif
