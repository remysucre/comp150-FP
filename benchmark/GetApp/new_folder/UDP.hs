{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Util.UDP
-- Copyright   :  (c) Einar Karttunen
-- License     :  BSD-style
--
-- Maintainer  :  ekarttun@cs.helsinki.fi
-- Stability   :  alpha
-- Portability :  network
--
-- Basic UDP connectivity. Currently inefficient - GHC 6.4 should provide
-- the primitives directly. Supports fast ops with CVS GHC.
--
-----------------------------------------------------------------------------

module HSNTP.Util.UDP (connectUDP, listenUDP,
                 recvBufFrom, sendBufTo,
                 sockAddr, newSock, sClose
                ) where

import Data.Char

import Network.BSD
import Network.Socket


#if (__GLASGOW_HASKELL__ < 603)

import Data.Word
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr

-- | Receive a buffer of bytes.
recvBufFrom :: Socket -> Ptr Word8 -> Int -> IO (Int, SockAddr)
recvBufFrom sock ptr n = do (str,len,sa) <- recvFrom sock n
                            pokeArray ptr ((map (fromIntegral . ord) str) :: [Word8])
                            return (len,sa)

-- | Send a buffer of bytes.
sendBufTo :: Socket -> Ptr Word8 -> Int -> SockAddr -> IO Int
sendBufTo sock ptr len sa = do str <- peekArray len ptr
                               sendTo sock (map (chr . fromIntegral) str) sa

#endif

-- | Connect to an UDP-port.
connectUDP :: String -> Int -> IO Socket
connectUDP host port = do sock <- socket AF_INET Datagram 0
                          addr <- getHostByName host
                          connect sock $ SockAddrInet (toEnum port) $ hostAddress addr
                          --socketToHandle sock ReadWriteMode
                          return sock


sockAddr :: String -> Int -> IO SockAddr
sockAddr host port = do ha <- getHostByName host
                        return $ SockAddrInet (toEnum port) (hostAddress ha)


newSock :: IO Socket
newSock = socket AF_INET Datagram 0

-- | Listen UDP
listenUDP :: Int -> IO Socket
listenUDP port = do sock <- socket AF_INET Datagram 0
                    bindSocket sock $ SockAddrInet (toEnum port) iNADDR_ANY
                    return sock
