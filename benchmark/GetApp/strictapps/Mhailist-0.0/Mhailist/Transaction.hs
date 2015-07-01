{-# LANGUAGE NamedFieldPuns #-}

module Mhailist.Transaction (
    updateSubscribers, subscribers
) where

import Control.Monad (liftM)
import Control.Monad.Error (ErrorT)
import Control.Monad.Trans (liftIO)
import Data.ByteString.Lazy.Char8 (pack,unpack)
import Data.List (nub)
import Data.Time.Clock.POSIX (getPOSIXTime)

import LTR.Transaction

import Mhailist.Address (Address,parseAddress)
import Mhailist.Error (liftError)
import Mhailist.List (ListAction (..))

--------------------------------------------------------------------------------

app = 42

ttype Subscribe   = 0
ttype Unsubscribe = 1
ttype _           = error "invalid transaction type"

--------------------------------------------------------------------------------

updateSubscribers :: FilePath -> ListAction -> Address -> IO ()
updateSubscribers _ SendToList _      = return ()
updateSubscribers path action address =
    do
        time <- getPOSIXTime
        appendTransaction path (trans time)
    where 
        trans time = mkTransaction (fromIntegral app)
                                   (fromIntegral $ ttype action) 
                                   (mkOriginator 0) time
                                   (pack $ show address)

subscribers :: FilePath -> ErrorT String IO [Address]
subscribers path =
    do ts <- liftIO $ getTransactions path
       let subsData  = map getData $ getSubscribed [] $! ts
           addresses = mapM (liftError . parseAddress . unpack) subsData
        in liftM nub addresses
    where
        -- XXX: check for invalid transactions
        getSubscribed subs []                                  = subs
        getSubscribed subs ((InvalidTransaction _):ts)         =
                getSubscribed subs ts
        getSubscribed subs (t@Transaction{transType}:ts)
                | transType == ttype Subscribe = getSubscribed (t:subs) ts
                | otherwise                    = getSubscribed
                                                    (removeEarlier t subs) ts
        removeEarlier _ []                                  = []
        removeEarlier unsub@(Transaction _ _ _ uTime uData _ _)
                      (t@(Transaction _ _ _ sTime sData _ _):ts)
                      | uData == sData && sTime < uTime = removeEarlier unsub ts
                      | otherwise                       =
                          t:(removeEarlier unsub ts)
        removeEarlier _ _                                   = []
        getData Transaction{transData} = transData
        getData (InvalidTransaction _) =
                error "Can't get data from invalid transaction"
