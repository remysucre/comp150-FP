{-# LANGUAGE CPP, NamedFieldPuns #-}

module Mhailist.List (
    List (..), ListAction (..), mkList, listAddress, listID,
#ifdef __TEST__
    _testList,
#endif
) where

import Mhailist.Message (Message, Address (..), envelopeRecipient)
import Mhailist.Error

#ifdef __TEST__
import UnitTest
#endif

import System.FilePath (combine)

----------------------------------------------------------------------

data List = List
    { listName          :: String
    , listDomain        :: String
    , listAddrFile      :: FilePath
    } deriving (Show, Eq)

data ListAction
    = SendToList
    | Subscribe
    | Unsubscribe
    deriving (Show, Eq)

mkList :: FilePath -> Message -> HandlesError (List,ListAction)
mkList listFileDir message = "mkList: " +#
    do recipient     <- envelopeRecipient message
       listAndAction <- mkListFromAddr listFileDir recipient
       return listAndAction

mkListFromAddr :: FilePath -> Address -> HandlesError (List,ListAction)
mkListFromAddr listFileDir (Address user domain) = "mkListFromAddr: " +#
   do let (name,action) = getAction user
      return (List name domain
                   (combine listFileDir $ concat [name, '@':domain])
             ,action)

----------------------------------------------------------------------

getAction :: String -> (String,ListAction)
getAction (x:"-subscribe")   = (x:[], Subscribe)
getAction (x:"-unsubscribe") = (x:[], Unsubscribe)
getAction (x:xs)             = let (id, action) = getAction xs
                               in (x:id, action)
getAction []                 = ([], SendToList)

----------------------------------------------------------------------

listAddress :: List -> Address
listAddress list = Address (listName list) (listDomain list)

listID :: List -> String
listID List{listName,listDomain} = concat [listName, "@", listDomain]

----------------------------------------------------------------------

#ifdef __TEST__
testList = List "f" "b" (combine "/x" "f@b")

_testList = runTests $ "Mhailist.List" ~: test 
    [ "getAction empty"     ~: getAction ""                 ~?= ("", SendToList)
    , "getAction no action" ~: getAction "foo"              ~?= ("foo", SendToList)
    , "getAction subscribe" ~: getAction "foo-subscribe"    ~?= ("foo", Subscribe)
    , "getAction unsub"     ~: getAction "foo-unsubscribe"  ~?= ("foo", Unsubscribe)
    , "mkList list"         ~: mkListFromAddr "/x" (Address "f" "b")
                    ~?= Right (testList, SendToList)
    , "mkList action sub"   ~: mkListFromAddr "/x" (Address "f-subscribe" "b")
                    ~?= Right (testList, Subscribe)
    , "mkList action unsub" ~: mkListFromAddr "/x" (Address "f-unsubscribe" "b")
                    ~?= Right (testList, Unsubscribe)
    , "listAddress"         ~: listAddress testList        ~?= Address "f" "b"
   ]
#endif
