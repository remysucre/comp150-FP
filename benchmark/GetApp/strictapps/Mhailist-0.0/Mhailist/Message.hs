{-# LANGUAGE CPP #-}
module Mhailist.Message (
    Message, parseMessage, mkMessage, addHeader,
    rfc2822Format, parseAddress,
    envelopeRecipient, fromAddress,
    Address (..), (+@), (-@),
#ifdef __TEST__
    _testMessage,
#endif
) where

import Mhailist.BuildMessage
import Mhailist.Address
import Mhailist.Error

#ifdef __TEST__
import UnitTest
#endif

import Data.Char (isSpace, toLower)

--------------------------------------------------------------------------------

headers :: String -> Message -> [String]
headers name (Message headers _) =
    map value matching
    where
        matching = filter matchName headers
        matchName (Header n _) = lower name == (lower . trimWhiteSpace) n
        value (Header _ (':':v)) = trimWhiteSpace v
        value _ = error "headers: bad value"
        lower = map toLower

header :: String -> Message -> HandlesError String
header name msg = do case headers name msg of
                          []     -> throwError $ "Header " ++ show name
                                                           ++ " not found"
                          (v:[]) -> return v
                          (_:_)  -> throwError $ "Expected one header"
                                                  ++ show name
                                                  ++ ", but got several"

rfc2822Format :: Message -> String
rfc2822Format (Message headers body) =
    concat $ (map headerLine headers) ++ [body]
    where
        headerLine (Header name value) = name ++ value

--------------------------------------------------------------------------------

addressFromHeader :: Message -> String -> HandlesError Address
addressFromHeader msg hdr = "addressFromHeader: " +#
                            header hdr msg >>= parseAddress >>= return

envelopeRecipient :: Message -> HandlesError Address
envelopeRecipient msg = "envelopeRecipient: " +#
                        addressFromHeader msg "x-original-to"

fromAddress :: Message -> HandlesError Address
fromAddress msg = "fromAddress: " +# addressFromHeader msg "From"

--------------------------------------------------------------------------------

trimWhiteSpace :: String -> String
trimWhiteSpace = let t = reverse . dropWhile isSpace
                  in t.t

--------------------------------------------------------------------------------

mkMessage :: Address -> Address -> String -> String -> Message
mkMessage from to subject body =
    let headerNames  = [ "From", "To", "Subject" ]
        headerValues = map (\x -> ": " ++ x ++ "\r\n")
                           [ show from, show to, subject ]
     in Message (map (\x -> Header (fst x) (snd x))
                     (zip headerNames headerValues))
                ("\r\n" ++ body)

addHeader :: Header -> Message -> Message
addHeader header (Message headers body) = Message (header:headers) body

--------------------------------------------------------------------------------

#ifdef __TEST__
test_headers = test
    [ headers "continued" testMessage ~?= ["more \n\t   data here"]
    , headers "hahaha"    testMessage ~?= []
    ]

test_mkMessage = test
    [ mkMessage (Address "me" "here") (Address "you" "there")
                "test" "foobar" ~?=
      Message
        [ Header "From"    ": me@here\r\n"
        , Header "To"      ": you@there\r\n"
        , Header "Subject" ": test\r\n"
        ] "\r\nfoobar"
    ]

_testMessage = runTests $ "Mhailist.Message" ~: test 
    [ test_headers
    , test_mkMessage
    , envelopeRecipient testMessage ~?= Right (Address "mylist" "lists.com")
    , fromAddress testMessage       ~?= Right (Address "foo" "bar.com")
    , "trimWhiteSpace" ~: test
        [ "empty"     ~: trimWhiteSpace ""        ~?= ""
        , "none"      ~: trimWhiteSpace "foo"     ~?= "foo"
        , "start"     ~: trimWhiteSpace " foo"    ~?= "foo"
        , "end"       ~: trimWhiteSpace "foo "    ~?= "foo"
        , "start/end" ~: trimWhiteSpace " foo "   ~?= "foo"
        , "tab"       ~: trimWhiteSpace "\tfoo"   ~?= "foo"
        , "only"      ~: trimWhiteSpace " \t\n\r" ~?= ""
        , "middle"    ~: trimWhiteSpace "f oo"    ~?= "f oo"
        ]
    ]
#endif
