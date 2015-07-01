{-# LANGUAGE CPP #-}
module Mhailist.BuildMessage (
    Message (..), Header (..), mkHeader, parseMessage,
#ifdef __TEST__
    testMessage, _testBuildMessage,
#endif
) where

import Data.List (isPrefixOf)
import Data.Char (isSpace)

#ifdef __TEST__
import UnitTest
#endif

data Message = Message [Header] String
    deriving (Show, Eq)

data Header = Header String String
    deriving (Show, Eq)

-- XXX This needs to use the line-ending convention of the message, which
-- may be \r\n instead of just \n.
--
mkHeader :: String -> String -> Header
mkHeader name value = Header (concat [name, ": "]) (concat [value, "\n"])

--------------------------------------------------------------------------------

-- parseMessage takes a string containing a message as an argument and returns
-- the message data structure, consisting of a list of header lines and the
-- body.
-- Note that no termination characters are dropped; i.e. the separator between
-- header lines (linebreak) will be added to the header it terminates, the
-- separator between header name and value (colon) is part of the header value.
parseMessage :: String -> Message
parseMessage msg = dropFrom $ pm headerLengths msg
    where
        (headerLengths,_) = hlenboff (lineBreak msg) msg
        pm [] msg = Message [] msg
        pm (hl:hls) msg =
            let Message headers body = pm hls $ drop hl msg
                (name,value) = span (\c -> c /= ':') (take hl msg)
             in Message (Header name value : headers) body
        dropFrom msg@(Message (Header name _:headers) body) =
            if isPrefixOf "From " name then Message headers body
                                            else msg
        dropFrom m = m

-- hlenboff takes the break between header lines and the header and the
-- body of the message (the break twice) as the first argument and the message
-- to process as the second argument. Returned is a pair of a list of integers
-- and an integer. The list of integers designates the lengths of the header
-- lines -- the first element of the list is the length of the first header
-- line, the second header line can be found at the offset given by the length
-- of the first header line; its length is designated by the second element in
-- the list of integers and so on. The second element of the pair denotes the
-- offset of the message body into the message.
-- The length of the header lines includes the break between them; i.e. if
-- header lines are terminated by "\r\n", the character at the end of a header
-- line is the '\n' of the "\r\n".
hlenboff  :: String -> String -> ([Int], Int) -- lens, bodyOffset
hlenboff lb msg = hlb lb msg
    where

        hlb _ [] = ([0], 0)

        hlb lb msg | (lb++lb) `isPrefixOf` msg = ([length lb], length lb)

        hlb lb msg@(_:ms) | lb `isPrefixOf` msg
                            && startsWithWhiteSpace (drop (length lb) msg) =
            let ((hl:hls),mo) = hlb lb ms
             in (hl + 1 : hls, mo + 1)

        hlb lb msg | lb `isPrefixOf` msg =
            let (hls,mo) = hlb lb (drop (length lb) msg)
             in (length lb : hls, mo + length lb)

        hlb lb (_:ms) =
            let ((hl:hls),mo) = hlb lb ms
             in (hl + 1 : hls, mo + 1)

        startsWithWhiteSpace (c:_) = isSpace c
        startsWithWhiteSpace _     = False

-- lineBreak takes a string as an argument and returns the first linebreak
-- sequence (one of "\r\n", "\n", "\r") encountered, or the empty string if the
-- input does not contain a linebreak sequence.
lineBreak :: String -> String
lineBreak ""            = ""
lineBreak ('\r':'\n':_) = "\r\n"
lineBreak ('\n':_)      = "\n"
lineBreak ('\r':_)      = "\r"
lineBreak (_:cs)        = lineBreak cs

--------------------------------------------------------------------------------

#ifdef __TEST__
testMessageInput = unlines $
    [ "From joe@example.com on somedate"
    , "X-Original-To: mylist@lists.com"
    , "From: foo@bar.com"
    , "Continued: more "
    , "\t   data here"
    , "Subject: \t Fubar\r\n"
    , "This is a message. Ha ha!\nHa ha!"
    ]

testMessage = Message
    [ Header "X-Original-To"    ": mylist@lists.com\n"
    , Header "From"             ": foo@bar.com\n"
    , Header "Continued"        ": more \n\t   data here\n"
    , Header "Subject"          ": \t Fubar\r\n"
    ] "\nThis is a message. Ha ha!\nHa ha!\n"

test_parseMessage = test
    [ parseMessage testMessageInput  ~?=  testMessage
    , parseMessage "A: foo\r\nB:bar\r\n\r\nCCC" ~?=
        Message [Header "A" ": foo\r\n", Header "B" ":bar\r\n"] "\r\nCCC"
    ]

test_hlenboff = test
    [ hlenboff lb ""        ~?= ( [0], 0 )
    , hlenboff lb "abc"     ~?= ( [3], 3 )
    , hlenboff lb "ab()c()()ef()()gh"  ~?= ( [4,3], 7 )
    ]
    where lb = "()"

test_lineBreak = test
    [ ""        ~=? lineBreak ""
    , "\n"      ~=? lineBreak "\n"
    , "\r"      ~=? lineBreak "abc\rdef\n"
    , "\n"      ~=? lineBreak "abc\ndef\r\n\n\r"
    , "\r\n"    ~=? lineBreak "abc\r\ndef\r\n"
    ]

_testBuildMessage = runTests $ "Mhailist.BuildMessage" ~: test 
    [ test_lineBreak
    , test_hlenboff
    , test_parseMessage
    ]
#endif
