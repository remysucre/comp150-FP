{-# LANGUAGE CPP #-}
module Mhailist.Address (
    parseAddress, Address (..),
    (+@), (-@), isValid, email,
#ifdef __TEST__
    _testAddress,
#endif
) where

import Mhailist.Error

#ifdef __TEST__
import UnitTest
#endif

--------------------------------------------------------------------------------

data Address = Address String String -- user domain
     deriving Eq

instance Show Address where
    show (Address user domain) = user ++ "@" ++ domain

-- function to retrieve an email address if the implementation changes later
email :: Address -> String
email = show

(+@) :: [Address] -> Address -> [Address]
(+@) addresses address = addresses ++ [address]

(-@) :: [Address] -> Address -> [Address]
(-@) addresses address = filter (address /=) addresses

--------------------------------------------------------------------------------

parseAddress :: String -> HandlesError Address
parseAddress header =
    let (jlen,alen) = jalen header
        usr         = takeWhile (/= '@') $ drop jlen header
        dmn         = tail $ dropWhile (/= '@') $ take alen $ drop jlen header
        in "parseAddress: " +# isValid (Address usr dmn) >>= return

-- |From a string containing an address return the length of the
-- substring before the address and the length of the address itself.
-- XXX This does not deal with quoted double-quotes.
jalen :: String -> (Int, Int)
jalen ""        = (0,0)
jalen ('"':xs)  = jalenSkipUntil '"' xs
jalen ('(':xs)  = jalenSkipUntil ')' xs
jalen ('\'':xs) = jalenSkipUntil '\'' xs
jalen ('@':xs)  = (0, length (takeWhile isUserDomainChar xs) + 1)
jalen (x:xs)    = let (jlen, alen) = jalen xs
                   in jalenNew x jlen alen

jalenNew :: Char -> Int -> Int -> (Int, Int)
jalenNew x 0 alen | isUserDomainChar x = (0, alen + 1)
                  | otherwise          = (1, alen)
jalenNew _ jlen alen                   = (jlen + 1, alen)

jalenSkipUntil :: Char -> String -> (Int, Int)
jalenSkipUntil x xs = let (junk, rest) = span (/= x) xs
                          (jlen, alen) = jalen $ tail rest
                       in (jlen + (length junk) + 2, alen)

isUserDomainChar :: Char -> Bool
isUserDomainChar x = if x `elem` "@'()<>/\\\" \t\n\r"
                  then False
                  else True

--------------------------------------------------------------------------------

isValid         :: Address -> HandlesError Address
isValid (Address "" _) = throwError "Invalid email address: empty user"
isValid (Address _ "") = throwError "Invalid email address: empty domain"
isValid addr@(Address user domain) =
    if (any (any (not . isUserDomainChar))) [user, domain]
      then throwError $ "Invalid email address: \"" ++ show addr ++ "\""
      else return addr

--------------------------------------------------------------------------------

#ifdef __TEST__
_testAddresses = [ "foo@bar.com"
                 , "<foo@bar.com>"
                 , "foo <foo@bar.com>"
                 , "foo bar <foo@bar.com>"
                 , "\"foo bar\" <foo@bar.com>"
                 , "\"foo bar\" foo@bar.com"
                 , "foo@bar.com (foo bar)"
                 , "\"foo bar \"<foo@bar.com>"
                 , "\"foo@baz.com\" <foo@bar.com>"
                 , "'foo@baz.com' <foo@bar.com>"
                 , "(foo@baz.com) <foo@bar.com>"
                 ]

test_parseAddress = test $ map testParse _testAddresses
    where testParse x = parseAddress x ~?= Right (Address "foo" "bar.com")

test_operators = test
    [ [] +@ (Address "foo" "bar")                     
            ~?= [(Address "foo" "bar")]
    , [(Address "bar" "foo")] +@ (Address "foo" "bar")
            ~?= [(Address "bar" "foo"), (Address "foo" "bar")]
    , [(Address "foo" "bar")] +@ (Address "foo" "bar")
            ~?= [(Address "foo" "bar"), (Address "foo" "bar")]
    , [] -@ (Address "foo" "bar")
            ~?= []
    , [(Address "foo" "bar")] -@ (Address "foo" "bar")
            ~?= []
    , [(Address "foo" "bar")] -@ (Address "bar" "foo")
            ~?= [(Address "foo" "bar")]
    , [(Address "foo" "bar"), (Address "bar" "foo")] -@ (Address "bar" "foo")
            ~?= [(Address "foo" "bar")]
    ]

_testAddress = runTests $ "Mhailist.Address" ~: test 
    [ test_parseAddress
    , test_operators
    , "isValid" ~: test
        [ "empty" ~: isValid (Address "" "")
                ~?= Left "Invalid email address: empty user"
        , "/"     ~: isValid (Address "foo/" "bar")
                ~?= Left "Invalid email address: \"foo/@bar\""
        , "\\"    ~: isValid (Address "foo\\" "bar")
                ~?= Left "Invalid email address: \"foo\\@bar\""
        , "\""    ~: isValid (Address "\"foo" "bar\"")
                ~?= Left "Invalid email address: \"\"foo@bar\"\""
        , "'"     ~: isValid (Address "foo" "bar'")
                ~?= Left "Invalid email address: \"foo@bar'\""
        , "()"    ~: isValid (Address "(foo" "bar)")
                ~?= Left "Invalid email address: \"(foo@bar)\""
        , "@"     ~: isValid (Address "foo@" "bar")
                ~?= Left "Invalid email address: \"foo@@bar\""
        , "<>"    ~: isValid (Address "<foo" "bar>")
                ~?= Left "Invalid email address: \"<foo@bar>\""
        , "valid" ~: isValid (Address "foo" "bar")
                ~?= Right (Address "foo" "bar")
        ]
    ]
#endif
