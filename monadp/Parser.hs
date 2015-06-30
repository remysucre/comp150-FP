{- Project Assignment 1: Writing and using a parsing monad   -}

{- 
   Fill in the code requested in this file.                  
   Make sure your code compiles and runs using ghc or ghci   
   on homework.eecs.tufts.edu.
   Please submit using provide no later than October 8.      

   This assignment is designed to give you familiarity both
   with defining and using monads and with using a monadic
   approach to parsing, which should come in handy for your 
   class projects.
-}


module MonadParser where
import Data.Char        -- Provides isDigit and isSpace functions

{- 
   The following imports and instance declarations have to do with 
   new requirements for the Monad typeclass in GHC 7.10.  
   They are irrelevant for this exercise, but omitting them produces a 
   compiler warning, so I've included them.  If you're curious,
   feel free to send me email or stop by office hours.
-}
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

instance Functor Parser where
    fmap = liftM
instance Applicative Parser where
    pure  = return
    (<*>) = ap
instance Functor Hopefully where
    fmap = liftM
instance Applicative Hopefully where
    pure  = return
    (<*>) = ap



{-
  In class, we defined the Monad Hopefully for processing errors 
  and the Monad (State t) for threading state with type t through a computation.  
  In this assignment, you will be defining a new monad that does both at the 
  same time in support of parsing strings that match the grammar
  (If this grammar notation is confusing, let me know and I'll explain.)

   Digit ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 
   Int ::= Digit+
   Exp ::= Int | '(' Exp ')' | '+' Exp Exp | '-' Exp Exp
                             | '*' Exp Exp | '/' Exp Exp

   and representing them in a version of the Exp datatype we saw in class:
-}

data Exp = Plus  Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div   Exp Exp
         | Const Int
      deriving (Show)


{- 
   First, we define the type of our parsing monad. 
   We will reuse the Hopefully type from class to record error messages. 
   We will fix the type of the state to be String, which will represent
   the input remaining to be parsed. 
-}

data Parser a = P {runParser :: String -> (Hopefully a, String)}
{-
   Note that the above declaration for Parser a creates
   the functions:

      P         :: (String -> (Hopefully a, String)) -> Parser a 
      runParser :: Parser a -> String -> (Hopefully a, String)

   for constructing and deconstructing values of type Parser a, 
   respectively.
   If you get confused, it is often helpful to consider what 
   operations you have available at the necessary types.
-}


{-
   The Hopefully datatype lets us record whether there is an error.
-}
data Hopefully a = Ok a | Error String
 deriving Show


{- 
   Given the type of the monad, we can write the monad instance 
   declaration as well as the return and bind functions for the 
   Parser monad.
-}

instance Monad Parser where
  return = returnParser
  (>>=)  = bindParser

{- 
   ACTION: Fill in the definiton of returnParser.
   The function should lift a simple value into the Parser monad
   without changing the state. 
-}
returnParser :: a -> Parser a
returnParser a = P (\s -> (return a, s))


{- 
   ACTION: Fill in the definition of bindParser.
   If an error occurs while parsing the first expression,
   that error should be the result of the parse and 
   no further parsing should be attempted.
-}
bindParser :: Parser a -> (a -> Parser b) -> Parser b
bindParser p k = P (\s -> let (v', s') = runParser p s
                           in case v' of 
                              Ok x      -> runParser (k x) s'
                              Error msg -> (Error msg, s'))

-- 
-- ACTION problematic! the s' at the end might explode

{- To start parsing our expressions, we'll need a way to parse a digit. -}
{- 
   ACTION:  Write a definition of the function parseDigit'.
   It should return the error "Input empty" if there is no input
   and the error "Input does not start with a digit." if the first
   character in the input is not a digit.
   The functions defined in the imported module Data.Char

     isDigit :: Char -> Bool
     digitToInt :: Char -> Int

   should be helpful.
-}
parseDigit' :: Parser Int
parseDigit' = P(\input -> parseDigStr input)

parseDigStr [] = (Error "Input empty", [])
parseDigStr (d:ds) = if isDigit d then (Ok $ digitToInt d, ds) 
                                  else (Error "Input does not start with a digit.", d:ds)

{-
   Given these definitions, you should be able to run the parser parseDigit' on the input "123"
   to produce the output (Ok 1,"23"), shown below.
-}
myDigit' = runParser parseDigit' "123"
-- (Ok 1,"23")

{-
   This pattern of testing the first element of the input by a predicate (in this case isDigit), is
   very common, and so it will be useful to write a function that abstracts this ability. Specifically,
   satisfy takes as an argument a predicate and returns a parse of any character satisfying the predicate.

   ACTION: Write a definition of the satisfy function.
   It should return the error "Input empty" if there is no input
   and the error "Input does not satisfy supplied predicate." if the 
   supplied predicate does not hold of the first character in the input.
-}
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = P(\input -> parseStr input p)

parseStr [] _   = (Error "Input empty.", [])
parseStr (d:ds) p = if p d then (Ok d, ds) else (Error "Input does not satisfy predicate.", d:ds)

{-
   ACTION: Use the function satisfy to provide an alternative implementation for parseDigit.
-}
parseDigit :: Parser Char
parseDigit = satisfy isDigit

{- We should get the same result parsing "123" with parseDigit that we got parsing with parseDigit' -}
myDigit = runParser parseDigit "123"
-- (Ok '1',"23")

{-
   ACTION: Use the function satisfy to define the function parseLit that takes a character c as an
   argument and parses the first element of the input iff it matches c.
-}
parseLit :: Char -> Parser Char
parseLit c = satisfy (== c)

{- Expected output for parsing a '+' on the input "+ 1 2" -}
myLit = runParser (parseLit '+') "+ 1 2"
-- (Ok '+'," 1 2")

{- We can use parseLit to define functions for parsing the literals in our expression grammar. -}
parsePlus  = parseLit '+'
parseMinus = parseLit '-'
parseTimes = parseLit '*'
parseDiv   = parseLit '/'


{-
  Given the functions we've already defined, we can now write a function for parsing something
  in parentheses given a parser p for what is enclosed by the parens. 

  ACTION: Supply the rest of the definition for the parseParens function.  You won't need to
  open up the monad structure to write this function (hence the stub starts with do instead of P).
-}
parseParens :: Parser a -> Parser a
parseParens p = do 
  {
        parseLit '(';
        res <- p;
        parseLit ')';
        return res;
  }


{- 
   Test code for parseParens.  
   Note that this code is also testing your bind and return code
   (The first function to do so). 
-}
myParens = runParser (parseParens (parseLit '+')) "(+)"
-- (Ok '+',"")








{- The (<|>) combinator, pronounced "or", and usually written

     p1 <|> p2

   tries to parse the input according to p1.  
   If p1 succeeds, (<|>) returns the result of p1.  
   If p1 fails, (<|>) returns the result of parsing p2.

   Defining this function requires opening up the monad structure. (Why?)

   ACTION: Fill in the rest of the definition of (<|>).
 -}
(<|>) :: Parser a -> Parser a -> Parser a
(<|>) p1 p2 = P (\s -> let (v', s') = runParser p1 s
                        in case v' of
                           Ok x    -> (v', s')
                           _       -> runParser p2 s)


{- Test outputs -}
myOr1 = runParser (parseLit '+' <|> parseLit '-') "-23"
-- (Ok '-',"23")
myOr2 = runParser (parseLit '+' <|> parseLit '-') "+123"
-- (Ok '+',"123")
myOr3 = runParser (parseLit '+' <|> parseLit '-') "?123"
-- (Error "Input does not satisfy supplied predicate.","?123")








{-
  Now let's think about how to parse an integer.
  In the input, an integer is a sequence of digits.  
  We know how to parse a digit, so we need a way of converting a digit
  parser into a parser for a sequence of digits.

  The many combinator is useful for this task.

  It takes as an argument a parser p and it repeatedly invokes p until it gets 
  an error, at which point it undoes the last parse attempt and returns the 
  sequence of all valid values parsed so far.

  A consequence of this definition is that many will never return an error.

  ACTION: Fill in the rest of the definition for many.  Your first decision
  in writing this code should be to decide whether  or not you will need to 
  open the monad to write this function. 
-}

{- Need to open up monad to access state before running p and to 
   check the error condition on parsing p.
   Need to return a list.  Look at example for or above.  -}
many :: Parser a -> Parser [a]
many p = P (\s -> let (v', s') = runParser p s
                   in case v' of
                      Ok x -> 
                         let (Ok vs'', s'') = runParser (many p) s'
                          in (Ok (x: vs''), s'')
                      _    -> (Ok [], s))

{- Test code -}
myManyA = runParser (many parseDigit) "123"
-- (Ok "123","")
myManyB = runParser (many parseDigit) "-"
-- (Ok "","-")



{-
  But an integer can't be an empty sequence of digits, and many 
  doesn't report an error when given a string with no leading digits
  (see myManyB above).

  The many1 combinator returns a sequence that is guaranteed to have
  at least one element or it returns an error.
-}

many1 :: Parser a -> Parser [a]
many1 p = do
  { first <- p
  ; rest  <- many p
  ; return (first:rest)
  }

myMany1A = runParser (many1 parseDigit) "123"
-- (Ok "123","")
myMany2B = runParser (many1 parseDigit) "-"
-- (Error "Input does not satisfy supplied predicate.","-")


{- 
  Using many1, we can parse a non-empty sequence of digits, 
  but we get back a String (aka [Char] in Haskell):
-}

myDigitString = runParser (many (satisfy isDigit)) "123a"
-- (Ok "123","a")

{- 
  Haskell's (ad hoc polymorphic) read function 

    read :: Read a => String -> a

  will let us convert a String to an Int.
  But we need to convert a value with type Parser String to a 
  value of type Parser Int.
  The function pmap lifts a function f into the monad.
-}

pmap :: (a -> b) -> Parser a -> Parser b
pmap f pa = do 
   { a <- pa
   ; return (f a)
   }

{- 
  ACTION: Given the functions 
       parseDigit :: Parser Char
       many1 :: Parser a -> Parser [a]
       read  :: [Char] -> Int 
  and  pmap  :: (a -> b) -> Parser a -> Parser b
  define the function parseInt.
 -}
parseInt :: Parser Int
parseInt = pmap read $ many1 parseDigit

myInt = runParser parseInt "123s"
-- (Ok 123,"s")



{- 
   Now we have operations for parsing integers, parenthesized expressions,
   and the necessary arithmetic operators.

   We need once last thing to be able to parse our expression grammar.
   We need a way to consume white space.  In the monadic parser library
   that we'll be using later, the parser is built on top of a lexer
   that handles white space.  Since we don't want to worry about lexing
   as a separate step here, we'll just explicitly indicate where white
   space is allowed.  To that end, we need to define an operation that
   matches sequences of white space.  We can do that using the many and 
   satisfy combinators. 

   ACTION: define the parser ws that consumes an arbitrary number of spaces.
   The function
     isSpace :: Char -> Bool
   should come in handy.
-}


ws :: Parser String
ws = many $ parseLit ' '

{- Test code -}
myWhiteSpace = runParser ws "   34"
-- (Ok "   ","34")


{-  With this infrastructure, we can now define a parser for the expression language
    described above.
    ACTION: Fill in the definition of parseExp.
-}

parseArithExp :: Parser a -> (Exp -> Exp -> Exp) -> Parser Exp
parseArithExp p e = do 
       { p
       ; ws
       ; e1 <- parseExp
       ; ws
       ; e2 <- parseExp
       ; ws 
       ; return (e e1 e2)
       }

parseExp :: Parser Exp
parseExp = do {
    pmap Const parseInt <|>
    parseParens parseExp <|> 
    parseArithExp parsePlus Plus <|>
    parseArithExp parseMinus Minus <|>
    parseArithExp parseTimes Times <|> 
    parseArithExp parseDiv Div }

{- Test code -}
(Ok myExp1, residual1) = runParser parseExp "23"
-- (Ok (Const 23),"")
(Ok myExp2', residual2') = runParser parseExp "+12"
(Ok myExp2, residual2) = runParser parseExp "+ 1 2"
-- (Ok (Plus (Const 1) (Const 2)),"")
(Ok myExp3, residual3) = runParser parseExp "- (* 4 2) + 2 4"
-- (Ok (Minus (Times (Const 4) (Const 2)) (Plus (Const 2) (Const 4))),"")
(myExp4, residual4) = runParser parseExp "+ + 1"
-- (Error "Input does not satisfy supplied predicate.","+ + 1")

{- 
   Using the eval function from class, we can evaluate the 
   three good parses above:
-}

myEval1 = eval myExp1
-- Ok 23
myEval2 = eval myExp2
-- Ok 3
myEval3 = eval myExp3
-- Ok 2

{-- Monadic error-checking interpreter --}   
eval :: Exp -> Hopefully Int
eval (Plus  e1 e2) = do {
    v1 <- eval e1;
    v2 <- eval e2;
    return (v1 + v2)     }
eval (Minus e1 e2) = do {
    v1 <- eval e1;
    v2 <- eval e2;
    return (v1 - v2)    }
eval (Times e1 e2) = do {
    v1 <- eval e1;
    v2 <- eval e2;
    return (v1 * v2)     }
eval (Div   e1 e2) =  do {
    v1 <- eval e1;
    v2 <- eval e2;
    if v2 == 0 then Error "divby0" else return (v1 `div` v2)}
eval (Const i)     = return i


{-
   CONGRATULATIONS, you've finished!
-}


instance Monad Hopefully where 
  return = Ok
  (>>=)  = ifOKthen

ifOKthen :: Hopefully a -> (a -> Hopefully b) -> Hopefully b
e `ifOKthen` k = 
   case e of 
     Ok x -> k x
     Error s -> Error s
