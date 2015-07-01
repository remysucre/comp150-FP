{-# LANGUAGE CPP #-}
module Utils where

import Codec.Binary.UTF8.String (encodeString)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

-- for daemonTrace
import System.IO.Unsafe (unsafePerformIO)
import System.Log.Logger

-- | Safe 'head'.
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

-- | Safe substitute for (!!).
maybeNth :: Int -> [a] -> Maybe a
maybeNth _ [] = Nothing
maybeNth 0 xs = maybeHead xs
maybeNth n (_:xs) = maybeNth (n-1) xs

-- | Safe 'last'.
maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast xs = Just $ last xs

-- | Extract the first component of a triplet.
fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

-- | Extract the second component of a triplet.
snd3 :: (a,b,c) -> b
snd3 (_,y,_) = y

-- | Extract the third component of a triplet.
trd3 :: (a,b,c) -> c
trd3 (_,_,z) = z

-- | Apply function of 2 arguments to triplets' elements.
liftT2 :: (a -> b -> c) -> (a,a,a) -> (b,b,b) -> (c,c,c)
liftT2 f (x,y,z) (x',y',z') = (f x x', f y y', f z z')

-- | Encode forbidden file names with trigraphs and substitutions
fixname :: String -> String
fixname "" = "нет имени"
fixname name  = concatMap tr name
  where tr c = fromMaybe [c] $ lookup c fixnameTable

-- | Inverse of 'fixname'. Hopefully (unfixname . fixname == id).
unfixname :: String -> String
unfixname "нет имени" = ""
unfixname name = unescape name
  where unescape [] = []
        unescape s@('_':_:_:rest) =
            let esc = take 3 s
                unesc = fromMaybe esc . lookup esc $ unfixnameTable
                               -- ^^^ leave as is if name is incorrect
             in unesc ++ (unescape rest)
        unescape (x:rest) = x:(unescape rest)

-- Symbols forbidden in WinXP filenames: \ / : * ? " < > |
fixnameTable :: [(Char,String)]
fixnameTable =
  [ ( '\\', "_Z_" )
  , ( '/',  "_%_" )
  , ( ':',  "_=_" )
  , ( '*',  "_x_" )
  , ( '?',  "_7_" )
  , ( '"',  "_'_" )
  , ( '<',  "_(_" )
  , ( '>',  "_)_" )
  , ( '|',  "_I_" )
  , ( '_',  "___" ) ]

unfixnameTable :: [(String,String)]
unfixnameTable = map (sndSingleton . swap) fixnameTable
  where swap (k,v) = (v,k)
        sndSingleton (x,y) = (x,[y])

-- | Debug tracing. To be used
dbg :: String -> Bool
#ifdef DEBUGBUILD
dbg msg = daemonTrace (encodeString msg) False
#else
dbg _ = False
#endif

-- | Debug tracing.
trace' :: (Show a) => a -> a
#ifdef DEBUGBUILD
trace' x = daemonTrace (show x) x
#else
trace' = id
#endif

#ifdef DEBUGBUILD
{-# NOINLINE daemonTrace #-}
-- | daemonTrace is a replacement for Debug.Trace.trace.
daemonTrace :: String -- ^ log message
            -> a -> a
daemonTrace msg value = unsafePerformIO $ do
  debugM rootLoggerName msg
  return value
#endif

showL :: String -> [String] -> String
showL "" xs = "[" ++ (intercalate ", " xs) ++ "]"
showL name xs = name ++ " = " ++ (showL "" xs)

showL' :: [String] -> String
showL' = showL ""

stub :: anytype
stub = undefined

