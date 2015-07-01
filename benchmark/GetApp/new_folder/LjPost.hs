{-# LANGUAGE CPP #-}
-- vim: set fileencoding=utf-8 ts=2 sw=2 expandtab:

module LjPost (readLjSetting, postToLj, isSuccess, lookupLjKey, putLjKey) where

import IO
import Maybe (fromJust, fromMaybe, isNothing)
import Data.Time
import System.Locale (defaultTimeLocale)
import System.Directory (getHomeDirectory)

#ifdef NANOMD5
import Data.ByteString.UTF8 (fromString)
import Data.Digest.OpenSSL.MD5 (md5sum)
#else
-- PUREMD5
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.MD5 (md5)
#endif

import Network.Curl

ljFlatUrl = "www.livejournal.com/interface/flat"
ljPassFile = "~/.ljpass"

currentTime = do
  t <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ utcToLocalTime tz t

showTime = formatTime defaultTimeLocale

-- options for various requests,
-- see http://www.livejournal.com/doc/server/ljp.csp.protocol.html

postFlags = [CurlPost True]

quoteOpt ('=':xs) = "%3d" ++ quoteOpt xs
quoteOpt ('&':xs) = "%26" ++ quoteOpt xs
quoteOpt (x:xs) = x : quoteOpt xs
quoteOpt [] = []

getChallengeOpts = CurlPostFields ["mode=getchallenge"] : postFlags

authOpts u p c = [ "user=" ++ quoteOpt u, "auth_method=challenge",
  "auth_challenge=" ++ quoteOpt c,
  "auth_response=" ++ quoteOpt (evalResponse c p) ]

loginAuthOpts u p c =
  CurlPostFields ("mode=login" : (authOpts u p c)) : postFlags

currentTimeOpts :: IO [String]
currentTimeOpts = do
  t <- currentTime
  let opts = [ "year=%Y", "mon=%m", "day=%d", "hour=%H", "min=%M" ]
  return $ map (flip showTime t) opts

postOpts u p c subj msg topts =
  CurlPostFields ("mode=postevent" : (authOpts u p c)
                  ++ ["event=" ++ quoteOpt msg, "subject=" ++ quoteOpt subj,
                     "lineendings=unix", "ver=1"]
                  ++ topts ) : postFlags

-- make an associative list
list2alist :: [a] -> [(a,a)]
list2alist (k:v:rest) = (k,v) : list2alist rest
list2alist _ = []

-- lookup key in flat LJ response
lookupLjKey :: String -> CurlResponse -> Maybe String
lookupLjKey k = ( lookup k . list2alist . lines . respBody )

-- print key k from flat LJ response
putLjKey k r = putStrLn $ show $ lookupLjKey k r

-- evaluate challenge response
evalResponse c p = smd5 ( c ++ (smd5 p) )
#ifdef NANOMD5
  where smd5 = md5sum . fromString
#else
  -- PUREMD5
  where smd5 = show . md5 . fromString
#endif

-- does LJ report success?
isSuccess :: CurlResponse -> Bool
isSuccess = (=="OK") . fromMaybe "" . lookupLjKey "success"

-- read and parse a file with LJ password and username
readPassFile f = do
  ljpass <- readFile f
  return $ map (\(f,s) -> (f,tail s)) $ map (break (== '=')) $ lines ljpass

expandhome ('~':'/':p) = do h <- getHomeDirectory ; return (h ++ "/" ++ p)
expandhome p = return p

readLjSetting key = do
  passfile <- expandhome ljPassFile
  s <- readPassFile passfile
  return (lookup key s)

-- login into LJ as ljuser/ljpass and post msg with given subj
postToLj ljuser ljpass subj msg = withCurlDo $ do
  curl <- initialize
  r <- do_curl_ curl ljFlatUrl getChallengeOpts :: IO CurlResponse
  if (isSuccess r)
    then do
      let challenge = fromJust $ lookupLjKey "challenge" r
      timeopts <- currentTimeOpts
      let opts = postOpts ljuser ljpass challenge subj msg timeopts
      r <- do_curl_ curl ljFlatUrl opts :: IO CurlResponse
      return r
    else return r
