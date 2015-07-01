{-# LANGUAGE CPP #-}
{-
 Copyright (c) 2005 Jesper Louis Andersen <jlouis@mongers.org>
                    Lemmih <lemmih@gmail.com>

 Permission to use, copy, modify, and distribute this software for any
 purpose with or without fee is hereby granted, provided that the above
 copyright notice and this permission notice appear in all copies.

 THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  BEncode.BEncode
-- Copyright   :  (c) Jesper Louis Andersen, 2005. (c) Lemmih, 2005-2006
-- License     :  BSD-style
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  believed to be stable
-- Portability :  portable
--
-- Provides a BEncode data type is well as functions for converting this
-- data type to and from a String.
--
-- Also supplies a number of properties which the module must satisfy.
-----------------------------------------------------------------------------
module BEncode.BEncode
  (
   -- * Data types
   BEncode(..),
   -- * Functions
   bRead,
   bShow,
  )
where

import qualified Data.Map as Map
import Data.Map (Map)
import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Char8 as BS

import Data.ByteString (ByteString)

import BEncode.BLexer ( Token (..), lexer )

#ifdef __CABAL_TEST__
import Test.QuickCheck
import Control.Monad
#endif


type BParser a = GenParser Token () a

{- | The B-coding defines an abstract syntax tree given as a simple
     data type here
-}
data BEncode = BInt Int
             | BString ByteString
             | BList [BEncode]
             | BDict (Map String BEncode)
               deriving (Eq, Ord, Show)

-- Source position is pretty useless in BEncoded data.
updatePos :: t -> t1 -> t2 -> t
updatePos pos _ _ = pos

bToken :: Token -> BParser ()
bToken t = tokenPrim show updatePos fn
    where fn t' | t' == t = Just ()
          fn _ = Nothing

token' :: (Token -> Maybe a) -> BParser a
token' = tokenPrim show updatePos

tnumber :: BParser Int
tnumber = token' fn
    where fn (TNumber i) = Just i
          fn _ = Nothing

tstring :: BParser ByteString
tstring = token' fn
    where fn (TString str) = Just str
          fn _ = Nothing

withToken :: Token -> BParser a -> BParser a
withToken tok
    = between (bToken tok) (bToken TEnd)

--------------------------------------------------------------
--------------------------------------------------------------

bInt :: BParser BEncode
bInt = withToken TInt $ fmap BInt tnumber

bString :: BParser BEncode
bString = fmap BString tstring

bList :: BParser BEncode
bList = withToken TList $ fmap BList (many bParse)

bDict :: BParser BEncode
bDict = withToken TDict $ fmap (BDict . Map.fromAscList) (many1 bAssocList)
    where bAssocList
              = do str <- tstring
                   value <- bParse
                   return (BS.unpack str,value)

bParse :: BParser BEncode
bParse = bDict <|> bList <|> bString <|> bInt

-- | bRead is a conversion routine. It assumes a B-coded string as input
--   and attempts a parse of it into a BEncode data type
bRead :: ByteString -> Maybe BEncode
bRead str = case parse bParse "" (lexer str) of
             Left _err -> Nothing
             Right b   -> Just b

-- | Render a BEncode structure to a B-coded string
bShow :: BEncode -> ShowS
bShow be = bShow' be
  where
    sc = showChar
    ss = showString
    sKV (k,v) = sString k (length k) . bShow' v
    sDict dict = foldr (.) id (map sKV (Map.toAscList dict))
    sList list = foldr (.) id (map bShow' list)
    sString str len = shows len . sc ':' . ss str
    bShow' b =
      case b of
        BInt i    -> sc 'i' . shows i . sc 'e'
        BString s -> sString (BS.unpack s) (BS.length s)
        BList bl  -> sc 'l' . sList bl . sc 'e'
        BDict bd  -> sc 'd' . sDict bd . sc 'e'











--------------------------------------------------------------
-- Tests
--------------------------------------------------------------
#ifdef __CABAL_TEST__

arbitraryBStr :: Int -> Gen ByteString
arbitraryBStr len
    = fmap BS.pack (replicateM len (elements ['\0' .. '\255']))

instance Arbitrary BS.ByteString where
    arbitrary = sized $ \n -> arbitraryBStr =<< choose (1,n)
    coarbitrary = undefined

instance Arbitrary Char where
  arbitrary = elements "abcdefghijklmopqstuvwzyx"
  coarbitrary = undefined

instance (Arbitrary k, Ord k
         ,Arbitrary a ) => Arbitrary (Map.Map k a) where
    arbitrary = sized $ \n ->
                do x <- arbitrary
                   xs <- resize (n `div` 2) arbitrary
                   return (Map.fromList (x:xs))
    coarbitrary = undefined

instance Arbitrary BEncode where
    coarbitrary = undefined
    arbitrary = sized $ \n ->
                oneof
                [ liftM BInt arbitrary
                , liftM BString arbitrary
                , liftM BList (resize (n `div` 2) arbitrary)
                , liftM BDict arbitrary ]

_prop_identity :: BEncode -> Bool
_prop_identity bencode
    = maybe False (== bencode) $ bRead (BS.pack (bShow bencode ""))

#endif
