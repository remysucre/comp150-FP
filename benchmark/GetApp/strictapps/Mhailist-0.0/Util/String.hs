{-# LANGUAGE CPP #-}

module Util.String (
    split,
#ifdef __TEST__
    _testString,
#endif
) where

#ifdef __TEST__
import UnitTest
#endif

#if __GLASGOW_HASKELL__ <= 608
import Data.String
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy.Char8 as LB
#endif

----------------------------------------------------------------------
-- String Overloading

-- You must add add the following pragma to files that use this:
-- {-# LANGUAGE OverloadedStrings #-}

#if __GLASGOW_HASKELL__ <= 608
instance IsString SB.ByteString where fromString = SB.pack
instance IsString LB.ByteString where fromString = LB.pack
#endif

----------------------------------------------------------------------

-- | Same as 'Data.ByteString.split'.
--
split :: (Eq a) => a -> [a] -> [[a]]
split c s  = reverse $ split' c s [[]]
    where
        split' _ [] (p:ps) = (reverse p:ps)
        split' c (s:ss) (p:ps)
            | c == s      = split' c ss ([]:reverse p:ps)
            | otherwise   = split' c ss ((s:p):ps)
        split' _ _ [] = error "Util.String.split: can't arrive here."

----------------------------------------------------------------------

#ifdef __TEST__
_testString = runTests $ "Util.String" ~: test
    [ split ',' ""              ~?=  [""]
    , split ',' "ab"            ~?=  ["ab"]
    , split ',' "ab,cd,ef"      ~?=  ["ab","cd","ef"]
    , split ',' ",ab,"          ~?=  ["","ab", ""]
    ]
#endif
