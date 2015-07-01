  --
-----------------------------------------------------------------------------
-- | 
-- @Int@-indexed, boxed, mutable @IO@-arrays.
--
-- Reference implementation (more portable but slower):
--
-- > type Array a = Data.Array.IO.IOArray Int a
-- > 
-- > newArray i a = Data.Array.IO.newArray (0, i) a
-- > 
-- > writeArray = Data.Array.IO.writeArray
-- > 
-- > readArray = Data.Array.IO.readArray
-----------------------------------------------------------------------------

{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Data.Array.Simple
    ( Array     -- instance Eq
    , newArray
    , writeArray
    , readArray
    ) where

import GHC.Base
import GHC.ST
import GHC.IOBase

-- | @(Array a)@ is similar to @('Data.Array.IO.IOArray' 'Int' a)@, but without boundary information.

data Array a = A !(MutableArray# RealWorld a)

instance Eq (Array a) where
    A a == A b = sameMutableArray# a b

-- | @(newArray i a)@ is similar to @('Data.Array.IO.newArray' (0, i) a)@.

newArray :: Int -> a -> IO (Array a)
newArray (I# n#) a 
    = stToIO $ ST $ \s1# -> case newArray# n# a s1# of
        (# s2#, arr# #) -> (# s2#, A arr# #)

-- | @writeArray@ is similar to 'Data.Array.IO.writeArray', but without boundary check.

writeArray :: Array a -> Int -> a -> IO ()
writeArray (A arr#) (I# n#) a 
    = stToIO $ ST $ \s1# -> (# writeArray# arr# n# a s1#, () #)

-- | @readArray@ is similar to 'Data.Array.IO.readArray', but without boundary check.

readArray :: Array a -> Int -> IO a
readArray (A arr#) (I# n#) 
    = stToIO $ ST $ \s1# -> readArray# arr# n# s1#


