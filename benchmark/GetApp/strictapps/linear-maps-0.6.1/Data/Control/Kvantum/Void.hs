{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- | Data control kvantums (phony implementation)
-----------------------------------------------------------------------------
module Data.Control.Kvantum.Void
    ( K
    , create    -- IO K
    , hit       -- K -> IO ()
    , kill      -- K -> IO ()
    , renew     -- K -> IO K
    , join      -- K -> K -> IO K
    ) where

----------------------------------------------

data K = K

{-# INLINE create #-} 
create :: IO K
create = return K

{-# INLINE hit #-} 
hit :: K -> IO ()
hit !_ = return ()

{-# INLINE kill #-} 
kill :: String -> K -> IO ()
kill _ !_ = return ()

{-# INLINE renew #-} 
renew :: String -> K -> IO K
renew _ !k = return k

{-# INLINE join #-} 
join :: K -> K -> IO K
join a b = return (a `seq` b)




