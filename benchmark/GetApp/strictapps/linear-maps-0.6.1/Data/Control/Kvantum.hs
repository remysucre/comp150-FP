  --
-----------------------------------------------------------------------------
-- | Data control kvantums
-----------------------------------------------------------------------------
module Data.Control.Kvantum 
    ( K
    , create    -- IO K
    , hit       -- K -> IO ()
    , kill      -- K -> IO ()
    , renew     -- K -> IO K
    , join      -- K -> K -> IO K
    ) where

import Control.Concurrent.MVar

----------------------------------------------

type K = [MVar ()]


create :: IO K
create = fmap (:[]) $ newMVar ()

hit :: K -> IO ()
hit = mapM_ f  where

    f x = do 
        y <- readMVar x
        y `seq` return y

kill :: String -> K -> IO ()
kill msg = mapM_ $ flip swapMVar $ error msg

renew :: String -> K -> IO K
renew msg k = do 
    hit k
    kill msg k
    create

join :: K -> K -> IO K
join k1 k2 = return $ k1 ++ k2


