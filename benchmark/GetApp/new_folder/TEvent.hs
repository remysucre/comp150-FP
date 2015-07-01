{-# LANGUAGE CPP #-}

module Control.Concurrent.STM.TEvent
  ( TEvent
  , newTEvent
  , newFiredTEvent
  , fireTEvent
  , blockTEvent

  , swapTVar
  ) where

import Control.Applicative
import Control.Concurrent.STM

-- | A signal which, when fired, notifies all observers and
-- remains fired for new observers.
newtype TEvent = TEvent (TVar (Maybe [TMVar ()]))
  deriving (Eq)
-- If 'Nothing', the event has fired.
--
-- If 'Just', contained is a list of handlers to be notified
-- when the event fires.

fromMaybe_ :: (Applicative m) => (a -> m ()) -> Maybe a -> m ()
fromMaybe_ = maybe (pure ())

-- | Creates a new, unfired 'TEvent'.
newTEvent :: STM (TEvent)
newTEvent = TEvent <$> newTVar (Just [])

-- | Creates a new, already-fired 'TEvent'.
newFiredTEvent :: STM (TEvent)
newFiredTEvent = TEvent <$> newTVar Nothing

#if !MIN_VERSION_stm(2, 3, 0)
swapTVar :: TVar a -> a -> STM a
swapTVar var x = readTVar var <* writeTVar var x
#endif

-- | Wakes up all listeners blocking (via 'blockTEvent') on
-- the event.
fireTEvent :: TEvent -> STM ()
fireTEvent (TEvent event)
  = swapTVar event Nothing >>= fromMaybe_
    (mapM_ $ \ var -> putTMVar var ())

-- | Waits for an event to be fired.
blockTEvent :: TEvent -> STM ()
blockTEvent (TEvent event)
  = readTVar event >>= fromMaybe_ (\ listeners -> do
      myListener <- newEmptyTMVar
      writeTVar event $ Just (myListener : listeners)
      takeTMVar myListener)
