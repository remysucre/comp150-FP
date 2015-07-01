{-# LANGUAGE ScopedTypeVariables,CPP #-}
module DarcsDen.Darcs where

import Control.Exception as E
import Control.Monad
import Control.Monad.Trans
#ifdef DARCS28
import Darcs.Commands (DarcsCommand(commandCommand))
import qualified Darcs.Commands.Get as G
import Darcs.Flags (DarcsFlag(..))
import Darcs.Utils (withCurrentDirectory)
#else
import Darcs.UI.Commands (DarcsCommand(commandCommand))
import qualified Darcs.UI.Commands.Get as G
import Darcs.UI.Flags (DarcsFlag(..))
import Darcs.Util.File (withCurrentDirectory)
import Darcs.Util.Path (ioAbsolute)
#endif
import System.Directory
import qualified Darcs.Repository as R
import DarcsDen.Settings


sanityDarcs :: DarcsCommand -> [DarcsFlag] -> [String] -> IO ()
sanityDarcs c fs as =
#ifdef DARCS28
  withCurrentDirectory "." ((commandCommand c) (Quiet:fs) as)
#else
 do
  cwd <- ioAbsolute "."
  withCurrentDirectory "." ((commandCommand c) (cwd, cwd) (Quiet:fs) as)
#endif


get :: MonadIO m => String -> m ()
get from = liftIO $ sanityDarcs G.get [] [from]


getTo :: MonadIO m => String -> String -> m ()
getTo from to = liftIO $ sanityDarcs G.get [] [from, to]


init :: MonadIO m => String -> m ()
init to = liftIO $ do
  createDirectoryIfMissing True to
#ifdef DARCS28
  withCurrentDirectory to (R.createRepository []) `E.catch` \(_ :: IOException) ->
#else
  withCurrentDirectory to (R.createRepository False False True) `E.catch` \(_ :: IOException) ->
#endif
    when deletefiles $ removeDirectoryRecursive to
