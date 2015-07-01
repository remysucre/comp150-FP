{-# LANGUAGE CPP #-}

{- | 
This module reexports either "Data.IdMap.Core.Pure" or "Data.IdMap.Core.Fast" 
depending on whether the @pure@ flag was turned on during the installation of the package.
-}

module Data.IdMap.Core
    ( module
#ifdef __PURE__
             Data.IdMap.Core.Pure
#else
             Data.IdMap.Core.Fast
#endif
                                  ) where

import Data.IdMap.Core.Pure
import Data.IdMap.Core.Fast

