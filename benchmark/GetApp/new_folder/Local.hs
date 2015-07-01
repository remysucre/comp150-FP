{-# LANGUAGE CPP #-}
module Fibon.Run.Config.Local(
  configs
)
where

import Fibon.Run.Config

-- This includes the conifg module imports and defines the localConfigs list
#include "LocalConfigs.txt"

configs :: [RunConfig]
configs = localConfigs

