{-# LANGUAGE UnicodeSyntax, CPP #-}

-- | Todos.Default.* modules contain implementation of RuntimeConfig instance for DefaultConfig type.

module Todos.Default
  (module Todos.Default.CmdLine,
   module Todos.Default.Instances,
   module Todos.Default.Utils,
   module Todos.Default.Print,
#ifdef WITH_CURSES
   module Todos.Default.Curses
#endif
   )
  where

import Todos.Default.CmdLine
import Todos.Default.Instances ()
import Todos.Default.Utils
import Todos.Default.Print

#ifdef WITH_CURSES
import Todos.Default.Curses
#endif
