module Mhailist.Error (
    HandlesError,
    liftError,
    throwError, (+#),
    ErrorT, runErrorT, liftIO,
) where

import Control.Monad.Error

type HandlesError = Either String

liftError :: (Error l, Monad m) => Either l r -> ErrorT l m r
liftError f = case f of
                Left err  -> throwError err
                Right val -> return val

(+#) :: String -> HandlesError b -> HandlesError b
(+#) msg cmp = case cmp of
                 Left err -> throwError $ msg ++ err
                 Right v  -> return v
