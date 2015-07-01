-- Copyright Corey O'Connor
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- | This provides a mock terminal implementation that is nice for testing. This transforms the
-- output operations to visible characters. Which is nice for some tests.
module Graphics.Vty.Output.Mock ( MockData, mockTerminal )
    where

import Graphics.Vty.Prelude

import Graphics.Vty.Output.Interface

import Blaze.ByteString.Builder.Word (writeWord8)

import Control.Monad.Trans

import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.String.UTF8 as UTF8

type MockData = IORef (UTF8.UTF8 BS.ByteString)

-- | The mock display terminal produces a string representation of the requested picture.  There is
-- *not* an isomorphism between the string representation and the picture.  The string
-- representation is a simplification of the picture that is only useful in debugging VTY without
-- considering terminal specific issues.
--
-- The mock implementation is useful in manually determining if the sequence of terminal operations
-- matches the expected sequence. So requirement of the produced representation is simplicity in
-- parsing the text representation and determining how the picture was mapped to terminal
-- operations.
--
-- The string representation is a sequence of identifiers where each identifier is the name of an
-- operation in the algebra.
mockTerminal :: (Applicative m, MonadIO m) => DisplayRegion -> m (MockData, Output)
mockTerminal r = liftIO $ do
    outRef <- newIORef undefined
    newAssumedStateRef <- newIORef initialAssumedState
    let t = Output
            { terminalID = "mock terminal"
            , releaseTerminal = return ()
            , reserveDisplay = return ()
            , releaseDisplay = return ()
            , displayBounds = return r
            , outputByteBuffer = \bytes -> do
                putStrLn $ "mock outputByteBuffer of " ++ show (BS.length bytes) ++ " bytes"
                writeIORef outRef $ UTF8.fromRep bytes
            , contextColorCount = 16
            , supportsCursorVisibility = True
            , assumedStateRef = newAssumedStateRef
            , mkDisplayContext = \tActual rActual -> return $ DisplayContext
                { contextRegion = rActual
                , contextDevice = tActual
                -- A cursor move is always visualized as the single character 'M'
                , writeMoveCursor = \_x _y -> writeWord8 $ toEnum $ fromEnum 'M'
                -- Show cursor is always visualized as the single character 'S'
                , writeShowCursor =  writeWord8 $ toEnum $ fromEnum 'S'
                -- Hide cursor is always visualized as the single character 'H'
                , writeHideCursor = writeWord8 $ toEnum $ fromEnum 'H'
                -- An attr change is always visualized as the single character 'A'
                , writeSetAttr = \_fattr _diffs _attr -> writeWord8 $ toEnum $ fromEnum 'A'
                -- default attr is always visualized as the single character 'D'
                , writeDefaultAttr = writeWord8 $ toEnum $ fromEnum 'D'
                -- row end is always visualized as the single character 'E'
                , writeRowEnd = writeWord8 $ toEnum $ fromEnum 'E'
                , inlineHack = return ()
                }
            }
    return (outRef, t)

