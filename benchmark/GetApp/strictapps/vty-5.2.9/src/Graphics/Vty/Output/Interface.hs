-- Copyright Corey O'Connor
-- General philosophy is: MonadIO is for equations exposed to clients.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.Vty.Output.Interface
where

import Graphics.Vty.Prelude

import Graphics.Vty.Picture
import Graphics.Vty.PictureToSpans
import Graphics.Vty.Span

import Graphics.Vty.DisplayAttributes

import Blaze.ByteString.Builder (Write, writeToByteString)
import Blaze.ByteString.Builder.ByteString (writeByteString)

import Control.Monad.Trans

import qualified Data.ByteString as BS
import Data.IORef
import Data.Monoid (mempty, mappend)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as Vector

data Output = Output
    { -- | Text identifier for the output device. Used for debugging. 
      terminalID :: String
    , releaseTerminal :: MonadIO m => m ()
    -- | Clear the display and initialize the terminal to some initial display state. 
    --
    -- The expectation of a program is that the display starts in some initial state. 
    -- The initial state would consist of fixed values:
    --
    --  - cursor at top left
    --  - UTF-8 character encoding
    --  - drawing characteristics are the default
    --
    -- The abstract operation I think all these behaviors are instances of is reserving exclusive
    -- access to a display such that:
    --
    --  - The previous state cannot be determined
    --  - When exclusive access to a display is released the display returns to the previous state.
    , reserveDisplay :: MonadIO m => m ()
    -- | Return the display to the state before `reserveDisplay`
    -- If no previous state then set the display state to the initial state.
    , releaseDisplay :: MonadIO m => m ()
    -- | Returns the current display bounds.
    , displayBounds :: MonadIO m => m DisplayRegion
    -- | Output the byte string to the terminal device.
    , outputByteBuffer :: BS.ByteString -> IO ()
    -- | Maximum number of colors supported by the context.
    , contextColorCount :: Int
    -- | if the cursor can be shown / hidden
    , supportsCursorVisibility :: Bool
    , assumedStateRef :: IORef AssumedState
    -- | Acquire display access to the given region of the display.
    -- Currently all regions have the upper left corner of (0,0) and the lower right corner at 
    -- (max displayWidth providedWidth, max displayHeight providedHeight)
    , mkDisplayContext :: MonadIO m => Output -> DisplayRegion -> m DisplayContext
    }

displayContext :: MonadIO m => Output -> DisplayRegion -> m DisplayContext
displayContext t = liftIO . mkDisplayContext t t

data AssumedState = AssumedState
    { prevFattr :: Maybe FixedAttr
    , prevOutputOps :: Maybe DisplayOps
    }

initialAssumedState :: AssumedState
initialAssumedState = AssumedState Nothing Nothing

data DisplayContext = DisplayContext
    { contextDevice :: Output
    -- | Provide the bounds of the display context. 
    , contextRegion :: DisplayRegion
    --  | sets the output position to the specified row and column. Where the number of bytes
    --  required for the control codes can be specified seperate from the actual byte sequence.
    , writeMoveCursor :: Int -> Int -> Write
    , writeShowCursor :: Write
    , writeHideCursor :: Write
    --  | Assure the specified output attributes will be applied to all the following text until the
    --  next output attribute change. Where the number of bytes required for the control codes can
    --  be specified seperate from the actual byte sequence.  The required number of bytes must be
    --  at least the maximum number of bytes required by any attribute changes.  The serialization
    --  equations must provide the ptr to the next byte to be specified in the output buffer.
    --
    --  The currently applied display attributes are provided as well. The Attr data type can
    --  specify the style or color should not be changed from the currently applied display
    --  attributes. In order to support this the currently applied display attributes are required.
    --  In addition it may be possible to optimize the state changes based off the currently applied
    --  display attributes.
    , writeSetAttr :: FixedAttr -> Attr -> DisplayAttrDiff -> Write
    -- | Reset the display attributes to the default display attributes
    , writeDefaultAttr :: Write
    , writeRowEnd :: Write
    -- | See `Graphics.Vty.Output.XTermColor.inlineHack`
    , inlineHack :: IO ()
    }

-- | All terminals serialize UTF8 text to the terminal device exactly as serialized in memory.
writeUtf8Text  :: BS.ByteString -> Write
writeUtf8Text = writeByteString

-- | Displays the given `Picture`.
--
--      0. The image is cropped to the display size. 
--
--      1. Converted into a sequence of attribute changes and text spans.
--      
--      2. The cursor is hidden.
--
--      3. Serialized to the display.
--
--      4. The cursor is then shown and positioned or kept hidden.
-- 
-- todo: specify possible IO exceptions.
-- abstract from IO monad to a MonadIO instance.
outputPicture :: MonadIO m => DisplayContext -> Picture -> m ()
outputPicture dc pic = liftIO $ do
    as <- readIORef (assumedStateRef $ contextDevice dc)
    let manipCursor = supportsCursorVisibility (contextDevice dc)
        r = contextRegion dc
        ops = displayOpsForPic pic r
        initialAttr = FixedAttr defaultStyleMask Nothing Nothing
        -- Diff the previous output against the requested output. Differences are currently on a per-row
        -- basis.
        -- \todo handle resizes that crop the dominate directions better.
        diffs :: [Bool] = case prevOutputOps as of
            Nothing -> replicate (fromEnum $ regionHeight $ effectedRegion ops) True
            Just previousOps -> if effectedRegion previousOps /= effectedRegion ops
                then replicate (displayOpsRows ops) True
                else zipWith (/=) (Vector.toList previousOps)
                                  (Vector.toList ops)
        -- build the Write corresponding to the output image
        out = (if manipCursor then writeHideCursor dc else mempty)
              `mappend` writeDefaultAttr dc
              `mappend` writeOutputOps dc initialAttr diffs ops
              `mappend`
                (case picCursor pic of
                    _ | not manipCursor -> mempty
                    NoCursor             -> mempty
                    Cursor x y           ->
                        let m = cursorOutputMap ops $ picCursor pic
                            (ox, oy) = charToOutputPos m (x,y)
                        in writeShowCursor dc `mappend` writeMoveCursor dc ox oy
                )
    -- ... then serialize
    outputByteBuffer (contextDevice dc) (writeToByteString out)
    -- Cache the output spans.
    let as' = as { prevOutputOps = Just ops }
    writeIORef (assumedStateRef $ contextDevice dc) as'

writeOutputOps :: DisplayContext -> FixedAttr -> [Bool] -> DisplayOps -> Write
writeOutputOps dc inFattr diffs ops =
    let (_, out, _, _) = Vector.foldl' writeOutputOps' 
                                       (0, mempty, inFattr, diffs) 
                                       ops
    in out
    where 
        writeOutputOps' (y, out, fattr, True : diffs') spanOps
            = let (spanOut, fattr') = writeSpanOps dc y fattr spanOps
              in (y+1, out `mappend` spanOut, fattr', diffs')
        writeOutputOps' (y, out, fattr, False : diffs') _spanOps
            = (y + 1, out, fattr, diffs')
        writeOutputOps' (_y, _out, _fattr, []) _spanOps
            = error "vty - output spans without a corresponding diff."

writeSpanOps :: DisplayContext -> Int -> FixedAttr -> SpanOps -> (Write, FixedAttr)
writeSpanOps dc y inFattr spanOps =
    -- The first operation is to set the cursor to the start of the row
    let start = writeMoveCursor dc 0 y
    -- then the span ops are serialized in the order specified
    in Vector.foldl' (\(out, fattr) op -> case writeSpanOp dc op fattr of
                                            (opOut, fattr') -> (out `mappend` opOut, fattr')
                     )
                     (start, inFattr)
                     spanOps

writeSpanOp :: DisplayContext -> SpanOp -> FixedAttr -> (Write, FixedAttr)
writeSpanOp dc (TextSpan attr _ _ str) fattr =
    let attr' = limitAttrForDisplay (contextDevice dc) attr
        fattr' = fixDisplayAttr fattr attr'
        diffs = displayAttrDiffs fattr fattr'
        out =  writeSetAttr dc fattr attr' diffs
               `mappend` writeUtf8Text (T.encodeUtf8 $ TL.toStrict str)
    in (out, fattr')
writeSpanOp _dc (Skip _) _fattr = error "writeSpanOp for Skip"
writeSpanOp dc (RowEnd _) fattr = (writeRowEnd dc, fattr)

-- | The cursor position is given in X,Y character offsets. Due to multi-column characters this
-- needs to be translated to column, row positions.
data CursorOutputMap = CursorOutputMap
    { charToOutputPos :: (Int, Int) -> (Int, Int)
    } 

cursorOutputMap :: DisplayOps -> Cursor -> CursorOutputMap
cursorOutputMap spanOps _cursor = CursorOutputMap
    { charToOutputPos = \(cx, cy) -> (cursorColumnOffset spanOps cx cy, cy)
    }

cursorColumnOffset :: DisplayOps -> Int -> Int -> Int
cursorColumnOffset ops cx cy =
    let cursorRowOps = Vector.unsafeIndex ops (fromEnum cy)
        (outOffset, _, _) 
            = Vector.foldl' ( \(d, currentCx, done) op -> 
                        if done then (d, currentCx, done) else case spanOpHasWidth op of
                            Nothing -> (d, currentCx, False)
                            Just (cw, ow) -> case compare cx (currentCx + cw) of
                                    GT -> ( d + ow
                                          , currentCx + cw
                                          , False 
                                          )
                                    EQ -> ( d + ow
                                          , currentCx + cw
                                          , True 
                                          )
                                    LT -> ( d + columnsToCharOffset (cx - currentCx) op
                                          , currentCx + cw
                                          , True
                                          )
                      )
                      (0, 0, False)
                      cursorRowOps
    in outOffset

-- | Not all terminals support all display attributes. This filters a display attribute to what the
-- given terminal can display.
limitAttrForDisplay :: Output -> Attr -> Attr
limitAttrForDisplay t attr 
    = attr { attrForeColor = clampColor $ attrForeColor attr
           , attrBackColor = clampColor $ attrBackColor attr
           }
    where
        clampColor Default     = Default
        clampColor KeepCurrent = KeepCurrent
        clampColor (SetTo c)   = clampColor' c
        clampColor' (ISOColor v) 
            | contextColorCount t < 8            = Default
            | contextColorCount t < 16 && v >= 8 = SetTo $ ISOColor (v - 8)
            | otherwise                          = SetTo $ ISOColor v
        clampColor' (Color240 v)
            -- TODO: Choose closes ISO color?
            | contextColorCount t <  8           = Default
            | contextColorCount t <  16          = Default
            | contextColorCount t <= 256         = SetTo $ Color240 v
            | otherwise 
                = let p :: Double = fromIntegral v / 240.0 
                      v' = floor $ p * (fromIntegral $ contextColorCount t)
                  in SetTo $ Color240 v'
