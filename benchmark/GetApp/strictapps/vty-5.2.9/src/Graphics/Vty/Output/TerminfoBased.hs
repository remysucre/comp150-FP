{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -D_XOPEN_SOURCE=500 -fno-warn-warnings-deprecations #-}
{-# CFILES gwinsz.c #-}
-- |  Terminfo based terminal handling.
--
-- The color handling assumes tektronix like. No HP support provided. If the terminal is not one I
-- have easy access to then color support is entirely based of the docs. Probably with some
-- assumptions mixed in.
--
-- Copyright Corey O'Connor (coreyoconnor@gmail.com)
module Graphics.Vty.Output.TerminfoBased ( reserveTerminal )
    where

import Graphics.Vty.Prelude

import Data.ByteString.Internal (toForeignPtr)
import Data.Terminfo.Parse
import Data.Terminfo.Eval

import Graphics.Vty.Attributes
import Graphics.Vty.DisplayAttributes
import Graphics.Vty.Output.Interface

import Blaze.ByteString.Builder (Write, writeToByteString)

import Control.Monad.Trans

import Data.Bits ((.&.))
import Data.Foldable (foldMap)
import Data.IORef
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Monoid
import Data.Word

import Foreign.C.Types ( CInt(..), CLong(..) )
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)

import qualified System.Console.Terminfo as Terminfo
import System.Posix.IO (fdWriteBuf)
import System.Posix.Types (Fd(..))

data TerminfoCaps = TerminfoCaps 
    { smcup :: Maybe CapExpression
    , rmcup :: Maybe CapExpression
    , cup :: CapExpression
    , cnorm :: Maybe CapExpression
    , civis :: Maybe CapExpression
    , supportsNoColors :: Bool
    , useAltColorMap :: Bool
    , setForeColor :: CapExpression
    , setBackColor :: CapExpression
    , setDefaultAttr :: CapExpression
    , clearScreen :: CapExpression
    , clearEol :: CapExpression
    , displayAttrCaps :: DisplayAttrCaps
    }

data DisplayAttrCaps = DisplayAttrCaps
    { setAttrStates :: Maybe CapExpression
    , enterStandout :: Maybe CapExpression
    , exitStandout :: Maybe CapExpression
    , enterUnderline :: Maybe CapExpression
    , exitUnderline :: Maybe CapExpression
    , enterReverseVideo :: Maybe CapExpression
    , enterDimMode :: Maybe CapExpression
    , enterBoldMode :: Maybe CapExpression
    }
    
-- kinda like: https://code.google.com/p/vim/source/browse/src/fileio.c#10422
-- fdWriteBuf will throw on error. Unless the error is EINTR. On EINTR the write will be retried.
fdWriteAll :: Fd -> Ptr Word8 -> Int -> Int -> IO Int
fdWriteAll outFd ptr len count
    | len <  0  = fail "fdWriteAll: len is less than 0"
    | len == 0  = return count
    | otherwise = do
        writeCount <- fromEnum <$> fdWriteBuf outFd ptr (toEnum len)
        let len' = len - writeCount
            ptr' = ptr `plusPtr` writeCount
            count' = count + writeCount
        fdWriteAll outFd ptr' len' count'

sendCapToTerminal :: Output -> CapExpression -> [CapParam] -> IO ()
sendCapToTerminal t cap capParams = do
    outputByteBuffer t $ writeToByteString $ writeCapExpr cap capParams

{- | Uses terminfo for all control codes. While this should provide the most compatible terminal
 - terminfo does not support some features that would increase efficiency and improve compatibility:
 -
 -  * determine the character encoding supported by the terminal. Should this be taken from the LANG
 - environment variable?  
 -
 -  * Provide independent string capabilities for all display attributes.
 - 
 - todo: Some display attributes like underline and bold have independent string capabilities that
 - should be used instead of the generic "sgr" string capability.
 -}
reserveTerminal :: ( Applicative m, MonadIO m ) => String -> Fd -> m Output
reserveTerminal termName outFd = liftIO $ do
    ti <- Terminfo.setupTerm termName
    -- assumes set foreground always implies set background exists.
    -- if set foreground is not set then all color changing style attributes are filtered.
    msetaf <- probeCap ti "setaf"
    msetf <- probeCap ti "setf"
    let (noColors, useAlt, setForeCap) 
            = case msetaf of
                Just setaf -> (False, False, setaf)
                Nothing -> case msetf of
                    Just setf -> (False, True, setf)
                    Nothing -> (True, True, error $ "no fore color support for terminal " ++ termName)
    msetab <- probeCap ti "setab"
    msetb <- probeCap ti "setb"
    let set_back_cap 
            = case msetab of
                Nothing -> case msetb of
                    Just setb -> setb
                    Nothing -> error $ "no back color support for terminal " ++ termName
                Just setab -> setab
    terminfoCaps <- pure TerminfoCaps
        <*> probeCap ti "smcup"
        <*> probeCap ti "rmcup"
        <*> requireCap ti "cup"
        <*> probeCap ti "cnorm"
        <*> probeCap ti "civis"
        <*> pure noColors
        <*> pure useAlt
        <*> pure setForeCap
        <*> pure set_back_cap
        <*> requireCap ti "sgr0"
        <*> requireCap ti "clear"
        <*> requireCap ti "el"
        <*> currentDisplayAttrCaps ti
    newAssumedStateRef <- newIORef initialAssumedState
    let t = Output
            { terminalID = termName
            , releaseTerminal = liftIO $ do
                sendCap setDefaultAttr []
                maybeSendCap cnorm []
            , reserveDisplay = liftIO $ do
                -- If there is no support for smcup: Clear the screen and then move the mouse to the
                -- home position to approximate the behavior.
                maybeSendCap smcup []
                sendCap clearScreen []
            , releaseDisplay = liftIO $ do
                maybeSendCap rmcup []
                maybeSendCap cnorm []
            , displayBounds = do
                rawSize <- liftIO $ getWindowSize outFd
                case rawSize of
                    (w, h)  | w < 0 || h < 0 -> fail $ "getwinsize returned < 0 : " ++ show rawSize
                            | otherwise      -> return (w,h)
            , outputByteBuffer = \outBytes -> do
                let (fptr, offset, len) = toForeignPtr outBytes
                actualLen <- withForeignPtr fptr
                             $ \ptr -> fdWriteAll outFd (ptr `plusPtr` offset) len 0
                when (toEnum len /= actualLen) $ fail $ "Graphics.Vty.Output: outputByteBuffer "
                  ++ "length mismatch. " ++ show len ++ " /= " ++ show actualLen
                  ++ " Please report this bug to vty project."
            , contextColorCount
                = case supportsNoColors terminfoCaps of
                    False -> case Terminfo.getCapability ti (Terminfo.tiGetNum "colors" ) of
                        Nothing -> 8
                        Just v -> toEnum v
                    True -> 1
            , supportsCursorVisibility = isJust $ civis terminfoCaps
            , assumedStateRef = newAssumedStateRef
            -- I think fix would help assure tActual is the only reference. I was having issues
            -- tho.
            , mkDisplayContext = \tActual -> liftIO . terminfoDisplayContext tActual terminfoCaps
            }
        sendCap s = sendCapToTerminal t (s terminfoCaps)
        maybeSendCap s = when (isJust $ s terminfoCaps) . sendCap (fromJust . s)
    return t

requireCap :: (Applicative m, MonadIO m) => Terminfo.Terminal -> String -> m CapExpression
requireCap ti capName 
    = case Terminfo.getCapability ti (Terminfo.tiGetStr capName) of
        Nothing     -> fail $ "Terminal does not define required capability \"" ++ capName ++ "\""
        Just capStr -> parseCap capStr

probeCap :: (Applicative m, MonadIO m) => Terminfo.Terminal -> String -> m (Maybe CapExpression)
probeCap ti capName 
    = case Terminfo.getCapability ti (Terminfo.tiGetStr capName) of
        Nothing     -> return Nothing
        Just capStr -> Just <$> parseCap capStr

parseCap :: (Applicative m, MonadIO m) => String -> m CapExpression
parseCap capStr = do
    case parseCapExpression capStr of
        Left e -> fail $ show e
        Right cap -> return cap

currentDisplayAttrCaps :: ( Applicative m, MonadIO m ) 
                       => Terminfo.Terminal 
                       -> m DisplayAttrCaps
currentDisplayAttrCaps ti 
    =   pure DisplayAttrCaps 
    <*> probeCap ti "sgr"
    <*> probeCap ti "smso"
    <*> probeCap ti "rmso"
    <*> probeCap ti "smul"
    <*> probeCap ti "rmul"
    <*> probeCap ti "rev"
    <*> probeCap ti "dim"
    <*> probeCap ti "bold"

foreign import ccall "gwinsz.h vty_c_get_window_size" c_getWindowSize :: Fd -> IO CLong

getWindowSize :: Fd -> IO (Int,Int)
getWindowSize fd = do 
    (a,b) <- (`divMod` 65536) `fmap` c_getWindowSize fd
    return (fromIntegral b, fromIntegral a)

terminfoDisplayContext :: Output -> TerminfoCaps -> DisplayRegion -> IO DisplayContext
terminfoDisplayContext tActual terminfoCaps r = return dc
    where dc = DisplayContext
            { contextDevice = tActual
            , contextRegion = r
            , writeMoveCursor = \x y -> writeCapExpr (cup terminfoCaps) [toEnum y, toEnum x]
            , writeShowCursor = case cnorm terminfoCaps of
                Nothing -> error "this terminal does not support show cursor"
                Just c -> writeCapExpr c []
            , writeHideCursor = case civis terminfoCaps of
                Nothing -> error "this terminal does not support hide cursor"
                Just c -> writeCapExpr c []
            , writeSetAttr = terminfoWriteSetAttr dc terminfoCaps
            , writeDefaultAttr = writeCapExpr (setDefaultAttr terminfoCaps) []
            , writeRowEnd = writeCapExpr (clearEol terminfoCaps) []
            , inlineHack = return ()
            }

-- | Portably setting the display attributes is a giant pain in the ass.
--
-- If the terminal supports the sgr capability (which sets the on/off state of each style
-- directly ; and, for no good reason, resets the colors to the default) this procedure is used: 
--
--  0. set the style attributes. This resets the fore and back color.
--
--  1, If a foreground color is to be set then set the foreground color
--
--  2. likewise with the background color
-- 
-- If the terminal does not support the sgr cap then:
--  if there is a change from an applied color to the default (in either the fore or back color)
--  then:
--
--      0. reset all display attributes (sgr0)
--
--      1. enter required style modes
--
--      2. set the fore color if required
--
--      3. set the back color if required
--
-- Entering the required style modes could require a reset of the display attributes. If this is
-- the case then the back and fore colors always need to be set if not default.
--
-- This equation implements the above logic.
--
-- \todo This assumes the removal of color changes in the display attributes is done as expected
-- with noColors == True. See `limitAttrForDisplay`
--
-- \todo This assumes that fewer state changes, followed by fewer bytes, is what to optimize. I
-- haven't measured this or even examined terminal implementations. *shrug*
terminfoWriteSetAttr :: DisplayContext -> TerminfoCaps -> FixedAttr -> Attr -> DisplayAttrDiff -> Write
terminfoWriteSetAttr dc terminfoCaps prevAttr reqAttr diffs = do
    case (foreColorDiff diffs == ColorToDefault) || (backColorDiff diffs == ColorToDefault) of
        -- The only way to reset either color, portably, to the default is to use either the set
        -- state capability or the set default capability.
        True  -> do
            case reqDisplayCapSeqFor (displayAttrCaps terminfoCaps)
                                     (fixedStyle attr )
                                     (styleToApplySeq $ fixedStyle attr) of
                -- only way to reset a color to the defaults
                EnterExitSeq caps -> writeDefaultAttr dc
                                     `mappend` 
                                     foldMap (\cap -> writeCapExpr cap []) caps
                                     `mappend`
                                     setColors
                -- implicitly resets the colors to the defaults
                SetState state -> writeCapExpr (fromJust $ setAttrStates 
                                                         $ displayAttrCaps 
                                                         $ terminfoCaps
                                               )
                                               (sgrArgsForState state)
                                  `mappend`
                                  setColors
        -- Otherwise the display colors are not changing or changing between two non-default
        -- points.
        False -> do
            -- Still, it could be the case that the change in display attributes requires the
            -- colors to be reset because the required capability was not available.
            case reqDisplayCapSeqFor (displayAttrCaps terminfoCaps)
                                     (fixedStyle attr)
                                     (styleDiffs diffs) of
                -- Really, if terminals were re-implemented with modern concepts instead of bowing
                -- down to 40 yr old dumb terminal requirements this would be the only case ever
                -- reached!  Changes the style and color states according to the differences with
                -- the currently applied states.
                EnterExitSeq caps -> foldMap (\cap -> writeCapExpr cap []) caps
                                     `mappend`
                                     writeColorDiff setForeColor (foreColorDiff diffs)
                                     `mappend`
                                     writeColorDiff setBackColor (backColorDiff diffs)
                -- implicitly resets the colors to the defaults
                SetState state -> writeCapExpr (fromJust $ setAttrStates 
                                                         $ displayAttrCaps terminfoCaps
                                               )
                                               (sgrArgsForState state)
                                  `mappend` setColors
    where 
        colorMap = case useAltColorMap terminfoCaps of
                        False -> ansiColorIndex
                        True -> altColorIndex
        attr = fixDisplayAttr prevAttr reqAttr
        setColors =
            (case fixedForeColor attr of
                Just c -> writeCapExpr (setForeColor terminfoCaps)
                                       [toEnum $ colorMap c]
                Nothing -> mempty)
            `mappend`
            (case fixedBackColor attr of
                Just c -> writeCapExpr (setBackColor terminfoCaps)
                                       [toEnum $ colorMap c]
                Nothing -> mempty)
        writeColorDiff _f NoColorChange
            = mempty
        writeColorDiff _f ColorToDefault
            = error "ColorToDefault is not a possible case for applyColorDiffs"
        writeColorDiff f (SetColor c)
            = writeCapExpr (f terminfoCaps) [toEnum $ colorMap c]

-- | The color table used by a terminal is a 16 color set followed by a 240 color set that might not
-- be supported by the terminal.
--
-- This takes a Color which clearly identifies which pallete to use and computes the index
-- into the full 256 color pallete.
ansiColorIndex :: Color -> Int
ansiColorIndex (ISOColor v) = fromEnum v
ansiColorIndex (Color240 v) = 16 + fromEnum v

-- | For terminals without setaf/setab
-- 
-- See table in `man terminfo`
-- Will error if not in table.
altColorIndex :: Color -> Int
altColorIndex (ISOColor 0) = 0
altColorIndex (ISOColor 1) = 4
altColorIndex (ISOColor 2) = 2
altColorIndex (ISOColor 3) = 6
altColorIndex (ISOColor 4) = 1
altColorIndex (ISOColor 5) = 5
altColorIndex (ISOColor 6) = 3
altColorIndex (ISOColor 7) = 7
altColorIndex (ISOColor v) = fromEnum v
altColorIndex (Color240 v) = 16 + fromEnum v

{- | The sequence of terminfo caps to apply a given style are determined according to these rules.
 -
 -  1. The assumption is that it's preferable to use the simpler enter/exit mode capabilities than
 -  the full set display attribute state capability. 
 -
 -  2. If a mode is supposed to be removed but there is not an exit capability defined then the
 -  display attributes are reset to defaults then the display attribute state is set.
 -
 -  3. If a mode is supposed to be applied but there is not an enter capability defined then then
 -  display attribute state is set if possible. Otherwise the mode is not applied.
 -
 -  4. If the display attribute state is being set then just update the arguments to that for any
 -  apply/remove.
 -
 -}
data DisplayAttrSeq
    = EnterExitSeq [CapExpression]
    | SetState DisplayAttrState

data DisplayAttrState = DisplayAttrState
    { applyStandout :: Bool
    , applyUnderline :: Bool
    , applyReverseVideo :: Bool
    , applyBlink :: Bool
    , applyDim :: Bool
    , applyBold :: Bool
    }

sgrArgsForState :: DisplayAttrState -> [CapParam]
sgrArgsForState attrState = map (\b -> if b then 1 else 0)
    [ applyStandout attrState
    , applyUnderline attrState
    , applyReverseVideo attrState
    , applyBlink attrState
    , applyDim attrState
    , applyBold attrState
    , False -- invis
    , False -- protect
    , False -- alt char set
    ]

reqDisplayCapSeqFor :: DisplayAttrCaps -> Style -> [StyleStateChange] -> DisplayAttrSeq
reqDisplayCapSeqFor caps s diffs
    -- if the state transition implied by any diff cannot be supported with an enter/exit mode cap
    -- then either the state needs to be set or the attribute change ignored.
    = case (any noEnterExitCap diffs, isJust $ setAttrStates caps) of
        -- If all the diffs have an enter-exit cap then just use those
        ( False, _    ) -> EnterExitSeq $ map enterExitCap diffs
        -- If not all the diffs have an enter-exit cap and there is no set state cap then filter out
        -- all unsupported diffs and just apply the rest
        ( True, False ) -> EnterExitSeq $ map enterExitCap 
                                        $ filter (not . noEnterExitCap) diffs
        -- if not all the diffs have an enter-exit can and there is a set state cap then just use
        -- the set state cap.
        ( True, True  ) -> SetState $ stateForStyle s
    where
        noEnterExitCap ApplyStandout = isNothing $ enterStandout caps
        noEnterExitCap RemoveStandout = isNothing $ exitStandout caps
        noEnterExitCap ApplyUnderline = isNothing $ enterUnderline caps
        noEnterExitCap RemoveUnderline = isNothing $ exitUnderline caps
        noEnterExitCap ApplyReverseVideo = isNothing $ enterReverseVideo caps
        noEnterExitCap RemoveReverseVideo = True
        noEnterExitCap ApplyBlink = True
        noEnterExitCap RemoveBlink = True
        noEnterExitCap ApplyDim = isNothing $ enterDimMode caps
        noEnterExitCap RemoveDim = True
        noEnterExitCap ApplyBold = isNothing $ enterBoldMode caps
        noEnterExitCap RemoveBold = True
        enterExitCap ApplyStandout = fromJust $ enterStandout caps
        enterExitCap RemoveStandout = fromJust $ exitStandout caps
        enterExitCap ApplyUnderline = fromJust $ enterUnderline caps
        enterExitCap RemoveUnderline = fromJust $ exitUnderline caps
        enterExitCap ApplyReverseVideo = fromJust $ enterReverseVideo caps
        enterExitCap ApplyDim = fromJust $ enterDimMode caps
        enterExitCap ApplyBold = fromJust $ enterBoldMode caps
        enterExitCap _ = error "enterExitCap applied to diff that was known not to have one."

stateForStyle :: Style -> DisplayAttrState
stateForStyle s = DisplayAttrState
    { applyStandout = isStyleSet standout
    , applyUnderline = isStyleSet underline
    , applyReverseVideo = isStyleSet reverseVideo
    , applyBlink = isStyleSet blink
    , applyDim = isStyleSet dim
    , applyBold = isStyleSet bold
    }
    where isStyleSet = hasStyle s

styleToApplySeq :: Style -> [StyleStateChange]
styleToApplySeq s = concat
    [ applyIfRequired ApplyStandout standout
    , applyIfRequired ApplyUnderline underline
    , applyIfRequired ApplyReverseVideo reverseVideo
    , applyIfRequired ApplyBlink blink
    , applyIfRequired ApplyDim dim
    , applyIfRequired ApplyBlink bold
    ]
    where 
        applyIfRequired op flag 
            = if 0 == (flag .&. s)
                then []
                else [op]

