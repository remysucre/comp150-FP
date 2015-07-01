module Casui.Draw where

{-# OPTIONS_GHC -XPackageImports #-}

import Graphics.UI.Gtk hiding (Arrow)
import Graphics.UI.Gtk.Gdk.GC
import Numeric
import Data.Bits
import Control.Monad.Trans
import Control.Applicative
import Casui.Utils
import Control.Arrow
import Data.List
import Control.Monad
import Data.Maybe

import Casui.Debug

-- deriving instance Show Rectangle

data Drawing t = LineD Point Point
          | GlyphD GlyphItem Double (Int, Int)
          | GroupD [DrawAttrib] [(Point, DrawInfo t)]
          | TagD t (Drawing t)
            deriving Show

data DrawAttrib = ColorA Color deriving Show

data DrawInfo t = DrawInfo {
      diBox :: Rectangle,
      diDrawing :: Drawing t
      -- ...
    } deriving Show

{-instance Show Color where
    showsPrec _ (Color r g b) = shows $ "#" ++ hexcolor r ++ hexcolor g ++ hexcolor b
	where hexcolor = pad . flip showHex "" . flip shiftR 8
	      pad [x] = ['0',x]
	      pad xs = xs-}

instance Show GlyphItem where
    show l = "<Glyph>"

-- todo: returned (c, scale) defines order on children, the [DrawInfo]
-- should use same order and be purged as it is used
newtype Draw t a = Draw {
      drawF :: Double
            -> PangoContext
            -> (DrawInfo t -> DrawInfo t -> DrawInfo t)
            -> IO (Maybe (DrawInfo t), a)
    }

instance Monad (Draw t) where
    return a = Draw $ \_ _ _ -> return (Nothing, a)
    d >>= f = Draw $ \s c m -> do (d, a) <- drawF d s c m
                                  (e, a') <- drawF (f a) s c m
                                  return (case (d, e) of
                                            (Just x, Just y) -> Just $ m x y
                                            (Just x, _) -> Just x
                                            (_, Just y) -> Just y
                                            _ -> Nothing
                                         , a')

instance MonadIO (Draw t) where
    liftIO a = Draw $ \_ _ _ -> (,) Nothing <$> a

instance Functor (Draw t) where
    fmap f = (>>= return . f)

runDraw :: Draw t a -> Double -> PangoContext -> IO a
runDraw d s c = snd <$> drawF d s c squashed

emptyDI :: DrawInfo t
emptyDI = DrawInfo (Rectangle 0 0 0 0) (GroupD [] [])

emptyp :: DrawInfo t -> Bool
emptyp (DrawInfo _ (GroupD _ [])) = True
emptyp _ = False

squashed :: DrawInfo t -> DrawInfo t -> DrawInfo t
squashed = groupDIs 0 0

groupDIs :: (Int, Int) -> (Int, Int) -> DrawInfo t -> DrawInfo t -> DrawInfo t
groupDIs dxy0@(px0,py0) dxy1@(px1,py1)
         d0@(DrawInfo r0@(Rectangle x0 y0 w0 h0) _)
         d1@(DrawInfo r1@(Rectangle x1 y1 w1 h1) _) =
             let rx0 = x0 + px0
                 rx1 = x1 + px1
                 ry0 = y0 + py0
                 ry1 = y1 + py1
                 --dxy0 = 0
                 --dxy1 = (rx1 - rx0, ry1 - ry0)
                 lx = min px0 px1 --rx0 rx1
                 by = min py0 py1 --ry0 ry1
                 rx = max (px0 + w0) (px1 + w1) -- (rx0 + w0) (rx1 + w1)
                 ty = max (py0 + h0) (py1 + h1) -- (ry0 + h0) (ry1 + h1)
                 tw = rx - lx
                 th = ty - by
                 --splat p (DrawInfo _ (GroupD [] l)) = map (first (+p)) l -- don't ignore diBox
                 splat p d = [(p, d)] in
             trace (show (dxy0, r0, dxy1, r1, tw, th)) $
             DrawInfo (Rectangle 0 0 tw th) -- (Rectangle rx0 ry0 tw th)
                      (GroupD [] $ splat dxy0 d0 ++ splat dxy1 d1)

getScale :: Draw t Double
getScale = Draw $ \s _ _ -> return (Nothing, s)

getPangoContext :: Draw t PangoContext
getPangoContext = Draw $ \_ c _ -> return (Nothing, c)

putDI :: DrawInfo t -> Draw t ()
putDI d = Draw $ \_ _ _ -> return (Just d, ())

removeDI :: Draw t a -> Draw t (Maybe (DrawInfo t), a)
removeDI d = Draw $ \s c m ->
             do (e, a) <- drawF d s c m
                return (Nothing, (e,a))

removeDI_ d = fst <$> removeDI d

getDI :: Draw t a -> Draw t (Maybe (DrawInfo t), a)
getDI d = do
  (e, a) <- removeDI d
  maybe (return ()) putDI e
  return (e, a)

scaleBy :: Double -> Draw t a -> Draw t a
scaleBy sc d = Draw $ \s -> drawF d (s * sc)

withScale :: Double -> Draw t a -> Draw t a
withScale sc d = Draw $ \s -> drawF d sc

diPos :: DrawInfo t -> (Int, Int)
diPos (DrawInfo (Rectangle x y _ _) _) = (x, y)

diSize :: DrawInfo t -> (Int, Int)
diSize (DrawInfo (Rectangle _ _ w h) _) = (w, h)

displaying :: (DrawInfo t -> DrawInfo t -> DrawInfo t) -> Draw t a -> Draw t a
displaying m d = Draw $ \s c _ -> drawF d s c m

tag :: t -> Draw t d -> Draw t d
tag t d = Draw $ \s c m ->
          first (Just . tagDI t . fromMaybe emptyDI) <$> drawF d s c m

tagDI :: t -> DrawInfo t -> DrawInfo t
tagDI t di@(DrawInfo { diDrawing = d }) = di { diDrawing = TagD t d }


draw :: (DrawableClass d, Show t) => d -> GC -> (Int, Int) -> DrawInfo t -> IO ()
draw dw gc p = draw' dw gc p . diDrawing

draw' :: (DrawableClass d, Show t) => d -> GC -> (Int, Int) -> Drawing t -> IO ()
draw' dw gc (x,y) (LineD (x1,y1) (x2,y2)) = drawLine dw gc (x1+x, y1+y) (x2+x, y2+y)
draw' dw gc (x,y) (GlyphD t a (ox, oy)) = drawGlyphs dw gc (x-ox) (y-oy) t
draw' dw gc (x,y) (GroupD [] l) = mapM_ f l
    where f ((sx,sy),d) = draw dw gc (x+sx, y+sy) d
draw' dw gc (x,y) (GroupD a l) = do save <- modifyGC gc a; mapM_ f l; gcSetValues gc save
    where f ((sx,sy),d) = draw dw gc (x+sx, y+sy) d
draw' dw gc p (TagD _ d) = draw' dw gc p d

modifyGC :: GC -> [DrawAttrib] -> IO GCValues
modifyGC gc l = do s <- gcGetValues gc; gcSetValues gc $ foldl f s l; return s
    where f a (ColorA c') = a {foreground = c'}

layoutSetFontSize :: PangoLayout -> Double -> IO ()
layoutSetFontSize layout size = do
  text <- layoutGetText layout
  layoutSetAttributes layout [AttrSize 0 (length text) size]

glyphItemSize :: GlyphItem -> IO (Int, Int)
glyphItemSize g = do
  (_, Rectangle _ _ w h) <- glyphItemPixelExtents g
  return (w,h)

glyphItemOffset :: GlyphItem -> IO (Int, Int)
glyphItemOffset = glyphItemOffset' True

glyphItemOffset' t g = do
  (Rectangle a b _ _, Rectangle x y _ _) <- glyphItemPixelExtents g
  return $ if t then (x,y) else (a,b)

glyphItemPixelExtents :: GlyphItem -> IO (Rectangle, Rectangle)
glyphItemPixelExtents g = do
  (PangoRectangle a b c d, PangoRectangle e f g h) <- glyphItemExtents g
  return (Rectangle (round a) (round b) (round c) (round d),
          Rectangle (round e) (round f) (round g) (round h))

glyphItemBBox :: GlyphItem -> IO Rectangle
glyphItemBBox g = snd <$> glyphItemPixelExtents g

glyphItemBBox' inkOrLogical g = (if inkOrLogical then snd else fst) <$> glyphItemPixelExtents g

-- todo: dynamic
normalFontSize :: Double
normalFontSize = 32

minFontSize :: Double
minFontSize = 6

scaleMultiplier :: Double
scaleMultiplier = 0.7

-- |Horizontal box combiner
nextTo :: DrawInfo t -> DrawInfo t -> DrawInfo t
nextTo d@(DrawInfo (Rectangle _ _ w _) _) =
           groupDIs 0 (w, 0) d

superscript :: DrawInfo t -> DrawInfo t -> DrawInfo t
superscript
  d1@(DrawInfo (Rectangle _ _ w1 h1) _)
  d2@(DrawInfo (Rectangle _ _ w2 h2) _) =
    groupDIs (0, h2 `div` 2)
            (w1, 0)
            d1 d2
    
leftSuperscript :: DrawInfo t -> DrawInfo t -> DrawInfo t
leftSuperscript
  d1@(DrawInfo (Rectangle _ _ w1 h1) _)
  d2@(DrawInfo (Rectangle _ _ w2 h2) _) =
    groupDIs 0
            (w1, h1`div`2)
            d1 d2

centeredNextTo :: DrawInfo t -> DrawInfo t -> DrawInfo t
centeredNextTo  d@(DrawInfo (Rectangle x0 y0 w0 h0) _)
       e@(DrawInfo (Rectangle x1 y1 w1 h1) _) =
           let th = max h0 h1
           in groupDIs
                  (0, (th - h0) `div` 2)
                  (w0, (th - h1) `div` 2)
                  d e


-- |Centered vertical box combiner
centeredUnder :: DrawInfo t -> DrawInfo t -> DrawInfo t
centeredUnder  d@(DrawInfo (Rectangle x0 y0 w0 h0) _)
       e@(DrawInfo (Rectangle x1 y1 w1 h1) _) =
           let tw = max w0 w1
           in groupDIs
                  ((tw - w0) `div` 2, 0) --(div (tw-w0) 2 - x0, 0)
                  ((tw - w1) `div` 2, h0) --(div (tw-w1) 2 - x1, h0 - y0 + y1)
                  d e
       

getFontSize :: Draw t Double
getFontSize = max minFontSize . (normalFontSize *) <$> getScale

text :: String -> Draw t ()
text str = advancedText True str

advancedText inkOrLogical str = do
  size <- getFontSize
  context <- getPangoContext
  dis <- liftIO $ do
           pangois <- pangoItemize context str [AttrSize 0 (length str) size]
           ascents <- map ascent <$> mapM pangoItemGetFontMetrics pangois
           glyphs <- mapM pangoShape pangois
           offsets <- mapM (glyphItemOffset' inkOrLogical) glyphs
           boxes <- mapM (glyphItemBBox' inkOrLogical) glyphs
           return $ zipWith DrawInfo boxes $ zipWith3 GlyphD glyphs ascents offsets
  displaying nextTo $ mapM_ putDI dis

parenLeft  = "(⎛⎜⎝"
parenRight = ")⎞⎟⎠"

vstretch :: String -> Int -> Draw t ()
vstretch [small,top,middle,bottom] heighti =
  let height = trn "height: " $ realToFrac heighti in
     if height < 2 * normalFontSize
     then {- withScale (height / normalFontSize) $-} text [small]
     else displaying centeredUnder $ do
       advancedText True [top]
       advancedText True [middle]
       advancedText True [bottom]
       
addParens :: Draw t a -> Draw t a
addParens d = displaying nextTo $ do
  (mdi, b) <- removeDI d
  case mdi of
    Nothing -> text "()" >> return b
    Just di -> displaying centeredNextTo $ do
      let height = rectHeight $ diBox di
      vstretch parenLeft height
      putDI di
      vstretch parenRight height
      return b

getTagsPos :: Point -> Drawing t -> [(Point, t)]
getTagsPos p (TagD t di) = [(p, t)]
getTagsPos p (GroupD _ l) = concatMap (uncurry getTagsPos . ((+p) *** diDrawing)) l
getTagsPos _ _ = []

rectWidth :: Rectangle -> Int
rectWidth (Rectangle _ _ w _) = w

rectHeight :: Rectangle -> Int
rectHeight (Rectangle _ _ _ h) = h

line :: Int -> Int -> Int -> Int -> Draw t ()
line x0 y0 x1 y1 = putDI (DrawInfo (Rectangle x0 y0 (x1-x0) (y1-y0)) (LineD (x0,y0) (x1,y1)))
