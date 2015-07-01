{-

    gmndl -- Mandelbrot Set explorer
    Copyright (C) 2010,2011,2014  Claude Heiland-Allen <claude@mathr.co.uk>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

-}

module Main (main) where

import Prelude hiding (isNaN)

import Control.Concurrent (forkIO, newEmptyMVar, takeMVar, putMVar)
import Control.Monad (forever)

-- some simple helpers
import Data.List (isPrefixOf)

-- the dependency on mtl is just for this!
import Control.Monad.Trans (liftIO)

-- the main program thread needs to store some thread-local state
import Data.IORef (newIORef, readIORef, writeIORef)

-- build the interface with GTK to allow more fancy controls later
import Graphics.UI.Gtk

-- use OpenGL to display frequently update images on a textured quad
import Graphics.UI.Gtk.OpenGL
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

-- need a hack to ensure correct qd operation
import Numeric.QD.QuadDouble (QuadDouble())
import Foreign (nullPtr)

import Complex (Complex((:+)))

-- mu-atom properties
import Calculate
import qualified Image
import Address (parse, parameter)

isNaN x = not (x == x)

-- the state we need for everything
data GMNDL
  = Invalid
  | GMNDL
      { center :: Complex QuadDouble
      , zradius :: QuadDouble
      , image :: Image.Image
      , stop :: IO ()
      }

-- command line arguments: currently only initial window dimensions
data Args = Args{ aWidth :: Int, aHeight :: Int, aOversample :: Int, aRe :: QuadDouble, aIm :: QuadDouble, aZr :: QuadDouble }

-- and the defaults are suitable for PAL DVD rendering, if that should
-- come to pass in the future
defaultArgs :: Args
defaultArgs = Args{ aWidth = 788, aHeight = 576, aOversample = 1, aRe = 0, aIm = 0, aZr = 2 }

-- braindead argument parser: latest argument takes priority
-- probably should use Monoid instances for this stuff
combineArgs :: Args -> String -> Args
combineArgs a0 s
  | "--width="  `isPrefixOf` s = a0{ aWidth  = read $ "--width="  `dropPrefix` s }
  | "-w="       `isPrefixOf` s = a0{ aWidth  = read $ "-w="       `dropPrefix` s }
  | "--height=" `isPrefixOf` s = a0{ aHeight = read $ "--height=" `dropPrefix` s }
  | "-h="       `isPrefixOf` s = a0{ aHeight = read $ "-h="       `dropPrefix` s }
  | "--aa="     `isPrefixOf` s = a0{ aOversample = read $ "--aa=" `dropPrefix` s }
  | "--re="     `isPrefixOf` s = a0{ aRe = read $ "--re=" `dropPrefix` s }
  | "--im="     `isPrefixOf` s = a0{ aIm = read $ "--im=" `dropPrefix` s }
  | "--zr="     `isPrefixOf` s = a0{ aZr = read $ "--zr=" `dropPrefix` s }
  | otherwise = a0

-- this is a bit silly, especially with the duplicated string literals..
dropPrefix :: String -> String -> String
dropPrefix p s = drop (length p) s

-- the main program!
main :: IO ()
main = do
  args <- foldl combineArgs defaultArgs `fmap` unsafeInitGUIForThreadedRTS
  let width = aWidth args
      height = aHeight args
      oversample = aOversample args
      rng = ((0, 0), (oversample * height - 1, oversample * width - 1))
  _ <- initGL
  glconfig <- glConfigNew [ GLModeRGBA, GLModeDouble ]
  canvas <- glDrawingAreaNew glconfig
  widgetSetSizeRequest canvas width height
  window <- windowNew
  eventb <- eventBoxNew
  vbox <- vBoxNew False 0
  status <- vBoxNew False 0
  statusRe <- entryNew
  statusIm <- entryNew
  statusZr <- entryNew
  ratios <- entryNew
  boxPackStart vbox eventb PackGrow 0
  boxPackStart vbox status PackGrow 0
  boxPackStart vbox ratios PackGrow 0
  boxPackStart status statusRe PackGrow 0
  boxPackStart status statusIm PackGrow 0
  boxPackStart status statusZr PackGrow 0
  let -- update the status bar
      updateStatus re im zr = do
        entrySetText statusRe (show re)
        entrySetText statusIm (show im)
        entrySetText statusZr (show zr)
  set window [ containerBorderWidth := 0, containerChild := vbox, windowResizable := False ]
  set eventb [ containerBorderWidth := 0, containerChild := canvas ]
  -- initial state is invalid because...
  sR <- newIORef Invalid
  done <- newEmptyMVar
  let -- restart the renderer
      restart :: IO ()
      restart = do
        g <- readIORef sR
        stop g
        Image.clear (image g)
        stop' <- renderer done rng (Image.plot (image g)) (center g) (zradius g)
        writeIORef sR $! g{ stop = stop' }
        let re :+ im = center g
        updateStatus re im (zradius g)
  -- ...need to initialize OpenGL stuff etc in this callback
  _ <- onRealize canvas $ {-# SCC "cbRz" #-} withGLDrawingArea canvas $ \_ -> do
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho 0.0 1.0 0.0 1.0 (-1.0) 1.0
    GL.drawBuffer $= GL.BackBuffers
    GL.texture GL.Texture2D $= GL.Enabled
    i <- Image.new (oversample * width) (oversample * height)
    writeIORef sR $! GMNDL{ image = i, center = aRe args :+ aIm args, zradius = aZr args, stop = return () }
    restart
  -- when the mouse button is pressed, center and zoom in or out
  _ <- eventb `on` buttonPressEvent $ {-# SCC "cbEv" #-} tryEvent $ do
    b <- eventButton
    (x, y) <- eventCoordinates
    liftIO $ do
      g <- readIORef sR
      let w2 = fromIntegral width  / 2
          h2 = fromIntegral height / 2
          p  = convert x :+ convert (-y)
          s  = (zradius g / (w2 `min` h2)) :+ 0
          c  = center g + (p - (w2 :+ (-h2))) * s
          zradius' = zradius g * delta
          delta | b == LeftButton = 0.5
                | b == RightButton = 2
                | otherwise = 1
      writeIORef sR $! g{ center = c, zradius = zradius' }
      restart
  -- when typing in the coordinate boxes, zoom to the new place
  _ <- statusRe `onEntryActivate` do
    s <- entryGetText statusRe
    liftIO $ do
      g <- readIORef sR
      case safeRead s of
        Just re -> do
          let _ :+ im = center g
          writeIORef sR $! g{ center = re :+ im }
          restart
        Nothing -> return ()
  _ <- statusIm `onEntryActivate` do
    s <- entryGetText statusIm
    liftIO $ do
      g <- readIORef sR
      case safeRead s of
        Just im -> do
          let re :+ _ = center g
          writeIORef sR $! g{ center = re :+ im }
          restart
        Nothing -> return ()
  _ <- statusZr `onEntryActivate` do
    s <- entryGetText statusZr
    liftIO $ do
      g <- readIORef sR
      case safeRead s of
        Just zradius' -> do
          writeIORef sR $! g{ zradius = zradius' }
          restart
        Nothing -> return ()
  -- when pressing return in the ratios list, zoom to that mu-atom
  muQueue <- newEmptyMVar
  _ <- forkIO . forever $ do
    qs <- takeMVar muQueue
    case parameter =<< parse qs of
      Nothing -> postGUISync $ do
        _ <- ratios `widgetSetSensitive` True
        return ()
      Just (cr, ci, radius) -> do
        let zradius' = radius * 3
        cr `seq` ci `seq` zradius' `seq` postGUISync $ do
          g <- readIORef sR
          if isNaN cr || isNaN ci
            then writeIORef sR $! g{ center = c0, zradius = zradius0 }
            else writeIORef sR $! g{ center = cr :+ ci, zradius = zradius' }
          _ <- ratios `widgetSetSensitive` True
          restart
  _ <- ratios `onEntryActivate` do
    s <- entryGetText ratios
    _ <- ratios `widgetSetSensitive` False
    g <- readIORef sR
    stop g
    putMVar muQueue s
  -- time to draw the image: upload to the texture and draw a quad
  _ <- onExpose canvas $ {-# SCC "cbEx" #-} \_ -> do
    withGLDrawingArea canvas $ \glwindow -> do
      GMNDL{ image = i } <- readIORef sR
      Image.upload i
      Image.draw i
      glDrawableSwapBuffers glwindow
    return True
  -- need an exit strategy
  _ <- onDestroy window mainQuit
  -- make sure the expose callback gets called regularly (5fps)
  _ <- timeoutAdd (widgetQueueDraw canvas >> return True) 200
  -- and we're off!
  widgetShowAll window
  mainGUI

-- initial center coordinates
-- using the maximum precision available from the start for this makes
-- sure that nothing weird happens when precision gets close to the edge
c0 :: Complex QuadDouble
c0 = 0

-- initial zoom level
-- the initial zoom level should probably depend on initial image size
zradius0 :: QuadDouble
zradius0 = 2

safeRead :: Read a => String -> Maybe a
safeRead s = case reads s of
  [(a, "")] -> Just a
  _ -> Nothing
