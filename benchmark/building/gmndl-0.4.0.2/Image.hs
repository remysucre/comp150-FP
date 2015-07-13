{-# LANGUAGE ForeignFunctionInterface #-}

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

module Image (Image(), new, clear, plot, upload, with, draw, putPPM, hPutPPM) where

-- poking bytes into memory is dirty, but it's quick and allows use of
-- other fast functions like memset and easy integration with OpenGL
import Foreign (castPtr, mallocBytes, nullPtr, plusPtr, pokeByteOff, Ptr, Word8)
import Foreign.C (CInt(..), CSize(..))
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL.Raw (glGenerateMipmap, gl_TEXTURE_2D)
import System.IO (Handle, hPutBuf, hPutStr, stdout)

data Image
  = Image
      { pixels :: Ptr Word8
      , width :: Int
      , height :: Int
      , size :: Int
      , texture :: GL.TextureObject
      }

-- these images are RGB only
channels :: Int
channels = 3

-- create a new image
-- note: this needs an OpenGL context
new :: Int -> Int -> IO Image
new w h = do
  p <- mallocBytes $ w * h * channels
  [t] <- GL.genObjectNames 1
  let s = roundUp2 (w `max` h)
      i = Image{ pixels = p, width = w, height = h, size = s, texture = t }
      dim = GL.TextureSize2D (fromIntegral s) (fromIntegral s)
      img = GL.PixelData GL.RGB GL.UnsignedByte nullPtr
  with i $ do
    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGB' dim 0 img
    GL.textureFilter GL.Texture2D $= ((GL.Linear', Just GL.Linear'), GL.Linear')
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)
  clear i
  upload i
  return i

-- clear with white
clear :: Image -> IO ()
clear i = do
  let bytes = width i * height i * channels
  _ <- memset (castPtr (pixels i)) 255 (fromIntegral bytes)
  return ()

-- plot a pixel
plot :: Image -> Int -> Int -> Word8 -> Word8 -> Word8 -> IO ()
plot i x y r g b = do
  let p = pixels i `plusPtr` ((y * width i + x) * channels)
  pokeByteOff p 0 r
  pokeByteOff p 1 g
  pokeByteOff p 2 b

-- upload an image to its texture
upload :: Image -> IO ()
upload i = do
  let pos = GL.TexturePosition2D 0 0
      dim = GL.TextureSize2D (fromIntegral $ width i) (fromIntegral $ height i)
      img = GL.PixelData GL.RGB GL.UnsignedByte (pixels i)
  with i $ do
    GL.texSubImage2D GL.Texture2D 0 pos dim img
    glGenerateMipmap gl_TEXTURE_2D

-- use a texture
-- FIXME TODO preserve the previous texture binding instead of clearing
with :: Image -> IO a -> IO a
with i act = do
  GL.textureBinding GL.Texture2D $= Just (texture i)
  r <- act
  GL.textureBinding GL.Texture2D $= Nothing
  return r

-- draw textured unit quad
draw :: Image -> IO ()
draw i = do
  let v :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> IO ()
      v tx ty vx vy = GL.texCoord (GL.TexCoord2 tx ty) >> GL.vertex (GL.Vertex2 vx vy)
      sx = fromIntegral (width  i) / fromIntegral (size i)
      sy = fromIntegral (height i) / fromIntegral (size i)
  with i $ GL.renderPrimitive GL.Quads $ do
    v 0  sy 0 0
    v 0   0 0 1
    v sx  0 1 1
    v sx sy 1 0

-- save as PPM
putPPM :: Image -> IO ()
putPPM = hPutPPM stdout

hPutPPM :: Handle -> Image -> IO ()
hPutPPM h i = do
  hPutStr h ("P6\n" ++ show (width i) ++ " " ++ show (height i) ++ " 255\n")
  hPutBuf h (pixels i) (width i * height i * channels)

-- round up to nearest power of two
-- this will probably explode when n gets large, but it's only used
-- for OpenGL texture dimensions so you'll run out of memory first
roundUp2 :: Int -> Int
roundUp2 n = head . dropWhile (< n) . iterate (2*) $ 1

-- import standard C library memset for clearing images efficiently
-- previous implementation used pokeArray ... (replicate ...) ...
-- which had a nasty habit of keeping the list around in memory
foreign import ccall unsafe "string.h memset"
  c_memset :: Ptr Word8 -> CInt -> CSize -> IO (Ptr Word8)
memset :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)
memset p w s = c_memset p (fromIntegral w) s
