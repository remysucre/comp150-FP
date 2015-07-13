{-# LANGUAGE BangPatterns #-}

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

module Calculate (convert, renderer) where

-- simple helpers
import Control.Monad (when)

-- concurrent renderer with capability-specific scheduling
import Control.Concurrent (MVar, newEmptyMVar, takeMVar, tryTakeMVar, tryPutMVar, threadDelay, forkIO, killThread)
import GHC.Conc (forkOn, numCapabilities)

-- each worker uses a mutable unboxed array of Bool to know which pixels
-- it has already started to render, to avoid pointless work duplication
import Data.Array.IO (IOUArray, newArray, readArray, writeArray, inRange)

-- each worker thread keeps a queue of pixels that it needs to render or
-- to continue rendering later
import Data.PriorityQueue (PriorityQueue, newPriorityQueue, enqueue, enqueueBatch, dequeue)

-- poking bytes into memory is dirty, but it's quick and allows use of
-- other fast functions like memset and easy integration with OpenGL
import Foreign (Word8)

-- higher precision arithmetic using libqd
import Numeric.QD.DoubleDouble (DoubleDouble())
import Numeric.QD.QuadDouble (QuadDouble())

import Complex (Complex((:+)), Turbo(sqr, twice), convert)

-- some type aliases to shorten things
type B = Word8
type N = Int
type R = Double

{-
-- colour space conversion from HSV [0..1] to RGB [0..1]
-- HSV looks quite 'chemical' to my eyes, need to investigate something
-- better to make it feel more 'natural'
hsv2rgb :: R -> R -> R -> (R, R, R)
hsv2rgb !h !s !v
  | s == 0 = (v, v, v)
  | h == 1 = hsv2rgb 0 s v
  | otherwise =
      let !i = floor (h * 6) `mod` 6 :: N
          !f = (h * 6) - fromIntegral i
          !p = v * (1 - s)
          !q = v * (1 - s * f)
          !t = v * (1 - s * (1 - f))
      in  case i of
            0 -> (v, t, p)
            1 -> (q, v, p)
            2 -> (p, v, t)
            3 -> (p, q, v)
            4 -> (t, p, v)
            5 -> (v, p, q)
            _ -> (0, 0, 0)
-}

hsv2rgb' :: R -> R -> R -> (R, R, R)
hsv2rgb' !h !s !l =
  let !a = 2 * pi * h
      !ca = cos a
      !sa = sin a
      !y = l / 2
      !u = s / 2 * ca
      !v = s / 2 * sa
      !r = y + 1.407 * u
      !g = y - 0.677 * u - 0.236 * v
      !b = y + 1.848 * v
  in  (r, g, b)

-- compute RGB [0..255] bytes from the results of the complex iterations
-- don't need very high precision for this, as spatial aliasing will be
-- much more of a problem in intricate regions of the fractal
colour :: Complex Double -> Complex Double -> N -> (B, B, B)
colour !(zr:+zi) !(dzr:+dzi) !n =
  let -- micro-optimization - there is no log2 function
      !il2 = 1 / log 2
      !zd2 = sqr zr + sqr zi
      !dzd2 = sqr dzr + sqr dzi
      -- normalized escape time
      !d = (fromIntegral n :: R) - log (log zd2 / log escapeR2) * il2
      !dwell = fromIntegral (floor d :: N)
      -- final angle of the iterate
      !finala = atan2 zi zr
      -- distance estimate
      !de = (log zd2 * il2) * sqrt (zd2 / dzd2)
      !dscale = -log de * il2
      -- HSV is based on escape time, distance estimate, and angle
      !hue = log ((- log de * il2) `max` 1) * il2 / 16 + log d * il2
      !saturation = 0 `max` (log d * il2 / 8) `min` 1
      !value = 0 `max` (1 - dscale / 256) `min` 1
      !h = hue - fromIntegral (floor hue :: N)
      -- adjust saturation to give concentric striped pattern
      !k = dwell / 2
      !satf = if k - fromIntegral (floor k :: N) >= (0.5 :: R) then 0.9 else 1
      -- adjust value to give tiled pattern
      !valf = if finala < 0 then 0.9 else 1
      -- convert to RGB
      (!r, !g, !b) = hsv2rgb' h (satf * saturation) (valf * value)
      -- convert to bytes
      !rr = floor $ 0 `max` (255 * r) `min` 255
      !gg = floor $ 0 `max` (255 * g) `min` 255
      !bb = floor $ 0 `max` (255 * b) `min` 255
  in  (rr, gg, bb)

-- a Job stores a pixel undergoing iterations
data Job c = Job !N !N !(Complex c) !(Complex c) !(Complex c) !N

-- the priority of a Job is how many iterations have been computed:
-- so 'fresher' pixels drop to the front of the queue in the hope of
-- avoiding too much work iterating pixels that will never escape
priority :: Job c -> N
priority !(Job _ _ _ _ _ n) = n

-- add a job to a work queue, taking care not to duplicate work
-- there is no race condition here as each worker has its own queue
addJob :: (Real c, Floating c, Turbo c) => N -> N -> Complex c -> c -> PriorityQueue IO (Job c) -> IOUArray (N,N) Bool -> N -> N -> IO ()
addJob !w !h !c !zradius' todo sync !i !j = do
  already <- readArray sync (j, i)
  when (not already) $ do
    writeArray sync (j, i) True
    enqueue todo $! Job i j (coords w h c zradius' i j) 0 0 0

-- spawns a new batch of workers to render an image
-- returns an action that stops the rendering
renderer' :: (Turbo c, Real c, Floating c) => MVar () -> ((N,N),(N,N)) -> (N -> N -> B -> B -> B -> IO ()) -> Complex c -> c -> IO (IO ())
renderer' done rng output !c !zradius' = do
  wdog <- newEmptyMVar
  workerts <- mapM (\w -> forkOn w $ worker wdog rng c zradius' output w) [ 0 .. workers - 1 ]
  watcher <- forkIO $ do
    () <- takeMVar wdog
    let loop = do
          threadDelay 10000000 -- 10 seconds
          m <- tryTakeMVar wdog
          case m of
            Nothing -> mapM_ killThread workerts >> tryPutMVar done () >> return ()
            Just () -> loop
    loop
  return $ killThread watcher >> mapM_ killThread workerts

-- compute the Complex 'c' coordinate for a pixel in the image
coords :: (Real c, Floating c, Turbo c) => N -> N -> Complex c -> c -> N -> N -> Complex c
coords !w !h !c !zradius' !i !j = c + ( (fromIntegral (i - w`div`2) * k)
                                      :+(fromIntegral (h`div`2 - j) * k))
  where !k = zradius' / (fromIntegral $ (w `div` 2) `min` (h `div` 2))

-- the worker thread enqueues its border and starts computing iterations
worker :: (Turbo c, Real c, Floating c) => MVar () -> ((N,N),(N,N)) -> Complex c -> c -> (N -> N -> B -> B -> B -> IO ()) -> N -> IO ()
worker wdog rng@((y0,x0),(y1,x1)) !c !zradius' output !me = do
  sync <- newArray rng False
  queue <- newPriorityQueue priority
  let addJ = addJob w h c zradius' queue sync
      js = filter mine (border w h)
      w = x1 - x0 + 1
      h = y1 - y0 + 1
  mapM_ (flip (writeArray sync) True) js
  enqueueBatch queue (map (\(j,i) -> Job i j (coords w h c zradius' i j) 0 0 0) js)
  compute wdog rng addJ output queue
  where mine (_, i) = i `mod` workers == me -- another dependency on spread

-- the compute engine pulls pixels from the queue until there are no
-- more, and calculates a batch of iterations for each
compute :: (Turbo c, Real c, Floating c) => MVar () -> ((N,N),(N,N)) -> (N -> N -> IO ()) -> (N -> N -> B -> B -> B -> IO ()) -> PriorityQueue IO (Job c) -> IO ()
compute wdog rng addJ output queue = do
  mjob <- dequeue queue
  case mjob of
    Just (Job i j c z dz n) -> do
      let -- called when the pixel escapes
          done' !(zr:+zi) !(dzr:+dzi) !n' = {-# SCC "done" #-} do
            _ <- tryPutMVar wdog ()
            let (r, g, b) = colour (convert zr :+ convert zi) (convert dzr :+ convert dzi) n'
            output i j r g b
            -- a wavefront of computation spreads to neighbouring pixels
            sequence_
              [ addJ x y
              | u <- spreadX
              , v <- spreadY
              , let x = i + u
              , let y = j + v
              , inRange rng (y, x)
              ]
          -- called when the pixel doesn't escape yet
          todo' !z' !dz' !n' = {-# SCC "todo" #-} {- output i j 255 0 0 >> -} enqueue queue $! Job i j c z' dz' n'
      calculate c limit z dz n done' todo'
      compute wdog rng addJ output queue
    Nothing -> return () -- no pixels left to render, so finish quietly

-- the raw z->z^2+c calculation engine
-- also computes the derivative for distance estimation calculations
-- this function is crucial for speed, too much allocation will slooow
-- everything down severely
calculate :: (Turbo c, Real c, Floating c) => Complex c -> N -> Complex c -> Complex c -> N -> (Complex c -> Complex c -> N -> IO ()) -> (Complex c -> Complex c -> N -> IO ()) -> IO ()
calculate !c !m0 !z0 !dz0 !n0 done todo = go m0 z0 dz0 n0
  where
    go !m !z@(zr:+zi) !dz !n
      | not (sqr zr + sqr zi < er2) = done z dz n
      | m <= 0 = todo z dz n
      | otherwise = go (m - 1) (sqr z + c) (let !zdz = z * dz in twice zdz + 1) (n + 1)
    !er2 = convert escapeR2

-- dispatch to different instances of renderer depending on required precision
-- if zoom is low, single precision Float is ok, but as soon as pixel spacing
-- gets really small, it's necessary to increase it
-- it's probably not even worth using Float - worth benchmarking this and
-- also the DD and QD types (which cause a massively noticeable slowdown)
renderer :: (Real c, Floating c) => MVar () -> ((N,N),(N,N)) -> (N -> N -> B -> B -> B -> IO ()) -> Complex c -> c -> IO (IO ())
renderer done rng output !c !zradius'
  | zoom' < 20  = {-# SCC "rF"  #-} renderer' done rng output (f c :: Complex Float       ) (g zradius')
  | zoom' < 50  = {-# SCC "rD"  #-} renderer' done rng output (f c :: Complex Double      ) (g zradius')
  | zoom' < 100 = {-# SCC "rDD" #-} renderer' done rng output (f c :: Complex DoubleDouble) (g zradius')
  | otherwise   = {-# SCC "rQD" #-} renderer' done rng output (f c :: Complex QuadDouble  ) (g zradius')
  where f !(cr :+ ci) = convert cr :+ convert ci
        g !x = convert x
        zoom' = - logBase 2 (zradius' / (fromIntegral $ w `min` h))
        ((x0,y0), (x1, y1)) = rng
        w = x1 - x0 + 1
        h = y1 - y0 + 1

-- start rendering pixels from the edge of the image
-- the Mandelbrot Set and its complement are both simply-connected
-- discounting spatial aliasing any point inside the boundary that is
-- in the complement is 'reachable' from a point on the boundary that
-- is also in the complement - probably some heavy math involved to
-- prove this though
-- note: this implicitly depends on the spread values below - it's
-- necessary for each interlaced subimage (one per worker) to have
-- at least a one pixel deep border
border :: N -> N -> [(N, N)]
border !w !h = concat $
  [ [ (j, i) | i <- [ 0 .. w - 1 ], j <- [ 0 ] ]
  , [ (j, i) | j <- [ 0 .. h - 1 ], i <- [ 0 .. workers - 1 ] ]
  , [ (j, i) | j <- [ 0 .. h - 1 ], i <- [ w - workers .. w - 1 ] ]
  , [ (j, i) | i <- [ 0 .. w - 1 ], j <- [ h - 1 ] ]
  ]

-- which neighbours to activate once a pixel has escaped
-- there are essentially two choices, with x<->y swapped
-- choose greater X spread because images are often wider than tall
-- other schemes wherein the spread is split in both directions
-- might benefit appearance with large worker count, but too complicated
spreadX, spreadY :: [ N ]
spreadX = [ -workers, 0, workers ]
spreadY = [ -1, 0, 1 ]

-- number of worker threads
-- use as many worker threads as capabilities, with the workers
-- distributed 1-1 onto capabilities to maximize CPU utilization
workers :: N
workers = numCapabilities

-- iteration limit per pixel
-- at most this many iterations are performed on each pixel before it
-- is shunted to the back of the work queue
-- this should be tuneable to balance display updates against overheads
limit :: N
limit = (2^(13::N)-1)

-- escape radius for fractal iteration calculations
-- once the complex iterate exceeds this, it's never coming back
-- theoretically escapeR = 2 would work
-- but higher values like this give a significantly smoother picture
escapeR, escapeR2 :: R
escapeR = 65536
escapeR2 = escapeR * escapeR
