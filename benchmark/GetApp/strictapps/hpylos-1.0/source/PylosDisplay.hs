-- ==================================
-- Module name: PylosDisplay
-- Project: Pylos
-- Copyright (C) 2008  Bartosz Wójcik
-- Created on: 09.06.2008
-- Last update: 06.11.2008
-- Version: %

{-  This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
-- ==================================
-- | This module is a simple interface to Pylos game <http://www.boardgamegeek.com/game/1419>
-- It uses AI module in order to follow game status or provide game between human and computer.
module PylosDisplay (displayPylos)
where

-- =========================================================
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GLUT
import Data.Array ( Array, listArray, array, elems, (!) )
import qualified Data.Map as Map ((!))
import Data.Either
import Data.IORef
import Data.List
import System.Exit
import PylosBoard (Pylos (..),
                   Coordinate,
                   Player (..),
                   putable,
                   takeable,
                   moveable,
                   initPylos,
                   putableForMove,
                   ifTakeAfterPut,
                   isFree,
                   stone2Player,
                   nextPlayer
                   )
import PylosMove (Move (NextMove))
import PylosAI (Algorithm (..),
                evaluate,
                maximise,
                minimise,
                sizeGT,
                drawGT
               )
import PylosEvaluator (Action (..),
                       TileStatus (..),
                       Actions,
                       BoardState,
                       BallsState,
                       PlayerType,
                       State (..),
                       algPlayerType,
                       depPlayerType,
                       brePlayerType,
                       nbrPlayerType,
                       makeState,
                       stateProcessMain,
                       actionTerminate,
                       alreadyFinishedCase,
                       stateProcessSecondary,
                       stateProcess,
                       updateNbrBalls,
                       aiOnMove
                       )
-- =========================================================
-- | Ball's display properity, apart of colour
data BallEmphasize = NoEmphasize
                   | Lighter
                   | Wire
                   | Transparent
                   | Marked
   deriving (Eq,Ord,Enum,Read,Show)

-- | Ball's display full properities.
type BallColour = (Player,BallEmphasize)

-- =========================================================
-- |
-- Prepares colour and flavour of given ball depending on status details.
ballColour :: Pylos                        -- ^ Pylos game status
           -> Coordinate                   -- ^ Coordinates of given ball
           -> Coordinate                   -- ^ Recently put ball
           -> Maybe Coordinate             -- ^ Ball pointed by cursor
           -> (Coordinate, Coordinate)     -- ^ Balls recently taken from board
           -> Actions                      -- ^ current action's state
           -> Bool                         -- ^ whether shadow mode (shadow mode is for balls that are supposed to react on mouse pointer)
           -> BallColour                   -- ^ real colour * flavour to be rendered.
-- ---------------------------------------------------------
ballColour pylos c@(l,(x,y)) aB pB (rB1@(lr,(xr,yr)),rB2) (player,action) shadowMode
     | action == Puts        && isPutableForMove          && ((Just c == pB) || shadowMode) = (player,Lighter)
     | action == PutsOrTakes && (isPutable || isMoveable) && ((Just c == pB) || shadowMode) = (player,Lighter)
     | doesTake              && isTakeable                && ((Just c == pB) || shadowMode) = (player,Lighter)
     |                           (c == rB1 || c == rB2)   && not shadowMode                 = (player,Wire)
     | ifTakeAfterPut pylos player c                      && not shadowMode                 = (player,Marked)
     | isFree c pylos                                     || shadowMode                     = (player,Transparent)
     | c == aB                                                                              = (stone,Lighter)
     | otherwise                                                                            = (stone,NoEmphasize)
   where isPutable = putable pylos c
         isTakeable = takeable player pylos c
         isMoveable = moveable player pylos c
         isPutableForMove = putableForMove lr c pylos
         doesTake = action == Takes1 || action == Takes2
         stone = stone2Player $ board pylos Map.! c
-- =========================================================

-- =========================================================
darkGrey = Color4 0.3 0.3 0.3 1
white    = Color4 1 1 1 1
blue     = Color4 0 0 1 1
-- =========================================================

-- =========================================================
-- |
-- Set of colour definitions.
blackBoard = do
   materialAmbient FrontAndBack $= Color4 0.2 0.2 0.2 1.0
   materialDiffuse FrontAndBack $= Color4 0.15 0.15 0.15 1.0
   materialSpecular FrontAndBack $= Color4 0.11 0.11 0.11 1
   materialShininess FrontAndBack $= 5

whiteBoard = do
   materialAmbient FrontAndBack $= Color4 0.4 0.4 0.4 1.0
   materialDiffuse FrontAndBack $= Color4 0.15 0.15 0.15 1.0
   materialSpecular FrontAndBack $= Color4 0.11 0.11 0.11 1
   materialShininess FrontAndBack $= 5

blackBall = do
   materialAmbient FrontAndBack $= Color4 0.01 0.01 0.01 1.0
   materialDiffuse FrontAndBack $= Color4 0.1 0.1 0.1 1.0
   materialSpecular FrontAndBack $= Color4 0.11 0.11 0.11 1
   materialShininess FrontAndBack $= 5

lightBlackBall = do
   materialAmbient FrontAndBack $= Color4 0.04 0.04 0.04 1.0
   materialDiffuse FrontAndBack $= Color4 0.15 0.15 0.15 1.0
   materialSpecular FrontAndBack $= Color4 0.65 0.65 0.65 1
   materialShininess FrontAndBack $= 5

whiteBall = do
   materialAmbient FrontAndBack $= Color4 0.45 0.45 0.45 1.0
   materialDiffuse FrontAndBack $= Color4 0.25 0.25 0.25 1.0
   materialSpecular FrontAndBack $= Color4 0 0 0 5
   materialShininess FrontAndBack $= 20

lightWhiteBall = do
   materialAmbient FrontAndBack $= Color4 0.58 0.58 0.58 1.0
   materialDiffuse FrontAndBack $= Color4 0.32 0.32 0.32 1.0
   materialSpecular FrontAndBack $= Color4 0 0 0 1
   materialShininess FrontAndBack $= 20

redBall = do
   materialAmbient FrontAndBack $= Color4 0.25 0 0 1.0
   materialDiffuse FrontAndBack $= Color4 0.5 0.05 0.05 1.0
   materialSpecular FrontAndBack $= Color4 0.65 0.65 0.65 1
   materialShininess FrontAndBack $= 5

blackGrid = do
   materialAmbient FrontAndBack $= Color4 0 0 0 1
   materialDiffuse FrontAndBack $= Color4 0 0 0 1
   materialSpecular FrontAndBack $= Color4 0 0 0 1
   materialShininess FrontAndBack $= 1

whiteGrid = do
   materialAmbient FrontAndBack $= Color4 1 1 1 1
   materialDiffuse FrontAndBack $= Color4 1 1 1 1
   materialSpecular FrontAndBack $= Color4 1 1 1 1
   materialShininess FrontAndBack $= 1

goldenBoard = do
   materialAmbient FrontAndBack $= Color4 0.12 0.1 0 1
   materialDiffuse FrontAndBack $= Color4 0.11 0.08 0 1
   materialSpecular FrontAndBack $= Color4 0.37 0.37 0.37 1
   materialShininess FrontAndBack $= 3

violetBall = do
   materialAmbient FrontAndBack $= Color4 0.05 0 0.05 1.0
   materialDiffuse FrontAndBack $= Color4 0.22 0.00 0.22 1.0
   materialSpecular FrontAndBack $= Color4 0.06 0.06 0.06 1
   materialShininess FrontAndBack $= 10   

-- =========================================================

-- =========================================================
-- |
-- This "function" initiates game, opens window, and passes stearing to 'Graphics.Rendering.OpenGL.mainLoop'.
displayPylos :: GLint -> PlayerType -> PlayerType -> Int -> IO ()
-- ---------------------------------------------------------
displayPylos size pl1 pl2 verbose = do
-- =========================================================
   initialDisplayMode $= [DoubleBuffered,WithDepthBuffer]
   win <- createWindow ("Pylos: " ++ header pl1 ++ " vs. " ++ header pl2)
   windowSize $= Size 900 800

   depthFunc $= Just Less
   lighting $= Enabled
   normalize $= Enabled
   position (Light 0) $= Vertex4 0 0 1 0-- 0.2 0.4 (-15) 1
   ambient (Light 0) $= Color4 1 1 1 1
   diffuse (Light 0) $= Color4 1 1 1 1
   specular (Light 0) $= Color4 1 1 1 1
   light (Light 0) $= Enabled
   clearColor $= darkGrey

   ambientCol <- newIORef (Color4 0 0 (0::GLfloat) 1)
   diffuseCol <- newIORef (Color4 0 0 (0::GLfloat) 1)
   specularCol <- newIORef (Color4 0 0 (0::GLfloat) 1)
   shineCol <- newIORef (0::GLfloat)

   aX <- newIORef 0     -- rotation in X axe
   aY <- newIORef 0     -- rotation in Y axe

   state <- makeState size pl1 pl2 verbose

   reshapeCallback $= Just reshape

   passiveMotionCallback $= Just (mousePosition win aX aY state)
   motionCallback $= Just (mouseDrags win aX aY state)
   keyboardMouseCallback $= Just (keyboardMouse win aX aY size state)

   displayCallback $= display aX aY size state

   idleCallback $= Just (idle win state)

   mainLoop

   where header Nothing = "Human"
         header (Just (alg,depth,breadth,nbr)) = show alg ++ " " ++ show depth ++ " " ++ show breadth ++ " " ++ show nbr
-- =========================================================

-- =========================================================
reshape :: Size -> IO ()
-- ---------------------------------------------------------
reshape screenSize@(Size w h) = do
-- =========================================================
   viewport $= ((Position 0 0),screenSize)
   matrixMode $= Projection
   loadIdentity
   myFrustrum w h
   matrixMode $= Modelview 0
-- =========================================================

-- =========================================================
myFrustrum :: (Integral a1, Integral a) => a -> a1 -> IO ()
-- ---------------------------------------------------------
myFrustrum w h = frustum (-right) right (-top) top near far
         where far = 120
               aspect = fromIntegral(w)/fromIntegral(h)
               right = top * aspect
-- =========================================================
near = 10
fov = 0.1
top = near * tan fov
-- =========================================================

-- =========================================================
myModelView :: GLdouble -> GLdouble -> IO ()
-- ---------------------------------------------------------
myModelView x y = do
-- =========================================================
     loadIdentity
     lookAt (Vertex3 0 0.5 15) (Vertex3 0 0 (-100)) (Vector3 0 1 0)
     rotate x (Vector3 1 0 (0::GLdouble))
     rotate y (Vector3 0 0 (1::GLdouble))
-- =========================================================

-- =========================================================
display :: IORef GLdouble -> IORef GLdouble -> GLint -> State -> IO ()
-- ---------------------------------------------------------
display aX aY size state = do
-- =========================================================
     clear [ColorBuffer,DepthBuffer]
     (_, (Size width height)) <- get viewport
     matrixMode $= Projection
     loadIdentity
     myFrustrum width height
     matrixMode $= Modelview 0

     x <- readIORef aX
     y <- readIORef aY
     myModelView x y

     goldenBoard
     displayBoard size state 
     displayBalls state False

     dispStat state $= True

     swapBuffers
-- =========================================================


-- =========================================================
-- |
-- Displays all balls. It does it in very imperative way.
-- 3 dimensional loop over all levels, rows and columns with ball display for each coordinate separatelly.
-- Balls can be displayed as follows. Already put in place are black or white. One that have been put recently
-- is a bit lighter. Ball where cursor points and which can be used for next move (either put as new one or 
-- removed from board) is in Wireframe flavour and black or white colour, corresponding to colour of player who is on move.
-- Recently removed balls are displayed also in Wireframe flavour.
displayBalls :: State     -- ^ Game status
             -> Bool      -- ^ If preserving matrix mode (for mouse pointer selection)
             -> IO ()     -- ^ Displays all balls
-- ---------------------------------------------------------
displayBalls state shadowMode = do
   loadName (Name 1)                                               -- this allows to recognize tile selection
   pyl <- get $ balls state
   aB <- get (aBall state)
   rB <- get (rBall state)
   sB <- get (sBall state)
   pB <- get (pBall state)
   act <- get (action state)
   let s = size pyl
   flip mapM_ [0 .. s-1] $ \l -> do
      withName (Name (fromIntegral l)) $ do
      flip mapM_ [0 .. s-l-1] $ \i -> do
         withName (Name (fromIntegral i)) $ do
         flip mapM_ [0 .. s-l-1] $ \j ->
            withName (Name (fromIntegral j)) $ do
               let c = (l,(i,j))
               let val = ballColour pyl c aB pB rB act shadowMode
               preservingMatrix $ displayBall s c val
-- =========================================================


-- =========================================================
-- |
-- Selects color of given ball and lets it be rendered with given flavour
displayBall :: GLint -> Coordinate -> BallColour -> IO ()
-- ---------------------------------------------------------
displayBall size c (_,Wire) = do
   violetBall
   renderSphere size c Wireframe
displayBall size c (_,Marked) = do
   redBall
   renderSphere size c Solid
displayBall size c (WhitePlayer,NoEmphasize) = do
   whiteBall
   renderSphere size c Solid
displayBall size c (WhitePlayer,Lighter) = do
   lightWhiteBall
   renderSphere size c Solid
displayBall size c (BlackPlayer,NoEmphasize) = do
   blackBall
   renderSphere size c Solid
displayBall size c (BlackPlayer,Lighter) = do
   lightBlackBall
   renderSphere size c Solid
displayBall _ _ _ = return ()
-- =========================================================

-- =========================================================
-- |
-- Renders a ball of given coordinates.
renderSphere :: GLint -> Coordinate -> Flavour -> IO ()
-- ---------------------------------------------------------
renderSphere size (level,(i,j)) material = do
-- =========================================================
   translate (Vector3 x y z)
   renderObject material (Sphere' (rr size) 24 24)
   where x = dXY size level i
         y = dXY size level j
         z = dZ size level
-- =========================================================
-- | Radius of ball depends on size of game.
rr size = 1 / fromIntegral size
-- =========================================================
-- |
-- Each level of balls has to be rendered at different Z-axe position.
-- In order that balls looked like thay were located laying each next level on previous one,
-- each Z-axe position differs of (sqrt 2) from next one.
-- =========================================================
dZ :: GLint -> GLint -> GLdouble
-- ---------------------------------------------------------
dZ size level = (rr size) * (1 + 1.42 * fromIntegral level)
-- =========================================================
dXY :: GLint -> GLint -> GLint -> GLdouble
-- ---------------------------------------------------------
dXY size level i = (rr size) * (2 * (fromIntegral i - fromIntegral (size-1) / 2) + fromIntegral level)
-- =========================================================

-- =========================================================
cuboidCoordinates :: GLint -> (GLint,GLint) -> [(GLfloat, GLfloat, GLfloat)]
-- ---------------------------------------------------------
cuboidCoordinates size (x,y) = [(cX,cY+d,0),(cX+d,cY+d,0),(cX,cY,0),(cX+d,cY,0),
                           (cX,cY+d,-0.05),(cX+d,cY+d,-0.05),(cX,cY,-0.05),(cX+d,cY,-0.05)]
                 where d = 1.8 / fromIntegral size
                       d1 = 2 / fromIntegral size
                       cX = (-1.0) + d1 * fromIntegral x
                       cY = (-1.0) + d1 * fromIntegral y
-- =========================================================

-- =========================================================
displayBoard :: GLint -> State -> IO ()
-- ---------------------------------------------------------
displayBoard size state = do
   loadName (Name 0)                                               -- this allows to recognize tile selection
   withName (Name 0) $ do
      flip mapM_ [0 .. size-1] $ \i -> do
         withName (Name (fromIntegral i)) $ do
         flip mapM_ [0 .. size-1] $ \j ->
            withName (Name (fromIntegral j)) $ do
               val <- get (sboard state ! (0,i,j))
               act <- get $ action state
               if val == PointedTile                               -- colour selection
                  then redBall
                  else if fst act == WhitePlayer
                     then whiteBoard
                     else blackBoard
               (sboard state ! (0,i,j)) $= Tile                     -- next display of board will unmark all tiles, unless some of them get marked by mouse move
               (cuboid.cuboidCoordinates size) (i,j)
-- =========================================================

-- =========================================================
displayText vector text = do
     loadIdentity
--     translate vector
     scale 0.001 0.001 (1::GLfloat)
     currentColor $= white
     blackGrid
     renderString Roman text
-- =========================================================


-- =========================================================
-- |
-- Draws a cuboid where vertices are given in the following order
-- frontLeftTop frontRightTop frontLeftBottom frontRightBottom
-- backLeftTop  backRightTop  backLeftBottom  backRightBottom
cuboid :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
-- ---------------------------------------------------------
cuboid [fLT,fRT,fLB,fRB,bLT,bRT,bLB,bRB] = renderPrimitive Quads $ makeVertices [fLT,fRT,fRB,fLB,
                                                                                 fLB,fRB,bRB,bLB,
                                                                                 bRB,bLB,bLT,bRT,
                                                                                 bLT,bRT,fRT,fLT,
                                                                                 fLT,fLB,bLB,bLT,
                                                                                 fRT,fRB,bLB,bLT]
-- =========================================================


-- =========================================================
-- |
-- Renders list of given points as vectors.
makeVertices :: [(GLfloat,GLfloat,GLfloat)] -> IO ()
-- ---------------------------------------------------------
makeVertices = mapM_ (\(x,y,z)->vertex$Vertex3 x y z)
-- =========================================================

-- =========================================================
-- |
-- KeyUp rotates board arround X axe.
keyboardMouse :: Window
                 -> IORef GLdouble
                 -> IORef GLdouble
                 -> GLint
                 -> State
                 -> Key
                 -> KeyState
                 -> t1
                 -> Position
                 -> IO ()
-- ---------------------------------------------------------
keyboardMouse win aX aY size state (SpecialKey KeyUp) Down _ (Position x y) = do
            readIORef aX >>= \x -> aX $= min 0 (x + 1)
            mousePosition win aX aY state (Position x y)
            postRedisplay (Just win)
-- =========================================================
-- =========================================================
-- |
-- KeyLeft rotates board arround Y axe.
keyboardMouse win aX aY size state (SpecialKey KeyLeft) Down _ (Position x y) = do
            readIORef aY >>= \y -> aY $= y - 1
            mousePosition win aX aY state (Position x y)
--            postRedisplay (Just win)
-- =========================================================
-- =========================================================
-- |
-- KeyRight rotates board arround Y axe.
keyboardMouse win aX aY size state (SpecialKey KeyRight) Down _ (Position x y) = do
            readIORef aY >>= \y -> aY $= y + 1
            mousePosition win aX aY state (Position x y)
--            postRedisplay (Just win)
-- =========================================================
-- =========================================================
-- |
-- KeyDown rotates board arround X axe.
keyboardMouse win aX aY size state (SpecialKey KeyDown) Down _ (Position x y) = do
            readIORef aX >>= \x -> aX $= max (-90) (x - 1)
            mousePosition win aX aY state (Position x y)
            postRedisplay (Just win)
-- =========================================================
-- =========================================================
-- |
-- ESC causes exit.
keyboardMouse win _ _ _ _ (Char '\27') Down _ _ = do
            exitWith ExitSuccess
            destroyWindow win
-- =========================================================
-- =========================================================
-- | Left Mouse Button released.
keyboardMouse _ _ _ _ state (MouseButton LeftButton) Up _ (Position x y) = leftButton state $= Nothing
-- =========================================================
-- | Left Mouse Button couses selection of pointed ball.
keyboardMouse win aX aY size state (MouseButton LeftButton) Down _ (Position x y) = do
   vp@(_, (Size width height)) <- get viewport
   ax <- readIORef aX
   ay <- readIORef aY
   (_, maybeHitRecords) <- getHitRecords bufSize $ do
         preservingMatrix $ do
            matrixMode $= Projection
            loadIdentity
            pickMatrix (fromIntegral x, fromIntegral height - fromIntegral y) (1, 1) vp
            myFrustrum width height
            matrixMode $= Modelview 0
            myModelView ax ay
            withName (Name 0) $ displayBoard size state
            withName (Name 1) $ displayBalls state True
         swapBuffers

   processMouseButton maybeHitRecords state
   leftButton state $= Just (Position x y)

   postRedisplay (Just win)
-- =========================================================

-- =========================================================
keyboardMouse _ _ _ _ _ _ _ _ _ = return ()
-- =========================================================


-- =========================================================
processMouseButton :: Maybe[HitRecord] -> State -> IO ()
-- ---------------------------------------------------------
processMouseButton Nothing _ = putStrLn "selection buffer overflow"
-- ---------------------------------------------------------
processMouseButton (Just hitRecords) state = do
    if hitRecords == []
       then return ()
       else do
            let (t:l:i:j:xs) = [ fromIntegral n | Name n <- namesOfHitRecord $ minimumBy closerObject hitRecords ]
            case t of
               1 -> stateProcessMain state (l,(i,j))
               0 -> stateProcessSecondary state (l,(i,j))
-- =========================================================

-- =========================================================
bufSize :: GLsizei
bufSize = 512
-- =========================================================

-- =========================================================
mouseDrags win aX aY state (Position x y) = do
   prevPosition <- readIORef $ leftButton state
   if prevPosition == Nothing
      then return ()
      else do
           readIORef aX >>= \ax -> aX $= (min 0 $ max (-90) (ax + (fromIntegral y - fromIntegral (pY prevPosition y)) / 2))
           readIORef aY >>= \ay -> aY $= ay + ((fromIntegral x - fromIntegral (pX prevPosition x)) / 2)
   leftButton state $= Just (Position x y)
   mousePosition win aX aY state (Position x y)
   where pX (Just (Position x y)) _ = x
         pX Nothing def = def
         pY (Just (Position x y)) _ = y
         pY Nothing def = def
-- =========================================================

-- =========================================================
mousePosition win aX aY state (Position x y) = do
   vp@(_, (Size width height)) <- get viewport
   ax <- readIORef aX
   ay <- readIORef aY
   pBall state $= Nothing
   pylos <- readIORef $ balls state
   (_, maybeHitRecords) <- getHitRecords bufSize $ do
         preservingMatrix $ do
            matrixMode $= Projection
            loadIdentity
            pickMatrix (fromIntegral x, fromIntegral height - fromIntegral y) (1, 1) vp
            myFrustrum width height
            matrixMode $= Modelview 0
            myModelView ax ay
            withName (Name 0) $ displayBoard (size pylos) state
            withName (Name 1) $ displayBalls state True
         swapBuffers

   aiOnM <- aiOnMove state
   if aiOnM
      then pBall state $= Nothing
      else processCursorPosition maybeHitRecords state
   postRedisplay (Just win)
-- =========================================================

-- =========================================================
processCursorPosition :: Maybe[HitRecord] -> State -> IO ()
-- ---------------------------------------------------------
processCursorPosition Nothing _ = putStrLn "selection buffer overflow"
-- ---------------------------------------------------------
processCursorPosition (Just hitRecords) state = do
   if hitRecords == []
      then return () -- putChar '$'
      else if length names < 3
         then putChar '#'
         else do
            let [t, l, i, j] = [ fromIntegral n | Name n <- namesOfHitRecord $ minimumBy closerObject hitRecords ]
            val <- readIORef $ balls state
            case t of
               0 -> do
                     (sboard state ! (0,i,j)) $= PointedTile                 -- mark tile mouse points on
                     pBall state $=  Just (l,(i,j))                          -- mark ball mouse points on

               1 -> do
                     pBall state $=  Just (l,(i,j))                          -- mark ball mouse points on
               _ -> putStrLn $ "? " ++ show (t,i)
      where names = namesOfHitRecord $ minimumBy closerObject hitRecords
            nbrNames = length names
-- =========================================================

-- =========================================================
namesOfHitRecord :: HitRecord -> [Name]
-- ---------------------------------------------------------
namesOfHitRecord (HitRecord _ _ names) = names
-- =========================================================

-- =========================================================
closerObject (HitRecord z1 _ _) (HitRecord z2 _ _) | z1 < z2   = LT
                                                   | otherwise = GT
-- =========================================================

-- =========================================================
idle win state = do
     (player,act) <- readIORef $ action state
     pyl <- readIORef $ balls state
     pl1 <- readIORef $ player1 state
     pl2 <- readIORef $ player2 state
     nbrB <- readIORef $ nbrBalls state     
     displayed <- readIORef $ dispStat state
     verb <- readIORef $ verbose state
     mvNbr <- readIORef $ moveNbr state
     if displayed && player == WhitePlayer && not (pl1 == Nothing) && (not $ nbrB == (0,0))
        then do
           let evWhite = evaluate (algPlayerType pl1) (depPlayerType pl1) (nbrPlayerType pl1) maximise (brePlayerType pl1) (NextMove pyl ( player) nbrB [])
           if verb > 0
              then putStrLn $ show mvNbr ++ ". WhitePlayer: " ++ show evWhite
              else return ()
           mapM_ (stateProcessMain state) (reverse evWhite)
           if verb > 1
              then putStrLn $ show (sizeGT (depPlayerType pl1) (nbrPlayerType pl1) (brePlayerType pl1) (NextMove pyl ( player) nbrB []))
              else return ()
           postRedisplay (Just win)
        else if displayed && player == BlackPlayer && not (pl2 == Nothing) && (not $ nbrB == (0,0))
           then do
              let evBlack = evaluate (algPlayerType pl2) (depPlayerType pl2) (nbrPlayerType pl2) minimise (brePlayerType pl2) (NextMove pyl ( player) nbrB [])
              if verb > 0
                 then putStrLn $ show mvNbr ++ ". BlackPlayer: " ++ show evBlack
                 else return ()
              mapM_ (stateProcessMain state) (reverse evBlack)
              if verb > 1
                 then do
                      putStrLn $ show (sizeGT (depPlayerType pl2) (nbrPlayerType pl2) (brePlayerType pl2) (NextMove pyl ( player) nbrB []))
--                      putStr $ drawGT (depPlayerType pl2) (nbrPlayerType pl2) (brePlayerType pl2) (NextMove pyl ( player) nbrB [])
                 else return ()
              postRedisplay (Just win)
           else return ()
-- =========================================================

