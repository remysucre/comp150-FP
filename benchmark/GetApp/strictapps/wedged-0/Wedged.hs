-- Wedged (c) 2013,2015 Claude Heiland-Allen <claude@mathr.co.uk> <http://mathr.co.uk>
-- Copyleft: This is a free work, you can copy, distribute, and modify it under
-- the terms of the Free Art License <http://artlibre.org/licence/lal/en/>

module Main (main) where

import           Control.Monad        (guard, liftM2)
import           Control.Monad.Random (MonadRandom, runRand, getRandomR, newStdGen, StdGen)
import           Data.Complex         (Complex((:+)), magnitude, mkPolar)
import           Data.Function        (on)
import           Data.List            (group, groupBy, sortBy, nub, nubBy)
import           Data.Maybe           (mapMaybe, fromJust, listToMaybe)
import           Data.Ord             (comparing)
import           Data.Strict.Tuple    (Pair((:!:)))
import           System.Environment   (withArgs)
import           Data.Array.Unboxed   (UArray, bounds, inRange, ixmap, indices)
import qualified Data.Array.Unboxed   as U
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M
import           Diagrams.Prelude
  hiding (inside, magnitude, appends, clamp, Colour, translate, place, render, e)
import qualified Diagrams.Prelude     as D
import           Data.Colour.SRGB     (sRGB24)
import           Diagrams.Backend.Cairo.Internal (Cairo)
import           Diagrams.Backend.Cairo.CmdLine (defaultMain)

type N = Int
type R = Double
type C = Complex R

data Colour = Red | Yellow | Green | Cyan | Magenta
  deriving (Eq, Ord, Show, Read)

type Label = Int
type Depth = Int
type Size  = Pair Int Int
type Coord = Pair Int Int
type Grid  = UArray Size Int

grid :: [[Cell]] -> Grid
grid css = U.array ((0:!:0),(h1:!:w1))
    [ ((y :!: x),munge c)
    | (y,cs) <- [0..h1] `zip` css
    , (x,c ) <- [0..w1] `zip` cs
    ]
  where
    w1 = length (head css) - 1
    h1 = length css        - 1

elems :: Grid -> [Cell]
elems = map unmunge . U.elems

(!) :: Grid -> Coord -> Cell
a ! i = unmunge (a U.! i)

(//) :: Grid -> [(Coord, Cell)] -> Grid
(//) a = (U.//) a . map (fmap munge)

assocs :: Grid -> [(Coord, Cell)]
assocs = map (fmap unmunge) . U.assocs

data Cell  = Empty | Blocked | Filled !Label !Colour
  deriving (Eq, Ord, Show)

munge :: Cell -> Label
munge Empty = -1
munge Blocked = -2
munge (Filled l Red) = 2 + 16 * l
munge (Filled l Yellow) = 3 + 16 * l
munge (Filled l Green) = 4 + 16 * l
munge (Filled l Cyan) = 5 + 16 * l
munge (Filled l Magenta) = 6 + 16 * l

unmunge :: Label -> Cell
unmunge (-1) = Empty
unmunge (-2) = Blocked
unmunge n = case n `divMod` 16 of
  (l, 2) -> Filled l Red
  (l, 3) -> Filled l Yellow
  (l, 4) -> Filled l Green
  (l, 5) -> Filled l Cyan
  (l, 6) -> Filled l Magenta
  x -> error $ "unmunge: " ++ show (n, x)

isEmpty :: Cell -> Bool
isEmpty Empty = True
isEmpty _ = False

isBlocked :: Cell -> Bool
isBlocked Blocked = True
isBlocked _ = False

isFilled :: Cell -> Bool
isFilled Filled{} = True
isFilled _ = False

colour :: Cell -> Maybe Colour
colour (Filled _ c) = Just c
colour _ = Nothing

label :: Cell -> Maybe Label
label (Filled l _) = Just l
label _ = Nothing

unsafeColour :: Cell -> Colour
unsafeColour (Filled _ c) = c
unsafeColour _ = error "unsafeColour"

data Piece = P{ pid :: !Int, unP :: !Grid } deriving (Show)
instance Eq Piece where p == q = pid p == pid q
instance Ord Piece where p `compare` q = pid p `compare` pid q

pieceColour :: Piece -> Colour
pieceColour = unsafeColour . (! (0 :!: 0)) . unP

colours :: [Colour]
colours = [Red, Yellow, Magenta, Green, Cyan]

rawPieces :: [Piece]
rawPieces
  = mapMaybe (fmap snd . normalize isFilled . P 0 . grid)
  . zipWith ccells colours . paras . lines $ pieceData

ccells :: Colour -> [String] -> [[Cell]]
ccells c hss = map (map (cell c)) hss

pieceData :: String
pieceData = "**\n**\n\n*--\n***\n\n-*-\n***\n\n-**\n**-\n\n****\n"

cell :: Colour -> Char -> Cell
cell c '*' = Filled 0 c
cell _ '-' = Empty
cell _ _ = error "cell"

paras :: [String] -> [[String]]
paras [] = []
paras ls = case break null ls of
  (p, ls') -> p : paras (drop 1 ls')

orientations :: [Piece -> Piece]
orientations =
    [ id
    , reverse' . transpose'
    , mapReverse' . transpose'
    , reverse' . mapReverse'
    , reverse'
    , mapReverse'
    , transpose'
    , reverse' . mapReverse' . transpose'
    ]

onP :: (Grid -> Grid) -> Piece -> Piece
onP f (P i g) = P i (f g)

reverse' :: Piece -> Piece
reverse' = onP vflip

mapReverse' :: Piece -> Piece
mapReverse' = onP hflip

transpose' :: Piece -> Piece
transpose' = onP dflip

vflip :: Grid -> Grid
vflip g =
  let bs@((y0:!:_),(h1:!:_)) = bounds g
      f (y :!: x) = (h1 - (y - y0) :!: x)
  in  ixmap bs f g

hflip :: Grid -> Grid
hflip g =
  let bs@((_ :!: x0),(_ :!: w1)) = bounds g
      f (y :!: x) = (y :!: w1 - (x - x0))
  in  ixmap bs f g

dflip :: Grid -> Grid
dflip g =
  let ((y0 :!: x0),(h1 :!: w1)) = bounds g
      f (y :!: x) = (x :!: y)
  in  ixmap ((x0 :!: y0),(w1 :!:h1)) f g

pieces :: [Piece]
pieces = zipWith P [0..] . nub . map unP . liftM2 o rawPieces $ orientations
  where o q@(P _ _) f = snd . fromJust . normalize isFilled $ f q

data Board = B
  { unB :: !Grid
  , topLeft_isEmpty :: !(Maybe Coord)
  , colour_counts :: !(Map Colour Int)
  }
  deriving (Eq, Ord, Show)

mkB :: Grid -> Board
mkB g = B
  { unB = g
  , topLeft_isEmpty = topLeft isEmpty g
  , colour_counts = M.fromList (colours `zip` repeat 0)
  }

rectangle :: Size -> Board
rectangle (h :!: w) = mkB $ U.listArray ((0 :!: 0),(h-1 :!: w-1)) (repeat (-1))

place :: Coord -> Label -> Piece -> Board -> [Board]
place yx l piece board
  | fits yx piece board = [blit yx l piece board]
  | otherwise = []

(==>) :: Bool -> Bool -> Bool
x ==> y = if x then y else True
infix 1 ==>

(=/>) :: Bool -> Bool -> Bool
x =/> y = if x then y else False
infix 1 =/>

surround :: Piece -> [Coord]
surround = (surrounds M.!)

surrounds :: Map Piece [Coord]
surrounds = M.fromList [(p, surround' p) | p <- pieces]

surround' :: Piece -> [Coord]
surround' (P _ piece) = nub
  [ vu
  | yx@(y :!: x) <- indices piece
  , isFilled (piece ! yx)
  , vu <- [(y-1 :!: x),(y+1 :!: x), (y :!: x-1), (y :!: x+1)]
  , inRange (bounds piece) vu ==> isEmpty (piece ! vu)
  ]

fits :: Coord -> Piece -> Board -> Bool
fits (y :!: x) p@(P _ piece) (B board _ cc)
  = inside bp bb &&
    cc M.! pc < hi &&
    and [ isEmpty (board ! (v+y :!: u+x))
        | vu@(v :!: u) <- indices piece
        , isFilled (piece ! vu) ] &&
    all distinct
        [ board ! yx
        | (v :!: u) <- surround p
        , let yx = (v+y :!: u+x)
        , inRange bb yx ] &&
    (pc == Cyan ==> case bp of
      ((0:!:0),(3:!:0)) -> not (blocked (y - 1 :!: x) || blocked (y + 4 :!: x))
      ((0:!:0),(0:!:3)) -> not (blocked (y :!: x - 1) || blocked (y :!: x + 4))
      _ -> error "fits")
  where
    bb@((y0 :!: x0), (h1 :!: w1)) = bounds board
    bp = bounds piece
    h = h1 - y0 + 1
    w = w1 - x0 + 1
    n :: Double
    n = fromIntegral (h * (w - 1)) / fromIntegral (4 * length colours)
    md = 4 * round n
    hi = md + 4
    pc = pieceColour p
    distinct = (Just pc /=) . colour
    blocked yx = inRange bb yx =/> isBlocked (board ! yx)
    inside ((ly :!: lx),(hy :!: hx)) ((lv :!: lu),(hv :!: hu))
      = lv <= (ly+y) && (hy+y) <= hv && lu <= (lx+x) && (hx+x) <= hu

blit :: Coord -> Label -> Piece -> Board -> Board
blit (y :!: x) l p@(P _ piece) (B board (Just (ty :!: tx)) cc) =
    B board' (topLeftFrom ty tx isEmpty board') cc'
  where
    cc' = M.adjust (4 +) (pieceColour p) cc
    board' = board // [ (yx, blit1 l (piece ! vu) (board ! yx))
                      | vu@(v :!: u) <- indices piece, let yx = (y + v :!: x + u)
                      ]
blit _ _ _ _ = error "blit"

blit1 :: Label -> Cell -> Cell -> Cell
blit1 l (Filled _ c) Empty = Filled l c
blit1 _ Empty x = x
blit1 _ x y = error $ "blit1" ++ show (x, y)

topLeft :: (Cell -> Bool) -> Grid -> Maybe Coord
topLeft p a = listToMaybe [ i | i <- indices a, p $ a ! i ]

topLeftFrom :: Int -> Int -> (Cell -> Bool) -> Grid -> Maybe Coord
topLeftFrom ty tx p a = go ty tx
  where
    ((_ :!: x0),(h0 :!: w0)) = bounds a
    go y x
      | y > h0 = Nothing
      | x > w0 = go (y + 1) x0
      | p (a ! yx) = Just yx
      | otherwise = go y (x + 1)
      where yx = (y :!: x)

normalize :: (Cell -> Bool) -> Piece -> Maybe (Coord, Piece)
normalize p (P i piece) = do
  (y :!: x) <- topLeft p piece
  return ((y :!: x), translate (-y :!: -x) (P i piece))

translate :: Coord -> Piece -> Piece
translate (y :!: x) (P i g) = P i (ixmap bs (\(v :!: u) -> (v - y :!: u - x)) g)
  where
    ((y0 :!: x0),(h1 :!: w1)) = bounds g
    bs = ((y0 + y :!: x0 + x), (h1 + y :!: w1 + x))

fill :: Depth -> [Piece] -> Board -> [Board]
fill 0 _ board = do
  guard $ colourCounts board
  guard $ lineLengths board
  return board
fill d piecesm board = do
  Just yx <- return $ topLeft_isEmpty board
  piece <- piecesm
  board' <- place yx (d - 1) piece board
  guard $ diverse board'
  fill (d - 1) piecesm board'

colourCounts :: Board -> Bool
colourCounts b = all (lo <=) cs && any (== md) cs
  where
    cs = M.elems (colour_counts b)
    ((y0 :!: x0), (h1 :!: w1)) = bounds (unB b)
    h = h1 - y0 + 1
    w = w1 - x0 + 1
    n :: Double
    n = fromIntegral (h * (w - 1)) / fromIntegral (4 * length colours)
    lo = md - 4
    md = 4 * round n

lineLengths :: Board -> Bool
lineLengths (B g _ _) = all (<= l) . concatMap (map length . group) $ hs ++ vs
  where
    hs = [ [ g ! (y :!: x) == g ! (y+1 :!: x) | x <- [x0..w1] ] | y <- [y0 .. h1-1] ]
    vs = [ [ g ! (y :!: x) == g ! (y :!: x+1) | y <- [y0..h1] ] | x <- [x0 .. w1-1] ]
    ((y0 :!: x0), (h1 :!: w1)) = bounds g
    w = w1 - x0 + 1
    l = w - 2

depth :: Board -> Maybe Depth
depth g
  | 0 == n `mod` 4 = Just (n `div` 4)
  | otherwise = Nothing
  where
    n = length . filter isEmpty . elems . unB $ g

packings :: [Piece] -> Board -> [Board]
packings piecesm board = maybe [] (\d -> fill d piecesm board) (depth board)

blockings :: Board -> [Board]
blockings (B board _ _) =
    blockings' (x0 - 200) (x0 - 100) y0 m0 board
  where
    ((y0 :!: x0), (h1 :!: w1)) = bounds board
    h = h1 - y0 + 1
    w = w1 - x0 + 1
    m0 = M.fromList [ (x, n) | x <- [x0 .. w1] ]
    n = ((h - 1) `div` w) + 1
    blockings' x'' x' y m b
      | y > h1 = if all (< n) (M.elems m) then return (mkB b) else []
      | otherwise = do
          let a x = abs (x - x') > 2 && abs (x - x'') > 2
          x <- M.keys $ M.filterWithKey (\x n' -> a x && n' > 0) m
          let b' = b // [((y:!:x), Blocked)]
              m' = M.adjust (subtract 1) x m
          blockings' x' x (y + 1) m' b'

diverse :: Board -> Bool
diverse (B b k _) = case k of
    Nothing -> d (row h1) && all d cols
    Just (ty :!: _) | ty > y0 -> d (row (ty - 1))
    _ -> True
  where
    row y1 = [ colour $ b ! (y1 :!: x) | x <- [x0 .. w1] ]
    cols   = [ [ colour $ b ! (y :!: x) | y <- [y0 .. h1] ] | x <- [x0 .. w1] ]
    d = (5 <=) . length . nub
    ((y0 :!: x0), (h1 :!: w1)) = bounds b

main :: IO ()
main = main' (13 :!: 9)

main' :: Size -> IO ()
main'
  = mapM_ (uncurry putDiagram) . zip [0..] . map unB
  . concatMap (nubBy (equivalentBy ((==) `on` colour)) . packings pieces)
  . nubBy equivalent . blockings . rectangle

equivalent :: Board -> Board -> Bool
equivalent = equivalentBy (==)

equivalentBy :: (Cell -> Cell -> Bool) -> Board -> Board -> Bool
equivalentBy ceq (B a _ _) (B b _ _) =
    a `eq` b || a `eq` vflip b || a `eq` hflip b || a `eq` hflip (vflip b)
  where
    eq p q = bounds p == bounds q && and (zipWith ceq (elems p) (elems q))

putDiagram :: Int -> Grid -> IO ()
putDiagram n g = do
  withArgs ["-w", "1287", "-h", "1795", "-o", show3 n ++ ".png"] $ do
    defaultMain . fst . render g =<< newStdGen
  where
    show3 i
      | i < 0 = show i
      | i < 10 = "00" ++ show i
      | i < 100 = "0" ++ show i
      | otherwise =      show i

render :: Grid -> StdGen -> (Diagram Cairo R2, StdGen)
render g = runRand $ do
  cs <- mapM renderCells $ pieceCells g
  return $
    withEnvelope' e (mconcat cs) # centerXY `atop`
    rect 10.9 15.2 # centerXY # fc white # lcA transparent
  where
    e = fromVertices [ p2(-1,-1), p2(-1,13), p2(9,13), p2(9,-1) ]
    withEnvelope' a b = withEnvelope (a `asTypeOf` b) b

pieceCells :: Grid -> [[(Coord, Cell)]]
pieceCells
  = map (sortBy (comparing fst))
  . groupBy ((==) `on` (label . snd))
  . sortBy (comparing (label . snd))
  . assocs

renderCells :: (Functor m, MonadRandom m) => [(Coord, Cell)] -> m (Diagram Cairo R2)
renderCells ((j :!: i, Filled _ Red):_) =
    (draw True (2^wdepth) (rgb Red) . (:[])) `fmap` appendsM [ w a b, w b c, w c d, w d a ]
  where
    wdepth :: N
    wdepth = 4
    w = wobble wdepth
    a = fromIntegral i :+ fromIntegral j
    b = fromIntegral i :+ fromIntegral (j + 1)
    c = fromIntegral (i + 1) :+ fromIntegral (j + 1)
    d = fromIntegral (i + 1) :+ fromIntegral j
renderCells [(j0:!:i0, Filled _ Yellow),(j1:!:i1,_),(j2:!:i2,_),(j3:!:i3,_)] =
    (draw False (2^wdepth) (rgb Yellow) . (:[])) `fmap` appendsM ws
  where
    wdepth :: N
    wdepth = 4
    w = wobble wdepth
    a = fromIntegral i0 :+ fromIntegral j0
    b = fromIntegral i1 :+ fromIntegral j1
    c = fromIntegral i2 :+ fromIntegral j2
    d = fromIntegral i3 :+ fromIntegral j3
    ws = case (j1 - j0, i1 - i0, j2 - j0, i2 - i0, j3 - j0, i3 - i0) of
      (0, 1, 0, 2, 1, 2) ->  {-   --,  -} [ w a b, w b c, w c d ]
      (1, 0, 2,-1, 2, 0) ->  {-  ,|    -} [ w a b, w b d, w d c ]
      (1, 0, 1, 1, 1, 2) ->  {-   '--  -} [ w a b, w b c, w c d ]
      (0, 1, 1, 0, 2, 0) ->  {-   |'   -} [ w b a, w a c, w c d ]
      (0, 1, 0, 2, 1, 0) ->  {-   ,--  -} [ w d a, w a b, w b c ]
      (0, 1, 1, 1, 2, 1) ->  {-   '|   -} [ w a b, w b c, w c d ]
      (1,-2, 1,-1, 1, 0) ->  {- --'    -} [ w b c, w c d, w d a ]
      (1, 0, 2, 0, 2, 1) ->  {-   |,   -} [ w a b, w b c, w c d ]
      x -> error $ "yellow" ++ show x
renderCells [(j0:!:i0, Filled _ Green),(j1:!:i1,_),(j2:!:i2,_),(j3:!:i3,_)] =
    (draw False (2^wdepth) (rgb Green) . (:[])) `fmap` appendsM ws
  where
    wdepth :: N
    wdepth = 4
    w = wobble wdepth
    a = fromIntegral i0 :+ fromIntegral j0
    b = fromIntegral i1 :+ fromIntegral j1
    c = fromIntegral i2 :+ fromIntegral j2
    d = fromIntegral i3 :+ fromIntegral j3
    ws = case (j1 - j0, i1 - i0, j2 - j0, i2 - i0, j3 - j0, i3 - i0) of
      (0, 1, 1,-1, 1, 0) ->  {-  _|'   -} [ w c d, w d a, w a b ]
      (0, 1, 1, 1, 1, 2) ->  {-   '|_  -} [ w a b, w b c, w c d ]
      (1, 0, 1, 1, 2, 1) ->  {-   ',   -} [ w a b, w b c, w c d ]
      (1,-1, 1, 0, 2,-1) ->  {-  ,'    -} [ w a c, w c b, w b d ]
      x -> error $ "green" ++ show x
renderCells [(j0:!:i0, Filled _ Cyan),(j1:!:i1,_),(j2:!:i2,_),(j3:!:i3,_)] =
    (draw False (2^wdepth) (rgb Cyan) . (:[])) `fmap` appendsM [ w a b, w b c, w c d ]
  where
    wdepth :: N
    wdepth = 4
    w = wobble wdepth
    a = fromIntegral i0 :+ fromIntegral j0
    b = fromIntegral i1 :+ fromIntegral j1
    c = fromIntegral i2 :+ fromIntegral j2
    d = fromIntegral i3 :+ fromIntegral j3
renderCells [(j0:!:i0, Filled _ Magenta),(j1:!:i1,_),(j2:!:i2,_),(j3:!:i3,_)] =
    draw False (2^wdepth) (rgb Magenta) `fmap` mapM appendsM wss
  where
    wdepth :: N
    wdepth = 4
    w = wobble wdepth
    a = fromIntegral i0 :+ fromIntegral j0
    b = fromIntegral i1 :+ fromIntegral j1
    c = fromIntegral i2 :+ fromIntegral j2
    d = fromIntegral i3 :+ fromIntegral j3
    wss = case (j1 - j0, i1 - i0, j2 - j0, i2 - i0, j3 - j0, i3 - i0) of
      (1,-1, 1, 0, 1, 1) ->  {-  _|_   -} [ [ w a c ], [ w b c, w c d ] ]
      (0, 1, 0, 2, 1, 1) ->  {-   -|-  -} [ [ w b d ], [ w a b, w b c ] ]
      (1, 0, 1, 1, 2, 0) ->  {-   |-   -} [ [ w b c ], [ w a b, w b d ] ]
      (1,-1, 1, 0, 2, 0) ->  {-  -|    -} [ [ w b c ], [ w a c, w c d ] ]
      x -> error $ "magenta" ++ show x
renderCells _ = return mempty

perturbMidpoint :: MonadRandom m => C -> C -> m C
perturbMidpoint p q = do
  let m0 = (p + q) / 2
      r1 = magnitude (p - q) / 16
  t <- getRandomR (-pi, pi)
  r <- getRandomR (0, r1)
  return $! m0 + mkPolar r t

append :: (R -> t) -> (R -> t) -> R -> t
append f g t
  | t < 0.5   = f $! 2 * t
  | otherwise = g $! 2 * t - 1

appends :: [(R -> t)] -> R -> t
appends fs t = fs !! ti $ tx
  where
    l = length fs
    t' = t * fromIntegral l
    ti = clamp (floor t') 0 (l - 1)
    tx = t' - fromIntegral ti

appendsM :: (Functor m, Monad m) => [m (R -> t)] -> m (R -> t)
appendsM fs = appends `fmap` sequence fs

wobble :: MonadRandom m => N -> C -> C -> m (R -> C)
wobble 0 p q = return $ lint p q
wobble n p q = do
  r <- perturbMidpoint p q
  pr <- wobble (n - 1) p r
  rq <- wobble (n - 1) r q
  return $ pr `append` rq

lint :: C -> C -> R -> C
lint p q t = c (1 - t) * p + c t * q where c r = r :+ 0

clamp :: Ord t => t -> t -> t -> t
clamp x lo hi = lo `max` x `min` hi

draw :: Bool -> N -> D.Colour R -> [(R -> C)] -> Diagram Cairo R2
draw cl m c fs = (plot 0 # fc c `atop` plot 0.05 # fc black) # lcA transparent
  where m' = 1 / fromIntegral m
        ps = [ cubicSpline cl
          [ p2(x,y)
          | i <- [0 .. if cl then m - 1 else m]
          , let t = fromIntegral i * m'
          , let x:+y = f t
          ] | f <- fs ]
        plot k = mconcat
          [ circle (0.1 + k) # D.translate (r2 . unp2 $ s `atParam` t)
          | p <- ps
          , s <- concat $ fixPath p
          , i <- [0 .. if cl then m - 1 else m]
          , let t = fromIntegral i * m'
          ]

rgb :: Colour -> D.Colour R
rgb Red     = sRGB24 205  63 125
rgb Yellow  = sRGB24 213 135  54
rgb Green   = sRGB24  58 110  70
rgb Cyan    = sRGB24  56 138 170
rgb Magenta = sRGB24 100  70 124
