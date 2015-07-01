{-# LANGUAGE BangPatterns,
             FlexibleContexts,
             PatternGuards,
             ScopedTypeVariables,
             CPP
 #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Exception(throwIO)
import Control.Monad.State.Strict
import Data.Char(isSpace)
import Data.ByteString.Char8(unpack,pack,ByteString)
import qualified Data.ByteString.Char8 as B
import Data.CSV.Enumerator
import qualified Data.Enumerator as E
import Data.Enumerator (($$))
import Data.Enumerator.Binary (enumHandle)
import Data.Function(on)
import Data.List(insertBy, intercalate, mapAccumL, sortBy, transpose, find)
import Data.Maybe(fromMaybe, isNothing)
import Data.Monoid
import Safe
import System.Exit
import System.IO
import Text.Regex.TDFA
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import qualified Algorithms.NaturalSort as NS
import System.Console.ParseArgs
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
#else
import System.Posix.Signals
#endif

-- Util
--
{-
{-# NOINLINE myTrace #-}
myTrace msg x = unsafePerformIO $ appendFile "myTrace" (msg ++ "\n") >> return x
myTraceIt msg x = myTrace (msg++show x) x
-}

instance NS.NaturalSort ByteString where
    sortKey = NS.sortKey . unpack
    sortKeyCollated = (. unpack) . NS.sortKeyCollated

killSpace :: String -> String
killSpace = dropWhile isSpace . dropEndWhile isSpace
dropEndWhile :: (a -> Bool) -> [a] -> [a]
dropEndWhile p = foldr (\x xs -> if p x && null xs then [] else x:xs) []

-- Uses natural sort to handle numbers and strings.
--

type SortCol = (Int, Bool)
multiColSort :: NS.NaturalSort a => [SortCol] -> [[a]] -> [[a]]
multiColSort sCols rows = sortBy cmp rows
  where cmp r1 r2 = foldr f EQ sCols
          where f (idx, asc) dflt = case NS.compare <$> atMay r1' idx <*> atMay r2' idx of
                                      Nothing -> dflt
                                      Just EQ -> dflt
                                      Just ordering -> ordering
                  where (r1', r2') | asc = (r1, r2)
                                   | otherwise = (r2, r1)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n x xs = prev ++ x:next
  where (prev,next) = second (drop 1) $ splitAt n xs

removeNth :: Int ->  [a] -> [a]
removeNth n = uncurry (++) . second (drop 1) . splitAt n

updateSortCols :: SortCol -> [SortCol] -> [SortCol]
updateSortCols scol [] = [scol]
updateSortCols (col, asc) (sc@(c,_):cs)
    | col == c = (col, asc):cs
    | otherwise = sc : updateSortCols (col, asc) cs

deleteFirst :: (a -> Bool) -> [a] -> [a]
deleteFirst _ [] = []
deleteFirst predicate (c:cs)
    | predicate c = cs
    | otherwise = c : deleteFirst predicate cs

normTo0 :: Int -> Int
normTo0 x | x > 0     = x
          | otherwise = 0

-- Draw to curses screen
--
myScrSize :: IO (Int, Int)
myScrSize = (subtract 1 *** subtract 1) <$> scrSize

drawRows :: Int -> Int -> [[(String, Attr)]] -> IO ()
drawRows xOff width rows = do
    (hScr, wScr) <- myScrSize
--    myTrace ("drawRows: xStart="++show xStart++", yStart="++show yStart++", hScr="++show hScr++", wScr="++show wScr) $
    wMove stdScr 0 0
    let drawRow = go (min width wScr) . myDrop xOff
        myDrop _ [] = []
        myDrop n ((x,attr):xs)
            | n > len = myDrop (n - len) xs
            | n > 0 = (drop n x, attr) : xs
            | otherwise = (x, attr) : xs
          where len = length x
        go _ [] = myAddLn
        go w ((x,attr):xs)
            | w > 0 = do attrOn attr
                         drawLine len x
                         attrOff attr
                         go (w - len) xs
            | otherwise = myAddLn
          where len = min w (length x)
        myAddLn = do
            (y,_) <- getYX stdScr
            wMove stdScr (y + 1) 0
    mapM_ drawRow $ take hScr rows

drawStatus :: String -> IO ()
drawStatus str = do
  (h,w) <- myScrSize
  wMove stdScr h 0
  drawLine w (replicate w ' ')
  wMove stdScr h 0
  drawLine (length str) (str)

bye :: MonadIO m => m ()
bye = liftIO $ do
  _ <- cursSet CursorVisible
  refresh
  end
  exitWith ExitSuccess

-- Tables
--

-- Todo: give individual columns widths.
type ColIdent = (String, Int)

-- Todo: no space b4 first, but space between tbls
data Tbl a = Tbl {
      trows :: [[a]],
      tColsOrd:: [ColIdent],
      tx :: Int,  -- which character is at left of screen (assume cols of a row are flattened into one string for printing)
      ty :: Int,  -- which row is at top of screen
      colSelected :: Maybe Int, -- which column is selected
      tselected :: Maybe (Int,Int) -- which cell is selected; (x, y). NOTE: x coord is in cols, not chars
    } deriving Show
tcols :: Tbl a -> [String]
tcols = map fst . tColsOrd

initTbl :: Tbl a
initTbl = Tbl [] [] 0 0 Nothing Nothing

mkTbl :: [Int] -> [[ByteString]] -> Tbl ByteString
mkTbl idxs xs = case padIt xs of
                  (cls:rs) -> initTbl{trows = rs, tColsOrd = zip (map unpack cls) idxs}
                  _ -> initTbl

-- | pads all valuse in a col to be width of widest value in the column
padIt :: [[ByteString]] -> [[ByteString]]
padIt cls = transpose $ map padToMax $ transpose $ map (map (B.dropWhile isSpace)) cls --map (map fixNl) cls TODO
    where padToMax vs = map (padTo $ ml) vs  -- + 1 for column space
              where ml = maximum $ map B.length vs
          padTo l x = B.concat [B.singleton ' ', x, B.replicate (l - B.length x) ' ']

          fixNl [] = []
          fixNl ('\n':cs) = "\\n" ++ fixNl cs
          fixNl ('\t':cs) = "\\t" ++ fixNl cs
          fixNl (c:cs) = c : fixNl cs

-- | Given an index, returns the col at the index, and the table less that col.
getCol :: Int -> Tbl a -> ((ColIdent, [a]), Tbl a)
getCol i tbl = ((col, row), newTbl)
  where extractCol ix = go . splitAt ix
            where go (prev, x:rest) = (x, prev ++ rest)
                  go _ = error ("getCol "++show i)
        (col, cols) = extractCol i $ tColsOrd tbl
        (row, rows) = extractCol i $ transpose $ trows tbl
        cselNew | Just i == csel = Nothing
                | Just i < csel = subtract 1 <$> csel
                | otherwise = csel
          where csel = colSelected tbl
        tselNew | Just i == tselx = Nothing
                | Just i < tselx = second (subtract 1) <$> tsel
                | otherwise = tsel
          where tsel = tselected tbl
                tselx = fst <$> tsel
        newTbl = tbl{trows = transpose rows, tColsOrd = cols, colSelected = cselNew, tselected = tselNew}

-- | Given a column and a column identifier, insert it into the table

addCol :: (ColIdent, [a]) -> Tbl a -> Tbl a
addCol ((col, pos), vs) tbl = tbl{tColsOrd = cs, trows = rs}
  where cs = myAdd (col, pos) (tColsOrd tbl)
        rs = transpose $ map fst $ myAdd (vs, pos) $ flip zip (map snd cs) $ transpose $ trows tbl
        myAdd = insertBy (compare `on` snd)

-- test = (mkTbl [["c0","c1","c3"],["a0","a1","a2"],["b0","b1","b2"]]){tselected=Just(1,1), colSelected=Just 1}

-- Table manipulation
--

-- | returns idx of column which offSet is in,
--     previous cols (in reverse),
--     the offSet into this column,
--     the rest of the cols (including this one)
colIdx :: Int -> [[a]] -> (Int, [[a]], Int, [[a]])
colIdx offSet css = go 0 [] offSet css
  where go xc prevCols _ [] = (xc, prevCols, 0, [])
        go xc prevCols i (c:cs) | i' >= 0    = go (xc + 1) (c:prevCols) i' cs
                                | otherwise  = (xc, prevCols, i, c:cs)
          where i' = i - length c

-- | Scrolls table vertically by incr
tblVert :: Int -> Tbl a -> Tbl a
tblVert incr tbl = tbl {ty = ty tbl + incr}

-- | Scrolls table horizontally by incr
tblHoriz :: Int -> Tbl a -> Tbl a
tblHoriz incr tbl = tbl {tx = tx tbl + incr}

tblGotoLine :: Int -> Tbl a -> Tbl a
tblGotoLine ln tbl = tbl {ty = ln}

tblBot :: Tbl a -> Tbl a
tblBot tbl = tbl {ty = (length $ trows tbl) - 1}

-- | Scrolls table left by a column
tblLeftCol :: Tbl a -> Tbl a
tblLeftCol tbl | offSet > 0        = tblHoriz (negate offSet) tbl
               | (c:_) <- prevCols = tblHoriz (negate $ length c) tbl
               | otherwise         = tbl
  where (_cIdx, prevCols, offSet, _cs) = colIdx (tx tbl) (tcols tbl)

-- | Scrolls table right by a column
tblRightCol :: Tbl a -> Tbl a
tblRightCol tbl | (c:_) <- cs = tblHoriz (length c - offSet) tbl
                | otherwise   = tbl
  where (_cIdx, _prevCols, offSet, cs) = colIdx (tx tbl) (tcols tbl)

-- | Make table wider
growTbl :: (Int, a) -> (Int, a)
growTbl (w, tbl) = (w + 1, tbl)

-- | Make table narrower
shrinkTbl :: (Int, a) -> (Int, a)
shrinkTbl (w, tbl) | w > 1 = (w - 1, tbl)
                   | otherwise = (w, tbl)

-- | Given a target, a size, and an offset, then return how far to scroll.
adjVisible :: Int -> Int -> Int -> Int
adjVisible targ sz offSet | offSet <= targ && targ <= offSet + sz = 0
                          | offSet > targ = targ - offSet
                          | otherwise = targ - (offSet + sz)

rowAdjTbl :: Int -> Int -> Tbl a -> Int
rowAdjTbl y h tbl = adjVisible y h (ty tbl)

colAdjTbl :: Int -> Int -> Tbl a -> Int
colAdjTbl i w (tbl@Tbl{tx=xOff}) = adjVisible targ w xOff
  where cs = tcols tbl
        (prev,rest) = splitAt i cs
        ix0 = sum $ map length prev
        ix1 = ix0 + sum [length x | x <- take 1 rest]
        targ | ix1 > xOff + w = ix1
             | otherwise = ix0

-- | Move the column selection by an increment. If no col is selected, pick first.
moveColSelected :: Int -> (Int, Tbl ByteString) -> (Int, Tbl ByteString)
moveColSelected n (w, tbl@(Tbl{colSelected=colSel})) = (w, newTbl)
  where newCol = case colSel of
                   Nothing -> 0
                   Just cIdx -> if cIdx + n < length cs && cIdx + n >= 0
                                then cIdx + n
                                else cIdx
        newTbl = tblHoriz (colAdjTbl newCol w tbl) tbl{colSelected=Just newCol}
        cs = tcols tbl


-- turn a table into a list of rows with attributes for each cell
-- takes height and width of table and does cropping
--
drawTbl :: Bool -> Bool -> Int -> Int -> Tbl ByteString -> [[(String, Attr)]]
drawTbl isActive _showSort height width (tbl@Tbl{trows=rs, tx=xPos, ty=yPos, colSelected=cSel, tselected=tsel}) =
   activeMark : map padRow (cols:rows)
  where
    cs = tcols tbl
    activeMark = [(take width $ mark:repeat ' ', convertAttributes [])]
      where mark | isActive = '*'
                 | otherwise = ' '

    (_,cols) = mapAccumL doCol (0, width) cs
    doCol (idx, w) c = ((idx + 1, w - length c), (c, convertAttributes attrs))
      where attrs | Just idx == cSel = [Reverse]
                  | otherwise        = []

    -- do height cropping
    xsel = fst <$> tsel
    ysel = snd <$> tsel
    height' = height - 3  -- (if showSort then height - 1 else height) - 3
    rows = drop (min yPos $ max 0 $ numRows - height') rows'
    (numRows, rows') = mapAccumL doRow 0 $ map (map unpack) rs
    doRow yIdx r = (yIdx + 1, snd $ mapAccumL go 0 r)
      where boldRow = Just yIdx == ysel
            go xIdx v = (xIdx + 1, (v, convertAttributes attrs))
              where attrs | boldRow && (Just xIdx == xsel) = [Bold]
                          | otherwise                      = []

    -- do width cropping
    dropX = 1 + min xPos (max 0 $ (length $ concat cs) - width)  -- if width is bigger than actual tbl then no movement. We always drop at least one to correct for initial space.
    padRow = takeRow width . dropRow dropX
      where dropRow _ [] = []
            dropRow cnt ((v, attr):vs) | cnt > vLen = dropRow (cnt - vLen) vs
                                       | cnt > 0 = (drop cnt v, attr) : vs
                                       | otherwise = (v, attr):vs
              where vLen = length v
            takeRow cnt [] = [(replicate cnt ' ', convertAttributes [])]
            takeRow cnt ((v, attr):vs) | cnt > vLen = (v, attr) : takeRow (cnt - vLen) vs
                                       | cnt > 0 = [(take cnt v, attr)]
                                       | otherwise = []
              where vLen = length v


-- | turn a list of widths and table into a list rows with attributes for each cell.
-- Then draws all rows and the status line to screen.
drawTbls :: CsvM ()
drawTbls = do
    AppState{xApp=xOff, myTbls=tbls, tblCur=curTbl, sortCols=scs, statusLine=stl} <- get
    liftIO $ do
      (hScr, wScr) <- liftIO myScrSize
      let allTbls = zipWith (\(w,t) i -> drawTbl (i == curTbl) (not $ null scs) hScr w t) tbls [0..]
          allRows = foldr1 zipTbls allTbls
      erase
      drawRows xOff wScr allRows
      maybe (drawStatus "") drawStatus stl
      refresh
  where
    zipTbls (x:xs) (y:ys) = (x ++ ("|",noAttr) : y) : go xs ys
        where go (a:as) (b:bs) = (a ++ (" ",noAttr) : b) : go as bs
              go as [] = as
              go [] bs = bs
    zipTbls xs [] = xs
    zipTbls [] ys = ys
    noAttr = convertAttributes []
{-
    addSort | showSort = ([(concat $ map fmtSort sCols, convertAttributes [])] :)
            | otherwise = id
      where fmt x = [(x, convertAttributes [])]
    fmtSort (idx, asc) | asc = colNm++"^ "
                       | otherwise = colNm++"v "
      where colNm = Msc.killSpace (atNote ("fmt "++show idx) cs idx) ++ " "
-}


-- Finding
--
myFind :: (a -> Maybe b) -> [a] -> Maybe b
myFind p = getFirst . go
  where go [] = First Nothing
        go (x:xs) = First (p x) `mappend` go xs

findInList :: Show elm => Bool -> Bool -> Maybe (elm -> Bool) -> (elm -> Maybe idx) -> [elm] -> Maybe idx
findInList useMatchElm backwards brkFunOpt predicate xs = myFind predicate row
  where (prevElms, nextElms) | Just bf <- brkFunOpt = first reverse $ break bf xs
                             | backwards                = (reverse xs, [])
                             | otherwise                = ([], xs)

        row | backwards && useMatchElm           = take 1 nextElms ++ prevElms
            | backwards                          = prevElms
            | useMatchElm || isNothing brkFunOpt = nextElms
            | otherwise                          = drop 1 nextElms

matchFun :: RegexLike regex source => regex -> (source, a) -> Maybe a
matchFun regex (x,i) | match regex x = Just i
                     | otherwise     = Nothing

brkFun :: Eq a => Maybe a -> Maybe ((b,a) -> Bool)
brkFun x = (\i (_,y) -> y == i) <$> x


findTbl :: Bool -> Maybe Regex -> Tbl ByteString -> Maybe (Int, Int)
findTbl backwards regex (Tbl{trows = rs, tselected = tsel}) =
    findInList True backwards (brkFun $ snd <$> tsel) rowPred (zip rs [0..])
  where
    rowPred (r,i) = flip (,) i <$> findInList False backwards (xBrkFun $ fst <$> tsel) mFun (zip r [0..])
      where xBrkFun xsel | Just i == (snd <$> tsel) = brkFun xsel
                         | otherwise = Nothing
    mFun = maybe (const Nothing) matchFun regex



-- Main state
--

data AppState = AppState {
      myTbls :: [(Int, Tbl ByteString)], -- width, table
      tblCur :: Int,
      myCols :: [String],
      myRows :: [[ByteString]],
      sortCols :: [(Int,Bool)], -- sorting preference for cols and whether col is ascending (we have a multi col sort)
      rxHist :: [String],
      rxCur :: Maybe Regex,
      xApp :: Int,
      statusLine :: Maybe String
    }

type CsvM a = StateT AppState IO a

putStatus :: Maybe String -> CsvM ()
putStatus s = modify (\as -> as {statusLine = s})

--Reproject rows into underlying tables. Needed when the order is changed
projectViews :: CsvM ()
projectViews = do
    as@AppState{myTbls=nums:myVs, myRows=rs, myCols=_cs, sortCols=_srtCs} <- get
    let proj tbl = tbl{trows = map doProj rs}
          where cIdxs = map snd $ tColsOrd tbl
                doProj r = map (atNote "projectViews" r) cIdxs
    put as{myTbls = nums:map (second proj) myVs}

withCurTbl :: ((Int, Tbl ByteString) -> CsvM a) -> CsvM a
withCurTbl f = do
    as <- get
    let tbl = atNote "withCurTbl" (myTbls as) (tblCur as)
    f tbl

putCurTbl :: (Int, Tbl ByteString) -> CsvM ()
putCurTbl tbl = modify $ \as -> as{myTbls = replaceNth (tblCur as) tbl (myTbls as)}

modCurTbl :: Int -> ((Int, Tbl ByteString) -> (Int, Tbl ByteString)) -> CsvM ()
modCurTbl h f = withCurTbl $ \tbl -> putCurTbl (correct h $ f tbl)

correct :: Int -> (Int, Tbl a) -> (Int, Tbl a)
correct h (w, t@(Tbl{tx=x, ty=y, trows=rs})) = (w, t{tx = x', ty = y'})
  where
    x' = max 0 $ min (length (concat cs) - w) x
    y' = max 0 $ min ((length rs - 1) - h) y
    cs = tcols t

correctAs :: AppState -> AppState
correctAs as = as {tblCur = newTblCur}
    where
      newTblCur = max 1 $ min (length (myTbls as) - 1) $ tblCur as

modAllTbls :: Int -> (Tbl ByteString -> Tbl ByteString) -> CsvM ()
modAllTbls h f = do
    as <- get
    put as{myTbls = map (correct h . second f) (myTbls as)}

curTblLeft, curTblRight :: CsvM ()
curTblLeft  = modify $ \as -> correctAs $ as {tblCur = tblCur as - 1}
curTblRight = modify $ \as -> correctAs $ as {tblCur = tblCur as + 1}

tryAdjColWidths :: Int -> Int -> Int -> (Int, Int)
tryAdjColWidths decrW incrW delta = (decrW - delta', incrW + delta')
    where
      decrDeltaDiff = decrW - delta
      delta'
        | decrDeltaDiff < 5 = delta - (decrDeltaDiff - 5)
        | otherwise = delta

moveColLeft :: (Int, Tbl ByteString) -> CsvM ()
moveColLeft (_, Tbl{colSelected=Nothing}) = return ()
moveColLeft (w, tbl@Tbl{colSelected=Just csel}) = do
    as @ AppState{myTbls=tbls, tblCur=curTblIdx} <- get
    let ((nw, newTbl), newTblIdx, newCurTblIdx)
            | curTblIdx > 1 = (atNote "moveColLeft" tbls (curTblIdx - 1), curTblIdx - 1, curTblIdx)
            | otherwise = ((1, (mkTbl [] []) {ty=ty tbl}), 1, curTblIdx + 1)
        (curCol, tbl') = getCol csel tbl
        newTbl' = addCol curCol newTbl
        (w',nw') = tryAdjColWidths w nw $ length (fst $ fst curCol)
        newTbls
            | curTblIdx > 1 = replaceNth newCurTblIdx (w', tbl') $ replaceNth newTblIdx (nw', newTbl') tbls
            | otherwise = replaceNth newCurTblIdx (w', tbl') $ myCons (nw', newTbl') tbls
        myCons x = uncurry (++) . second (x :) . splitAt 1
    put $ correctAs $ as{myTbls = filter ((> 0) . length . tColsOrd . snd) newTbls, tblCur = newCurTblIdx}

moveColRight :: (Int, Tbl ByteString) -> CsvM ()
moveColRight (_, Tbl{colSelected=Nothing}) = return ()
moveColRight (w, tbl@Tbl{colSelected=Just csel}) = do
    as @ AppState{myTbls=tbls, tblCur=curTblIdx} <- get
    let tblsLen = length tbls - 1
        (nw, newTbl)
            | curTblIdx < tblsLen = atNote "moveColRight" tbls (curTblIdx + 1)
            | otherwise = (1, (mkTbl [] []) {ty=ty tbl})
        (curCol, tbl') = getCol csel tbl
        newTbl' = addCol curCol newTbl
        (w',nw') = tryAdjColWidths w nw $ length (fst $ fst curCol)
        newTbls
            | curTblIdx < tblsLen = replaceNth curTblIdx (w', tbl') $ replaceNth (curTblIdx + 1) (nw', newTbl') tbls
            | otherwise = replaceNth curTblIdx (w', tbl') $ tbls ++ [(nw', newTbl')]
    put $ correctAs $ as{myTbls = filter ((> 0) . length . tColsOrd . snd) newTbls}

addSortCol :: Bool -> (Int, Tbl ByteString) -> CsvM ()
addSortCol _ (_, Tbl{colSelected=Nothing}) = return ()
addSortCol asc (_, Tbl{trows=_rs, colSelected=Just csel}) = do
    scs <- updateSortCols (csel, asc) . sortCols <$> get
    modify $ \as -> as{sortCols = scs, myRows = multiColSort scs (myRows as)}
    projectViews

remSortCol :: (Int, Tbl ByteString) -> CsvM ()
remSortCol (_, Tbl{colSelected=Nothing}) = return ()
remSortCol (_, Tbl{trows=_rs, colSelected=Just csel}) = do
    scs <- deleteFirst ((== csel) . fst) . sortCols <$> get
    modify $ \as -> as{sortCols = scs, myRows = multiColSort scs (myRows as)}
    projectViews

showSortOrder :: CsvM ()
showSortOrder = do
  sCols <- sortCols <$> get
  cls    <- myCols <$> get
  putStatus $ Just $ "Sort Ordering: " ++ intercalate ", " (map (fmtSort cls) sCols)
        where fmtSort cs (idx, asc) | asc = colNm++" ^ "
                                    | otherwise = colNm++" v"
                  where colNm = killSpace (atNote ("fmt "++show idx) cs idx)


-- User Input
--
getK :: CsvM Key
getK = getK' (return ())

getK' :: CsvM () -> CsvM Key
getK' f = getKey $ do
            liftIO endWin
            liftIO refresh
            (h, w) <- liftIO scrSize
            as <- get
            let width = sum [x | (x,_) <- myTbls as] - xApp as
            modCurTbl h $ \(wTbl, tbl) -> (max 2 (wTbl + w - width), tbl)
            drawTbls
            f
            liftIO refresh

getLn :: String -> CsvM String
getLn msg = do
  liftIO $ drawStatus $ msg
  liftIO $ refresh
  go 0 ""
  where go hstN str = do
          c <- getK' $ liftIO $ drawStatus (msg ++ str)
          -- (_, w) <- liftIO myScrSize
          let getHistory incr = do
                hst <- ("" :) . rxHist <$> get
                let hstN1 = incr hstN
                    hstN2 | hstN1 >= length hst = 0
                          | hstN1 < 0 = length hst - 1
                          | otherwise = hstN1
                    str' = atNote "getLn"  hst hstN2
                -- (y,_x) <- liftIO $ getYX stdScr
                liftIO $ drawStatus (msg ++ str') >> refresh
                go hstN2 str'
          case c of
            KeyChar '\r' -> return str
            KeyBackspace -> do
              -- (y,_x) <- liftIO $ getYX stdScr
              let str' = (reverse . drop 1 . reverse) str
              liftIO $ drawStatus (msg ++ str') >> refresh
              go hstN str'
            KeyChar char -> do
              liftIO $ wAddStr stdScr [char]
              liftIO $ refresh
              go hstN $ str ++ [char]
            KeyUp -> getHistory (+ 1) -- do
            KeyDown -> getHistory (subtract 1) -- do
            _ -> go hstN str

queryStatus :: String -> CsvM a -> CsvM a -> CsvM a
queryStatus msg ycont ncont = do
  str <- getLn (msg ++ " (y/n): ")
  case str of
    ('y':_) -> ycont
    _ -> ncont

runFindCol :: Bool -> Maybe Regex -> (Int, Tbl ByteString) -> CsvM ()
runFindCol backwards regex (w, tbl) =
    case findInList False backwards (brkFun $ colSelected tbl) (maybe (const Nothing) matchFun $ regex) (zip (tcols tbl) [0..]) of
      Just cIdx -> putCurTbl (w, (tblHoriz (colAdjTbl cIdx w tbl) tbl){colSelected=Just cIdx})
      Nothing -> if backwards
                 then queryStatus "Not found. Start from end?"
                                  (runFindCol True regex (w, tbl{colSelected=Nothing}))
                                  (return ())
                 else queryStatus "Not found. Start from beginning?"
                                  (runFindCol False regex (w, tbl{colSelected=Nothing}))
                                  (return ())

runFind :: Int -> Bool -> Maybe Regex -> (Int, Tbl ByteString) -> CsvM ()
runFind h backwards regex (w, tbl) =
    case findTbl backwards regex tbl of
      Just pos@(xPos, yPos) -> do let yDelta = rowAdjTbl yPos h tbl
                                      xDelta = colAdjTbl xPos w tbl
                                  modCurTbl h $ \(w', t) -> (w', tblHoriz xDelta t{tselected = Just pos})
                                  modAllTbls h $ tblVert yDelta
      Nothing -> if backwards
                 then queryStatus "Not found. Start from end?"
                                  (runFind h True regex (w, tbl{tselected=Nothing}))
                                  (return ())
                 else queryStatus "Not found. Start from beginning?"
                                  (runFind h False regex (w, tbl{tselected=Nothing}))
                                  (return ())

unselectVal, unselectCol :: Tbl a -> Tbl a
unselectVal tbl = tbl{tselected=Nothing}
unselectCol tbl = tbl{colSelected=Nothing}

--todo pass in h
incrSearch :: String -> (Bool -> Maybe Regex -> (Int, Tbl ByteString) -> CsvM ()) -> (Tbl ByteString -> Tbl ByteString) -> CsvM ()
incrSearch prompt findFun unselectFun = do
  lnp <- getLn prompt
  (h,_) <- liftIO myScrSize
  case killSpace lnp of
    "" -> modCurTbl h (second unselectFun) >> modify (\as -> as {rxCur = Nothing})
    ln -> do
      let regex = Just $ makeRegex ln
      modify (\as -> as {rxHist = ln : rxHist as, rxCur = regex})
      withCurTbl $ findFun False regex

gotoLine :: Int -> CsvM ()
gotoLine h = do
  ln <- getLn "goto line: "
  case readMay ln of
    Just lnNum -> modAllTbls h (tblGotoLine lnNum)
    Nothing -> putStatus $ Just "Bad line number."

-- REPL
--
processKeys :: (Int -> Int -> Maybe Regex -> KeyTree) -> CsvM ()
processKeys kt = go
    where
      go = do
        drawTbls
        putStatus Nothing
        key <- getK
        as <- get
        (h,w) <- liftIO myScrSize
        let regex = rxCur as
        fromMaybe (putStatus (Just $ "Did not recognize key: " ++ showKey key)) $
                  lookupKey key (kt (h - 3) w regex) -- nb (h - 2) ignores a possible sortcol. counts status line and tbl select, figure out what to do?
        if key `elem` map KeyChar ['e','q']
          then return ()
          else go

type KeyAssoc = ([Key], CsvM (), String)
type KeyTree = [(String, [KeyAssoc])]

lookupKey :: Key -> KeyTree -> Maybe (CsvM ())
lookupKey k kt = (\(_,x,_) -> x) <$> find (\(ks,_,_) -> k `elem` ks) km
 where km = concat . map snd $ kt

showKey :: Key -> String
showKey kc = case kc of
     KeyChar ' ' -> "Spc"
     KeyUp -> "Up"
     KeyDown -> "Down"
     KeyLeft -> "Left"
     KeyRight -> "Right"
     KeySLeft -> "Shift-Left"
     KeySRight -> "Shift-Right"
     KeyPPage -> "PgUp"
     KeyNPage -> "PgDown"
     KeyUnknown 565 -> "^Up"
     KeyUnknown 521 -> "^Down"
     KeyUnknown 559 -> "^Right"
     KeyUnknown 541 -> "^Left"
     KeyUnknown 560 -> "^Shift-Right"
     KeyUnknown 542 -> "^Shift-Left"
     KeyChar '\t' -> "Tab"
     KeyChar '\SUB' -> "^z"
     KeyChar '\ETX' -> "^c"
     KeyBTab -> "Shift-Tab"
     KeyChar x -> show x
     x -> show x

keyTreeNoHelp :: Int -> Int -> Maybe Regex -> KeyTree
keyTreeNoHelp h w regex = ("",[([KeyChar 'h',KeyChar '?'], return (), "")]) : keyTree h w regex

suspendMe :: CsvM ()
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
suspendMe = return ()
#else
suspendMe = liftIO $ raiseSignal sigTSTP
#endif

keyTree :: Int -> Int -> Maybe Regex -> KeyTree
keyTree h w regex = [
     ("Basics",
      [ ([KeyChar 'h', KeyChar '?'], liftIO helpScreen, "Display this help")
      , ([KeyChar 'e', KeyChar 'q'], return (), "Exit")
      , ([KeyChar 'r'], liftIO $ erase >> refresh, "Redraw")
      , ([KeyChar '\SUB'], suspendMe, "Suspend")
      -- , ([KeyChar '\ETX'], bye, "Force Exit")
      ])

   , ("Moving",
      [ ([KeyDown], modAllTbls h (tblDown 1), "Forward one line")
      , ([KeyUp], modAllTbls h (tblUp 1), "Backward one line")
      , ([KeyChar ' ', KeyNPage, KeyUnknown 521], modAllTbls h (tblDown (h - 1)), "Forward one page")
      , ([KeyChar 'b', KeyPPage, KeyUnknown 565], modAllTbls h (tblUp (h - 1)), "Backward one page")
      , ([KeyRight]  , modCurTbl h (tblRight 1), "Right one character")
      , ([KeyLeft]   , modCurTbl h (tblLeft 1), "Left one character")
      , ([KeySRight] , modCurTbl h (second tblRightCol), "Right one column")
      , ([KeySLeft]  , modCurTbl h (second tblLeftCol), "Left one column")
      , ([KeyUnknown 559] , modCurTbl h (tblRight (w - 1)), "Right one page")
      , ([KeyUnknown 541] , modCurTbl h (tblLeft (w - 1)), "Left one page")
      ])

   , ("Jumping",
      [ ([KeyChar 'g', KeyHome], modAllTbls h (tblGotoLine 0), "Go to first line")
      , ([KeyChar 'G', KeyEnd], modAllTbls h tblBot, "Go to last line")
      , ([KeyChar '\a'], gotoLine h, "Jump to a given line")
      -- To add: goto line.
      ])

   , ("Searching",
      [ ([KeyChar '/'], incrSearch "/" (runFind h) unselectVal, "Enter regular expression and search rows for a match")
      , ([KeyChar 'n'], withCurTbl (runFind h False regex), "Repeat previous search on rows")
      , ([KeyChar 'p'], withCurTbl (runFind h True  regex), "Repeat previous search on rows in reverse direction")
      , ([KeyChar ':'], incrSearch ":" (runFindCol) unselectVal, "Enter regular expression and search column names for a match")
      , ([KeyChar 'N'], withCurTbl (runFindCol False regex), "Repeat previous search on rcolumn names")
      , ([KeyChar 'P'], withCurTbl (runFindCol True  regex), "Repeat previous search on column names in reverse direction")
      ])

   , ("Column Navigation",
      [ ([KeyChar '\t'], modCurTbl h (moveColSelected 1), "Move column selection to the right")
      , ([KeyBTab], modCurTbl h (moveColSelected (-1)), "Move column selection to the left")
      ])

   , ("Sorting",
      [ ([KeyChar '^'], withCurTbl (addSortCol True), "Make selected column ascending in sort, or add it to sort")
      , ([KeyChar 'v'], withCurTbl (addSortCol False), "Make selected column descending in sort, or add it to sort")
      , ([KeyChar 'x'], withCurTbl remSortCol, "Remove column from sort")
      , ([KeyChar '&'], showSortOrder, "Display current sort ordering")
      ])

   , ("Slicing",
      [ ([KeyUnknown 542], curTblLeft, "Move table focus left")
      , ([KeyUnknown 560], curTblRight, "Move table focus right")
      , ([KeyChar '<'], withCurTbl moveColLeft, "Move selected column left one table")
      , ([KeyChar '>'], withCurTbl moveColRight, "Move selected column right one table")
      , ([KeyChar 'a'], modCurTbl h growTbl, "Grow current table 1 column")
      , ([KeyChar 'z'], modCurTbl h shrinkTbl, "Shrink current table 1 column")
      ])

   ]
  where
    tblUp = tblVert . negate
    tblDown = tblVert
    tblLeft = second . tblHoriz . negate
    tblRight = second . tblHoriz
{-
    moveApp incr = modify go
      where go as = as{xApp = min (max 0 $ len - w) (max 0 (xApp as + incr))}
              where len = sum $ map fst $ myTbls as
-}

helpScreen :: IO ()
helpScreen = do
  (_, wScr) <- myScrSize
  evalStateT (processKeys keyTreeNoHelp) $ AppState
                 [(0, mkTbl [] []), (wScr, tbl)] 1 (tcols tbl) (trows tbl) [] [] Nothing 0 Nothing
      where tbl = mkTbl [0..] $ map (map pack) $ ["keys","action"] : concatMap render (keyTree 0 0 Nothing)
            render (heading,kas) =
                ["","---------"] :
                [""," * " ++ heading ++ " * "] :
                ["",""] :
                map (\(ks,_,desc) -> [intercalate " " (map showKey ks), desc]) kas

--todo: catch exception and cleanup



main :: IO ()
main = do
  pargs <- parseArgsIO ArgsComplete
          [
           Arg "Separator" (Just 's') (Just "separator") (argDataDefaulted "character" ArgtypeString ",") "separator character, defaults to comma",
           Arg "Help" (Just 'h') (Just "help") Nothing "Displays this help",
           Arg "Infile"  Nothing Nothing (argDataOptional "filename"  ArgtypeString) "Input file"
          ]
  when (gotArg pargs "Help") $ usageError pargs ""
  let sepChar = fromMaybe ',' $ join $ fmap headMay $ getArg pargs "Separator"
      foldCSVHandle h csvs f acc = E.run (enumHandle 4096 h $$ iterCSV csvs f acc)
      readCSVHandle s h = fmap reverse <$> foldCSVHandle h s collectRows []

  inHandle <- getArgStdio pargs "Infile" ReadMode
  canRead <- hReady inHandle
  when (not canRead) $ usageError pargs ""
  mydata <- {-fmap (map (map unpack))$ -} either throwIO return =<< readCSVHandle (defCSVSettings {csvSep = sepChar}) inHandle
  start
  -- [headsty,tbsty] <- convertStyles [AttributeStyle [Bold] DarkGreenF BlackB, Style DarkGreenF DefaultB]
  (_h, w) <- myScrSize
  let myTbl = mkTbl [0..] mydata
      nums = (5, mkTbl [0] (map (map pack) $ ["row"] : map ((:[]) . show) [(0::Int) .. length mydata]))
      as = AppState [nums, (w - 5, myTbl)] 1 (tcols myTbl) (trows myTbl) [] [] Nothing 0 Nothing
  evalStateT (processKeys keyTree) $ as
  bye


