{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}

{-|

Library for using the LCM algorithm in order to compute closed frequent pattern.
Input must be a transaction database, either in text format (as a ByteString)
or in @[[Item]]@ format, where @Item = Int@.

Several bencharking functions allowing to tune parallel strategy used and depth
cutoff are also provided.

-}


-- HLCM
-- (c) Alexandre Termier, 2009-2010
-- Original LCM algorithm from Takaki Uno and Hiroki Arimura.
-- Many performance improvements thanks to Simon Marlow and Satnam Singh.
-- 
-- Module implementing the LCM algorithm.
-- 
--
-- See the README file for installation, usage, and details.
-- See the LICENSE file for licensing.



module HLCM
       ( Frequency, Item
       , runLCMstring
       , runLCMmatrix
       , benchLCM_parBuffer
       , benchLCM_parMap
       ) where

-----------------------------------------------------------------
-- Imports
-----------------------------------------------------------------

import Data.List
import Data.Array.Unboxed
import Control.Monad.ST
import Data.Array.ST
import Control.Parallel
import Control.Parallel.Strategies
import Control.Exception( evaluate )
import Data.Array.Base
import Control.Monad
import GHC.Exts
import GHC.ST

import qualified Data.List as List

import qualified Data.ByteString.Char8 as L

-----------------------------------------------------------------
-- Type definitions
-----------------------------------------------------------------

type Item = Int
type Frequency = Int
type Tid = Int
type Weight = Int

-----------------------------------------------------------------
-- LCM functions
-----------------------------------------------------------------

{-|
   Get the data as a long bytestring, parses it and
   and executes LCM to discover closed frequent itemsets.
-}
runLCMstring :: L.ByteString -- ^ The transaction database as a big string. Transactions are separated by newlines, items are separated by spaces
                -> Frequency -- ^ Minimum frequency threshold for the frequent itemsets
                -> [[Item]]  -- ^ Output: list of closed frequent itemsets
runLCMstring stringMatrix freq =
  let -- Load the file and convert it in the transaction database format (lexicographic tree)
      (transactionsLT, maxItem, antiPerm) = loadTransactionsString stringMatrix freq 
      -- Compute occurrences for 1-itemsets
      occs = occurrenceDeliverLT transactionsLT  maxItem 
  in
      reversePerm antiPerm $ concat (parBuffer 8 rdeepseq $ (map (\x -> lcmIter 1 transactionsLT [] x (-1) occs maxItem freq) [0..maxItem]) )
       
       
{-|
   Get the data as a matrix of Items, parses it and
   and executes LCM to discover closed frequent itemsets.
-}
runLCMmatrix :: [[Item]] -- ^ The transaction database as matrix of items (List of List)
                -> Frequency -- ^ Minimum frequency threshold for the frequent itemsets
                -> [[Item]]  -- ^ Output: list of closed frequent itemsets
runLCMmatrix transMatrix freq =
    let -- Convert the matrix in the transaction database format (lexicographic tree)
        (transactionsLT, maxItem, antiPerm) = loadTransactionsMatrix transMatrix freq 
        -- Compute occurrences for 1-itemsets
        occs = occurrenceDeliverLT transactionsLT  maxItem 
    in
        reversePerm antiPerm $ concat (parBuffer 8 rdeepseq $ (map (\x -> lcmIter 1 transactionsLT [] x (-1) occs maxItem freq) [0..maxItem]) )
     

{-|
   Use for benchmarking, parallel strategy = parBuffer by Simon Marlow. 
   This strategy does not have space leak. 

   /Warning: outputs are unusable as is, because items are renamed internally, and in this function the reverse 
   renaming is not performed. It is trivial to have it back by copying the code from runLCMstring./
-}
benchLCM_parBuffer :: L.ByteString -- ^ The transaction database as a big string. Transactions are separated by newlines, items are separated by spaces
                        -> Frequency -- ^ Minimum frequency threshold for the frequent itemsets
                        -> Int       -- ^ value for parBuffer
                        -> Int       -- ^ depth for cutting parallelism
                        -> [[Item]]  -- ^ Output: list of closed frequent itemsets
benchLCM_parBuffer stringMatrix freq n d =
  let -- Load the file and convert it in the transaction database format (lexicographic tree)
      (transactionsLT, maxItem, antiPerm) = loadTransactionsString stringMatrix freq 
      -- Compute occurrences for 1-itemsets
      occs = occurrenceDeliverLT transactionsLT  maxItem 
  in
      concat (parBuffer n rdeepseq $ (map (\x -> lcmIterParBuffer 1 transactionsLT [] x (-1) occs maxItem freq n d) [0..maxItem]) )
       


{-|
   Use for benchmarking, parallel strategy = parMap from Control.Parallel.Strategies. 

   /Warning: outputs are unusable as is, because items are renamed internally, and in this function the reverse 
   renaming is not performed. It is trivial to have it back by copying the code from runLCMstring./
-}
benchLCM_parMap :: L.ByteString -- ^ The transaction database as a big string. Transactions are separated by newlines, items are separated by spaces
                   -> Frequency -- ^ Minimum frequency threshold for the frequent itemsets
                   -> Int       -- ^ depth for cutting parallelism
                   -> [[Item]]  -- ^ Output: list of closed frequent itemsets
benchLCM_parMap stringMatrix freq d =
  let -- Load the file and convert it in the transaction database format (lexicographic tree)
      (transactionsLT, maxItem, antiPerm) = loadTransactionsString stringMatrix freq 
      -- Compute occurrences for 1-itemsets
      occs = occurrenceDeliverLT transactionsLT  maxItem 
  in
      concat ( ((parMap rdeepseq) (\x -> lcmIterParMap 1 transactionsLT [] x (-1) occs maxItem freq d) [0..maxItem]))
       




{-|
  Takes a list of itemsets and a permutation of items, an apply this permutation to the itemsets.
-}
reversePerm :: UArray Item Item -- ^ A permutation of items
            -> [[Item]] -- ^ Input list of itemsets. An itemset is a list of items, starting with its frequency.
            -> [[Item]] -- ^ Permuted list of itemsets. Frequency at the head of the itemset is untouched.
reversePerm _ [] = []
reversePerm antiPerm (t:ts) = ((head t):(sort $ map (\it -> unsafeAt antiPerm it) (tail t))):(reversePerm antiPerm ts)

{-|
   Loads input datase as string into a database of transactions in lexicographic tree format.
   Also reorders/renames the item by their frequency.
   The permutation between old item values and new item values is also returned.
-}
loadTransactionsString :: L.ByteString -- ^ Data as a long bytestring
                          -> Frequency    -- ^ Minimum support threshold
                          -> (LexicoTreeItem, Item, UArray Item Item) -- ^ Return value : (Database, maximum item value after reduction, permutation of items)
loadTransactionsString s thres = 
    case stringToTransDB (L.lines s) of
      Nothing -> (Nil,  -1, array (1,0) []) 
      Just intMatrix -> let itmFrq = histogram (0, maxItem) intMatrix
                            maxItem = maximum (concat intMatrix)
                            (perm, antiPerm) = permut itmFrq
                            reorderedMat = reorderMat intMatrix perm itmFrq thres
                            maxItem' = maximum (concat reorderedMat)
                            lexicoTree = foldr (\t lt -> insertLT t (-1) 1 lt) Nil reorderedMat
                        in  (lexicoTree, maxItem', antiPerm)
                            
                            
{-|
   Loads input datase as a matrix of Items into a database of transactions in lexicographic tree format.
   Also reorders/renames the item by their frequency.
   The permutation between old item values and new item values is also returned.
-}
loadTransactionsMatrix :: [[Item]] -- ^ Data as a matrix of Items
                          -> Frequency    -- ^ Minimum support threshold
                          -> (LexicoTreeItem, Item, UArray Item Item) -- ^ Return value : (Database, maximum item value after reduction, permutation of items)
loadTransactionsMatrix intMatrix thres = 
  let itmFrq = histogram (0, maxItem) intMatrix
      maxItem = maximum (concat intMatrix)
      (perm, antiPerm) = permut itmFrq
      reorderedMat = reorderMat intMatrix perm itmFrq thres
      maxItem' = maximum (concat reorderedMat)
      lexicoTree = foldr (\t lt -> insertLT t (-1) 1 lt) Nil reorderedMat
  in  (lexicoTree, maxItem', antiPerm)


{-|
  Converts the contents of a data string to 
  a transaction database and computes maxItem.
-}
stringToTransDB :: [L.ByteString] -- ^ List of transactions
                -> Maybe [[Item]] -- ^ Return value : transactions as lists of items, and maxItem
stringToTransDB [] = return []
stringToTransDB (t:ts) = do
    ts' <- stringToTransDB ts
    return (convertOneTrans t:ts')

{-|
  Converts one transaction from string format to item format [Item].
-}
convertOneTrans :: L.ByteString          -- ^ Transaction as extracted from file
                -> [Item]                -- ^ Result : list of items
convertOneTrans s = 
    case L.readInt s of
      Nothing -> []
      Just (itm, rest) -> if (not $ L.null rest) 
                             then itm:(convertOneTrans (L.tail rest))
                             else [itm]


{-|
  For a transaction database of type [[Item]], compute the frequency
  of each item and return an array (item, frequency). 
-}
histogram :: (Item,Item) -- ^ Bounds for resulting array, must be (0, maxItem)
          -> [[Item]]    -- ^ Transaction database 
          -> UArray Item Frequency -- ^ Result : Array associating each item with its frequency
histogram bnds lst = accumArray (+) 0 bnds [(i, 1) | j <- lst, i <- {-nub-} j{-, inRange bnds i-}]


{-|
  Sorts a list of (Item, frequency of this item) in
  decreasing ordrer of frequency.

  XXX PERF : seems unefficient. 
-} 
sortFrq :: [(Item,Frequency)] -- ^ Each item is associated with its frequency
        -> [(Item,Frequency)] -- ^ Same list as input, but sorted in decreasing frequency order
sortFrq [] = []
sortFrq ((x,y):rest) = (sortFrq [(x',y') | (x',y') <- rest, y' >= y])
                       ++ [(x,y)]
                       ++ (sortFrq [(x'',y'') | (x'',y'') <- rest, y'' < y])
-- XXX PERF: lots of copies here...maybe not a pb for small lists ?

{-|
  From an array of (item, frequency of this item),
  computes an associative array X where X[i] is
  the ith most frequent item.

  XXX PERF : performance issue with conversions to and from list inside function.
-}
permut :: UArray Item Frequency -- ^ Array associating to each item its frequency
       -> (UArray Item Item, UArray Item Item) -- ^ Result : new array where items are ranked by frequency, and the reverse
permut arr = let (lo,hi) = bounds arr
                 lstVal = [(i,(arr!i)) | i <- [lo..hi]]
                 lst2 = sortFrq lstVal
          --   in  array (lo,hi) [(k, (fst (lst2 !! (k)))) | k <- [lo..hi]]
             in  (array (lo,hi) [(k, (head $ findIndices ((==k).fst) lst2)) | k <- [lo..hi]],
                 array (lo,hi) [(k, (fst (lst2 !! (k)))) | k <- [lo..hi]])
-- XXX PERF: must be bad : the array is converted to list (one copy),
-- then this list is sorted (more copies of small lists), and at
-- last a new array is created...
-- Try to improve this with a mutable array and more "in place" spirit...


{-|
  Rewrites an input transaction database by removing infrequent items and 
  using reordered items (i.e. each item is replaced by its rank in decreasing frequency order).
-}
reorderMat :: [[Item]]                -- ^ Original transaction database
             -> UArray Item Item      -- ^ Array of items sorted by decreasing frequency order (item, rank)
             -> UArray Item Frequency -- ^ Array associating each item with its frequency (item, frequency)
             -> Frequency             -- ^ Minimum frequency threshold
             -> [[Item]]              -- ^ Result : rewritten transaction database
reorderMat ts perm itmFrq thres = 
    map (\t -> 
             sort $ (map (\i -> (unsafeAt perm i)) -- permutates items
                    (filter (\x -> (unsafeAt itmFrq x) >= thres) -- eliminate unfrequent items
                     t))) ts


{-| 
  Compute for each item of the transaction database its frequency.
-}
occurrenceDeliverLT :: LexicoTreeItem         -- ^ Transaction database (in lexicographic tree format)
                    -> Item                   -- ^ Maximal item in transaction database
                    -> UArray Item Frequency  -- ^ Result : array associating each item to its frequency.
occurrenceDeliverLT cdb maxItem = 
    runST (do
      arr <- newArray_ (0,maxItem)
      -- Creates an empty array : each item starts with frequency 0
      forM_ [0..maxItem] $ \i -> unsafeWrite arr i 0 -- workaround for http://hackage.haskell.org/trac/ghc/ticket/3586
      -- Compute frequencies for each item by efficient tree traversal
      _ <- traverse cdb arr
      unsafeFreeze arr
      )
 
    

{-|
  Efficient traversal of the transaction database as a lexicographic tree.
  Items frequencies are updated on the fly.
-}
traverse :: LexicoTreeItem            -- ^ Transaction database
         -> STUArray s Item Frequency -- ^ Array associating each item with its frequency. UPDATED by this function !
         -> ST s ()
traverse tree arr = ST $ \s -> 
  case traverse' tree arr s of (# s', _ #) -> (# s', () #)

traverse' :: LexicoTreeItem -> STUArray s Item Frequency -> State# s -> (# State# s, Int# #)
traverse' Nil                     !arr s = (# s, 0# #)
traverse' (Node item child alt w@(I# w#)) !arr s0 =
   case traverse' child arr s0 of { (# s1, childw #) ->
   case traverse' alt arr   s1 of { (# s2, altw   #) ->
   case unsafeRead arr item of { ST f -> case f s2 of { (# s3, I# itemw #) ->
   case unsafeWrite arr item (I# itemw + I# childw + w) of { ST f -> case f s2 of { (# s4, _ #) ->
   (# s4, childw +# w# +# altw #)
   }}}}}}


{-|
  For a transaction database, a closed frequent itemset, and a candidate item
  for extension of this closed frequent itemset, recursively computes all
  the successor closed frequent itemsets by PPC-extension.
-}
lcmIter :: Int                   -- ^ Current depth in the search tree (for parallel optimisation purposes)
          -> LexicoTreeItem       -- ^ Transaction database. 
          -> [Item]                -- ^ Input closed frequent itemset.
          -> Item                  -- ^ Candidate to extend the closed frequent itemset above.
          -> Item                  -- ^ CoreI item relative to the closed frequent itemset
          -> UArray Item Frequency -- ^ Array associating each item with its frequency
          -> Item                  -- ^ Maximal item
          -> Frequency             -- ^ Minimum suppport threshold
          -> [[Item]]              -- ^ Result : list of closed frequent itemsets. Each result is a list of items, the head of the list being the frequency of the item.
lcmIter prof cdb itemset candidate coreI occs maxItem thres = 
    let 
        -- Reduce database
        rdb = projectAndReduce cdb candidate occs
        -- Compute items occurrences in reduced database 
        newOccs = occurrenceDeliverLT rdb maxItem 
        -- Check which items actually appear in reduced database
        presentItems = filter (\i -> newOccs!i > 0) [0..maxItem] 
        candidateFreq = occs!candidate
        -- Compute 100% frequent items, unfrequent items, and future candidates
        (closedFrqItems, candidates, unfrqItems) = computeCandidates thres candidateFreq presentItems newOccs 
        -- Update items occurrences table by suppressing 100% frequent and unfrequent items
        newOccs' = suppressItems newOccs closedFrqItems unfrqItems  
        -- Result closed frequent itemset = input closed frequent itemset + 100% frequent items
        closedItemset = sort (itemset ++ closedFrqItems) 
        -- Only candidates with value lower than input candidate can be used for further extension on this branch
        smallCandidates = takeWhile (<candidate) candidates in
        if (closedFrqItems /= []) -- if there is a result ...
        then if ((last closedFrqItems) <= candidate)-- ...and if it is OK to extend it
             then if ((length smallCandidates) > 0) -- ... and if we have at least 1 possible extension
                  then -- recursiverly extend the candidates
                       if (prof < 3) -- create parallel sparks only for low search space depth 
                       then ((candidateFreq:closedItemset):(concat (parBuffer 2 rdeepseq $ (map (\x -> lcmIter (prof+1) rdb closedItemset x candidate newOccs' maxItem thres) smallCandidates))))
                       else ((candidateFreq:closedItemset):(concat (map (\x -> lcmIter (prof+1) rdb closedItemset x candidate newOccs' maxItem thres) smallCandidates)))
                  else [candidateFreq:closedItemset]      
             else [] 
        else [] 
             
             
             
------------------------------- BENCHMARKING ------------------------------


{-|
  
-}
lcmIterParBuffer :: Int                      -- ^ Current depth in the search tree (for parallel optimisation purposes)
                    -> LexicoTreeItem        -- ^ Transaction database. 
                    -> [Item]                -- ^ Input closed frequent itemset.
                    -> Item                  -- ^ Candidate to extend the closed frequent itemset above.
                    -> Item                  -- ^ CoreI item relative to the closed frequent itemset
                    -> UArray Item Frequency -- ^ Array associating each item with its frequency
                    -> Item                  -- ^ Maximal item
                    -> Frequency             -- ^ Minimum suppport threshold
                    -> Int                   -- ^ Value for parBuffer
                    -> Int                   -- ^ Depth for cutting parallelism
                    -> [[Item]]              -- ^ Result : list of closed frequent itemsets. Each result is a list of items, the head of the list being the frequency of the item.
lcmIterParBuffer prof cdb itemset candidate coreI occs maxItem thres n d = 
    let 
        -- Reduce database
        rdb = projectAndReduce cdb candidate occs
        -- Compute items occurrences in reduced database 
        newOccs = occurrenceDeliverLT rdb maxItem 
        -- Check which items actually appear in reduced database
        presentItems = filter (\i -> newOccs!i > 0) [0..maxItem] 
        candidateFreq = occs!candidate
        -- Compute 100% frequent items, unfrequent items, and future candidates
        (closedFrqItems, candidates, unfrqItems) = computeCandidates thres candidateFreq presentItems newOccs 
        -- Update items occurrences table by suppressing 100% frequent and unfrequent items
        newOccs' = suppressItems newOccs closedFrqItems unfrqItems  
        -- Result closed frequent itemset = input closed frequent itemset + 100% frequent items
        closedItemset = sort (itemset ++ closedFrqItems) 
        -- Only candidates with value lower than input candidate can be used for further extension on this branch
        smallCandidates = takeWhile (<candidate) candidates in
        if (closedFrqItems /= []) -- if there is a result ...
        then if ((last closedFrqItems) <= candidate)-- ...and if it is OK to extend it
             then if ((length smallCandidates) > 0) -- ... and if we have at least 1 possible extension
                  then -- recursiverly extend the candidates
                       if (prof < d) -- create parallel sparks only for low search space depth 
                       then ((candidateFreq:closedItemset):(concat (parBuffer n rdeepseq $ (map (\x -> lcmIterParBuffer (prof+1) rdb closedItemset x candidate newOccs' maxItem thres n d) smallCandidates))))
                       else ((candidateFreq:closedItemset):(concat (map (\x -> lcmIterParBuffer (prof+1) rdb closedItemset x candidate newOccs' maxItem thres n d) smallCandidates)))
                  else [candidateFreq:closedItemset]      
             else [] 
        else [] 

---

{-|
  
-}
lcmIterParMap :: Int                      -- ^ Current depth in the search tree (for parallel optimisation purposes)
                 -> LexicoTreeItem        -- ^ Transaction database. 
                 -> [Item]                -- ^ Input closed frequent itemset.
                 -> Item                  -- ^ Candidate to extend the closed frequent itemset above.
                 -> Item                  -- ^ CoreI item relative to the closed frequent itemset
                 -> UArray Item Frequency -- ^ Array associating each item with its frequency
                 -> Item                  -- ^ Maximal item
                 -> Frequency             -- ^ Minimum suppport threshold
                 -> Int                   -- ^ Depth for cutting parallelism
                 -> [[Item]]              -- ^ Result : list of closed frequent itemsets. Each result is a list of items, the head of the list being the frequency of the item.
lcmIterParMap prof cdb itemset candidate coreI occs maxItem thres d = 
    let 
        -- Reduce database
        rdb = projectAndReduce cdb candidate occs
        -- Compute items occurrences in reduced database 
        newOccs = occurrenceDeliverLT rdb maxItem 
        -- Check which items actually appear in reduced database
        presentItems = filter (\i -> newOccs!i > 0) [0..maxItem] 
        candidateFreq = occs!candidate
        -- Compute 100% frequent items, unfrequent items, and future candidates
        (closedFrqItems, candidates, unfrqItems) = computeCandidates thres candidateFreq presentItems newOccs 
        -- Update items occurrences table by suppressing 100% frequent and unfrequent items
        newOccs' = suppressItems newOccs closedFrqItems unfrqItems  
        -- Result closed frequent itemset = input closed frequent itemset + 100% frequent items
        closedItemset = sort (itemset ++ closedFrqItems) 
        -- Only candidates with value lower than input candidate can be used for further extension on this branch
        smallCandidates = takeWhile (<candidate) candidates in
        if (closedFrqItems /= []) -- if there is a result ...
        then if ((last closedFrqItems) <= candidate)-- ...and if it is OK to extend it
             then if ((length smallCandidates) > 0) -- ... and if we have at least 1 possible extension
                  then -- recursiverly extend the candidates
                       if (prof < d) -- create parallel sparks only for low search space depth 
                       then ((candidateFreq:closedItemset):(concat ((parMap rdeepseq) (\x -> lcmIterParMap (prof+1) rdb closedItemset x candidate newOccs' maxItem thres d) smallCandidates)))
                       else ((candidateFreq:closedItemset):(concat (map (\x -> lcmIterParMap (prof+1) rdb closedItemset x candidate newOccs' maxItem thres d) smallCandidates)))
                  else [candidateFreq:closedItemset]      
             else [] 
        else [] 



------------------- END OF BENMARKING FUNCTIONS -------------------------------



{-|
  For a given itemset being extended by a given candidate,
  compute the closure of this itemset and compute the candidates for further extension.
-}
computeCandidates ::  Frequency                   -- ^ Minimum support thresold
                      -> Frequency                -- ^ Frequency of item selected for extension (@candidate@)
                      -> [Item]                   -- ^ List of items in the transaction database
                      -> UArray Item Frequency    -- ^ Array associating to each item its frequency
                      -> ([Item], [Item], [Item]) -- ^ Result : (100% frequent items = closure, 
                                                  --  candidates for further extension, unfrequent items)
computeCandidates thres candidateFreq presentItems occs =
    let (frequentItems, unfrqItems) = partition(\i -> occs!i >= thres) presentItems
        closedFrqItems = filter (\i -> occs!i == candidateFreq) frequentItems
        candidates = frequentItems \\ closedFrqItems 
    in  (closedFrqItems, candidates, unfrqItems)



{-|
  Modifies an array associating items with their frequency, in order to 
  give a frequency of 0 to a given list of items.

  NB : for performance reasons, this is REALLY a modification, made with unsafe operations.
-}
suppressItems :: UArray Item Frequency    -- ^ Array associating an item with its frequency
                 -> [Item]                -- ^ List of 100% frequent items
                 -> [Item]                -- ^ List of unfrequent items
                 -> UArray Item Frequency -- ^ Initial array, with frequencies of 100% frequent items
                                          --   and unfrequent items set to 0.
suppressItems occs closedItems unfreqItems =
    runST $ do occsST <- unsafeThaw occs :: ST s (STUArray s Item Frequency) -- Can be used in multithread because no concurrent write
               sequence_ (map (\i -> writeArray occsST i 0) closedItems)
               sequence_ (map (\i -> writeArray occsST i 0) unfreqItems)
               newOccs <- unsafeFreeze occsST  -- Can be used in multithread because no concurrent write
               return newOccs



{-|
  Creates a new, reduced transaction database by eliminating all items
  greater than @candidate@ item, and all infrequent items.
-}
projectAndReduce :: LexicoTreeItem           -- ^ Original transaction database
                    -> Item                   -- ^ Candidate item, on which the projection is made
                    ->  UArray Item Frequency -- ^ Array associating each item with its frequency in 
                                              -- original transaction database.
                    -> LexicoTreeItem        -- ^ Result : Reduced transaction database
projectAndReduce Nil _ _ = Nil
projectAndReduce (Node e suiv alt w) !candidate occs 
    | e > candidate = Nil
    | e == candidate = let !(TreeWeight suiv' addWeight) = filterInfrequent suiv occs
                       in  (Node e suiv' Nil (w+addWeight))
    | e < candidate = let !alt'  = projectAndReduce alt candidate occs
                          !suiv' = projectAndReduce suiv candidate occs
                      in  if (occs!e > 0)
                             then if notNil suiv' && notNil alt'
                                     then (Node e suiv' alt' 0)
                                     else if notNil suiv'
                                             then (Node e suiv' Nil 0)
                                             else alt'
                             else if notNil suiv' && notNil alt'
                                     then mergeAlts suiv' alt'
                                     else if notNil suiv'
                                             then suiv'
                                             else alt'

{-|
  Suppress all infrequent items from a transaction database expressed as 
  lexicographic tree, and returns a new lexicographic tree.
-}
filterInfrequent :: LexicoTreeItem              -- ^ Original transaction database
                    -> UArray Item Frequency     -- ^ Array associating each item with its frequency in 
                                                 -- original transaction database. In this setting, 
                                                 -- an infrequent item as a frequency of 0 (because of preprocessing by 
                                                 -- 'suppressItems').
                    -> TreeWeight                -- ^ Result : (transaction database without infrequent items, weight to report in parent nodes)
filterInfrequent Nil _ = TreeWeight Nil 0
filterInfrequent (Node e suiv alt w) occs
  | occs!e > 0                  = TreeWeight (Node e suiv' alt' (w+ws)) wa
  | notNil suiv' && notNil alt' = TreeWeight (mergeAlts suiv' alt') w'
  | notNil alt'                 = TreeWeight alt'  w'
  | notNil suiv'                = TreeWeight suiv' w'
  | otherwise                   = TreeWeight Nil   w'
  where
    w' = w+ws+wa
    !(TreeWeight suiv' ws) = filterInfrequent suiv occs
    !(TreeWeight alt'  wa) = filterInfrequent alt occs

{-# INLINE notNil #-}
notNil Nil = False
notNil _   = True

data TreeWeight = TreeWeight !LexicoTreeItem {-#UNPACK#-}!Weight

-----------------------------------------------------------------
-- LEXICOGRAPHIC TREE MANIPULATION
-----------------------------------------------------------------

{-|
  Type for a lexicographic tree, implementating a n-ary tree over a binary tree.
-}
data LexicoTreeItem = Nil -- ^ Void node
                   | Node {-#UNPACK#-} !Item
                         !LexicoTreeItem -- NB. experimental strictness annotation
                         !LexicoTreeItem -- NB. experimental strictness annotation
                         {-#UNPACK#-} !Int -- ^ A node : item, next node (next in transaction), alternative node (other branch), weight
                    deriving (Eq, Show)

{-|
  Returns the maximal item of a lexicographic tree
-}
treeMax :: LexicoTreeItem -> Item
treeMax Nil = -1
treeMax (Node e Nil Nil _) = e 
treeMax (Node _ suiv Nil _) = treeMax suiv
treeMax (Node _ Nil alt _) = treeMax alt
treeMax (Node _ suiv alt _) = max (treeMax suiv) (treeMax alt)


{-|
  Inserts a transaction in list format into the lexicographic tree.
  Automatically merges identical transactions.
  Performs suffix intersection.
-}
insertLT :: [Item] -- ^ Transaction to insert into lexicographic tree
         -> Item              -- ^ "coreI" item, for suffix intersection.
         -> Int            -- ^ Weight of the transaction to inserct
         -> LexicoTreeItem   -- ^ Input lexicographic tree
         -> LexicoTreeItem   -- ^ Result : a new lexicographic tree with the transaction inserted
insertLT [] _ _ lt = lt
insertLT lst _ w Nil = createPath lst w
insertLT [x] i w (Node e suiv alt weight) 
    | x < e = Node x Nil (Node e suiv alt weight) w
    | x == e = Node e suiv alt (weight + w)
    | x > e = Node e suiv (insertLT [x] i w alt) weight
insertLT (x:xs) i w (Node e suiv alt weight) 
         | x < e = Node x (createPath xs w) (Node e suiv alt weight) 0
         | x == e = if (e /= i)
                    then Node e (insertLT xs i w suiv) alt weight
                    else suffixIntersectionLT xs w (Node e suiv alt weight)
         | x > e = Node e suiv (insertLT (x:xs) i w alt) weight

{-|
  From a transaction and its weight, directly creates a path-shaped lexicographic tree.
-}
createPath :: [Item] -- ^ Transaction
           -> Int            -- ^ Weight of the transaction
           -> LexicoTreeItem   -- ^ Result : a path-shaped lexicographic tree encoding the transaction
createPath [] _ = Nil
createPath [x] w = Node x Nil Nil w
createPath (x:xs) w = Node x (createPath xs w) Nil 0

{-|
  Perform the "suffix intersection" operation with the suffix of a transaction
  and the corresponding part of a lexicographic tree.

  For more details, see "prefixIntersection" in Takeaki Uno's papers about LCM.
-}
suffixIntersectionLT :: [Item] -- ^ Suffix of the transaction to insert.
                     -> Int            -- ^ Weight of the transaction to insert
                     -> LexicoTreeItem   -- ^ (Sub-)lexicographic tree where the transaction must be inserted. The @next@ part (see data type comments)
                                       --   should be a simple path, it will be the target of intersection with the suffix.
                     -> LexicoTreeItem   -- ^ Result : lexicographic tree where the suffix has been added, with correct intersections performed.
suffixIntersectionLT _ w (Node e Nil alt weight) = Node e Nil alt (weight+w)
suffixIntersectionLT lst w (Node e suiv alt weight) = 
    let (newSuiv, addWeight) = suffInterSuiv lst w suiv 
    in  Node e newSuiv alt (weight+addWeight)

{-|
  Intersects a list-shaped transaction and a path-shaped lexicographic tree.
  The result is a path shaped lexicographic tree with weights correctly updated.
-}
suffInterSuiv :: [Item] -- ^ Transaction as list
              -> Int            -- ^ Weight of the above transaction
              -> LexicoTreeItem   -- ^ Path-shaped lexicographic tree
              -> (LexicoTreeItem, Int) -- ^ Result : (path-shaped lexicographic tree representing the intersection 
                                     -- of transaction and input path , 0 if intersection not [] / sum of weights else)
suffInterSuiv lst w suiv =
    let (lstSuiv, weightSuiv) = getLstSuiv suiv
        inter = List.intersect lstSuiv lst
    in  if (inter /= [])
        then (createPath inter (weightSuiv+w), 0)
        else (Nil, weightSuiv+w)
        
{-|
  Collects all the nodes of lexicographic tree in a list of elements.
-}
getLstSuiv :: LexicoTreeItem -- ^ Path shaped lexicographic tree.
           -> ([Item], Int)              -- ^ Result : (list of elements in the path, sum of weights of nodes in the path)
getLstSuiv Nil = ([], 0)
getLstSuiv (Node e suiv Nil weight) =
    let (lst, w) = getLstSuiv suiv
    in  (e:lst, w + weight)

{-|
  Merge two lexicographic trees.
-}
mergeAlts :: LexicoTreeItem -- ^ Tree 1
             -> LexicoTreeItem         -- ^ Tree 2
             -> LexicoTreeItem         -- ^ Return : Tree 1 merged with Tree 2
mergeAlts Nil lt = lt
mergeAlts lt Nil = lt
mergeAlts (Node e1 suiv1 alt1 w1) (Node e2 suiv2 alt2 w2)
    | e1 < e2 = (Node e1 suiv1 (mergeAlts alt1 (Node e2 suiv2 alt2 w2)) w1)
    | e1 > e2 = (Node e2 suiv2 (mergeAlts (Node e1 suiv1 alt1 w1) alt2) w2)
    | e1 == e2 = (Node e1 (mergeAlts suiv1 suiv2) (mergeAlts alt1 alt2) (w1+w2))

------------------------------------------------------------------
