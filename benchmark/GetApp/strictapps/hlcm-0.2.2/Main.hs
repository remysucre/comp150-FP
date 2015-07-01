-- HLCM
-- (c) Alexandre Termier, 2009-2010
-- Original LCM algorithm from Takaki Uno and Hiroki Arimura.
-- 
-- 
--
-- See the README file for installation, usage, and details.
-- See the LICENSE file for licensing.

{-|
Main program to invoke the LCM algorithm and
compute closed frequent itemsets.

Usage :

@hlcm /input_data type_of_input_file support_threshold/@

Type of input file can be @csv@ or @num@.

Support threshold correspond to the minimum number of times that the frequent pattern must appear in data to be reported (i.e., the minimum number of transactions in which it is included).

Results are dumped on stdout.

For both input types, input data must be a file with one line per transaction.


/ * CSV data/

In case of @csv@ data : items in transactions are strings separated by commas.

Example: the file @Data\/simple.csv@ contains:

> bread,butter
> butter,chocolate,bread
> chocolate

To know the itemsets that appear at least 2 times, invoke hlcm with : 

@hlcm Data\/simple.csv csv 2@

Result : 

> HLCM, (c) Alexandre Termier 2010, from original Takeaki Uno and Hiroki Arimura LCM algorithm.
> Input file was a CSV file.
>
> frequency: 2  cfis: ["chocolate"]
> frequency: 2  cfis: ["bread","butter"]
> 
> There are 2 closed frequent itemsets.

As expected, @[chocolate]@ appears two times but is not frequently appearing with other items. 
On the other hand, the itemset @[bread,butter]@ appears twice.

/ * Numerical data/

In case of @num@ data : items in the transactions are integers separated by spaces.

Example: the file @Data\/simple.num@ contains:

> 1 2
> 2 3 1
> 3

As you can see, it is a simple transformation of @simple.csv@ replacing text items by integers.

To know the itemsets that appear at least 2 times, invoke hlcm with : 

@hlcm Data\/simple.num num 2@

WARNING : do not feed HLCM with numerical datasets where an item is repeated several times in a transaction.
The results would be wrong.

Result: 

> HLCM, (c) Alexandre Termier 2010, from original Takeaki Uno and Hiroki Arimura LCM algorithm.
> Input file was a NUMERIC file.
> 
> frequency: 2  cfis: [3]
> frequency: 2  cfis: [1,2]
> 
> There are 2 closed frequent itemsets.



-}
module Main(main) where



import HLCM
import System( getArgs )

import Text.CSV.ByteString -- get with cabal install bytestring-csv


import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as Lst

import qualified Data.ByteString.Char8 as L



{-|
Main program, parses command line, calls LCM and dumps output nicely.
-}
main :: IO ()
main = do
  args <- getArgs
  (dataFile, fileType, _thres) <- return (args !! 0, args !! 1, args !! 2)
  stringMatrix <- L.readFile dataFile
  let thres = read _thres::Frequency
      (transMatrix, labelToItem) = if (fileType == "csv")
                                      then readCSVFile stringMatrix
                                      else ([[]],Map.empty)  -- never evaluated     
      cfis = if (fileType == "csv")
                then runLCMmatrix transMatrix thres
                else runLCMstring  stringMatrix thres  in
    do
      putStrLn "HLCM, (c) Alexandre Termier 2010, from original Takeaki Uno and Hiroki Arimura LCM algorithm."
      if (fileType == "csv")
         then putStrLn "Input file was a CSV file.\n"
         else putStrLn "Input file was a NUMERIC file.\n"
      showNiceCFIS cfis labelToItem
      
  
{-|
  Read input given in a CSV file.
-}
readCSVFile :: L.ByteString -> ([[Item]],Map.Map Item L.ByteString)
readCSVFile dataFile = 
  case (parseCSV dataFile) of 
    Nothing      -> ([[]],Map.empty)
    Just csvData -> let labelsSet = Set.fromList $ (concat csvData)
                        itmWithLabelLst = zip (Set.toList labelsSet) [1..]
                        mapLabel2itm = Map.fromList itmWithLabelLst
                        mapItm2Label = Map.fromList $ zip [1..] (Set.toList labelsSet)
                    in  (map (\t -> Lst.nub $ map (\it -> Map.findWithDefault (-1) it mapLabel2itm) t) csvData, mapItm2Label)

      
{-|
  Pretty printing of the results.
-}
showNiceCFIS :: [[Item]] -> Map.Map Item L.ByteString -> IO ()
showNiceCFIS l m 
  | Map.null m = showNiceCFIS' l 0
  | otherwise  = showNiceCFIS'' l m 0

showNiceCFIS' :: [[Item]] -> Int -> IO ()
showNiceCFIS' [] counter = putStrLn ("\nThere are "++(show counter)++" closed frequent itemsets.")
showNiceCFIS' (t:ts) counter =
    do
      putStrLn ("frequency: "++(show (head t))++"  cfis: "++(show (tail t)))
      showNiceCFIS' ts (counter+1)

showNiceCFIS'' :: [[Item]] -> Map.Map Item L.ByteString -> Int -> IO ()
showNiceCFIS'' [] _ counter = putStrLn ("\nThere are "++(show counter)++" closed frequent itemsets.")
showNiceCFIS'' (t:ts) m counter =
    do
      putStrLn ("frequency: "++(show (head t))++"  cfis: "++(show (map (\it -> Map.findWithDefault L.empty it m) (tail t))))
      showNiceCFIS'' ts m (counter+1)