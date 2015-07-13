-- Search named directory for .hs source files

import System.FilePath.Find
import System.Environment 
import Data.Functor
import Control.Applicative

main :: IO ()
main = do
    filePath <- head <$> getArgs
    srcs <- System.FilePath.Find.find always (extension ==? ".hs") filePath -- how to get rid of module names
    putStrLn $ unlines srcs
