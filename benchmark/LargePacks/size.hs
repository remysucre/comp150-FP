--import System.FilePath.Find
import System.Directory
import Control.Applicative
import Data.Functor

main = do 
    --dir <- head <$> getArgs
    --srcs <- System.FilePath.Find.find always (fileSize >? 300000000) dir -- how to get rid of module names
    --putStrLn $ unlines srcs
    size <- fileSize "."
    putStrLn $ show size
