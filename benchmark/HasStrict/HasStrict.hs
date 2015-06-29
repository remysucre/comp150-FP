import System.FilePath.Find
import System.Environment 
import System.Process
import Control.Monad
import Data.Functor
import Control.Applicative
import Data.List
import FindHs
import HasBang

main :: IO ()
main = do
    filePath <- head <$> getArgs
    srcs <- findHs filePath
    srcsWBang <- filterM hasBang srcs 
    putStrLn $ concat srcsWBang
