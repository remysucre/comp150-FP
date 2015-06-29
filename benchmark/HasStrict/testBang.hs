import System.Environment
import HasBang
import Control.Applicative
import Data.Functor

main = do
    fp <- head <$> getArgs
    result <- hasBang fp
    putStrLn $ show result
