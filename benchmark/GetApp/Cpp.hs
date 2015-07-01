import Language.Preprocessor.Cpphs
import System.Environment
import Control.Applicative
import Data.Functor


main = do 
    fp <- head <$> getArgs
    fc <- readFile fp
    out <- runCpphs defaultCpphsOptions fp fc
    putStrLn out
