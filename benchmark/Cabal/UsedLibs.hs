import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Distribution.Verbosity
import Distribution.Text
import System.Environment
import Data.Functor
import Control.Applicative

{- Useful fields: executable, hsSourceDirs, exposedModules, extraSrcFiles, 
 - libraries, extrlibs -}

-- library build dependents

main = do
    cabalFile <- (head <$> getArgs)
    cabalDescript <- flattenPackageDescription <$> readPackageDescription normal cabalFile
    putStr $ unlines $ map display (buildDepends cabalDescript)
