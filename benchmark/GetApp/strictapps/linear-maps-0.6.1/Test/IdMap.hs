  --
-- | Integrity test of the @linear-maps@ package.

module Test.IdMap where

import qualified Data.LinkMap.Tests
import qualified Data.Sequence.IdMap.Tests

-- | Runs all unit tests found in the @linear-maps@ package.

tests :: IO ()
tests = do
    putStrLn "------ Data.Sequence.IdMap.Tests"
    Data.Sequence.IdMap.Tests.tests
    putStrLn "------ Data.LinkMap.Tests"
    Data.LinkMap.Tests.tests
    return ()

