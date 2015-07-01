import Test.Tasty ( TestTree, defaultMain )

import Xml ( xml_tests )

tests :: TestTree
tests = xml_tests

main :: IO ()
main = defaultMain tests
