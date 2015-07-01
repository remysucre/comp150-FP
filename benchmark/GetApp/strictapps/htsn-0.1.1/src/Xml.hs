-- | Minimal XML functionality needed to parse each document's
--   XML_File_ID.
--
module Xml (
  parse_xmlfid,
  -- * Tests
  xml_tests )
where

import Data.Either.Utils ( maybeToEither )
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( (@?=), Assertion, testCase )
import Text.Read ( readMaybe )
import Text.XML.HXT.Core (
  (>>>),
  (/>),
  getChildren,
  getText,
  hasName,
  runLA,
  xreadDoc )


-- | A tiny parser written in HXT to extract the \"XML_File_ID\"
--   element from a document. If we fail to parse an XML_File_ID, we
--   return the reason wrapped in a 'Left' constructor. The reason
--   should be one of two things:
--
--     1. No XML_File_ID elements were found.
--
--     2. An XML_File_ID element was found, but it could not be read
--        into an Integer.
--
--   We use an Either rather than a Maybe because we do expect some
--   non-integer XML_File_IDs. In the examples, you will see
--   NHL_DepthChart_XML.XML with an XML_File_ID of \"49618.61\" and
--   CFL_Boxscore_XML1.xml with an XML_File_ID of
--   \"R28916\". According to Brijesh Patel of TSN, these are special
--   category files and not part of the usual feed.
--
--   We want to report them differently, \"just in case.\"
--
parse_xmlfid :: String -- ^ The XML Document
             -> Either String Integer
parse_xmlfid doc =
  case parse_results of
    []    -> Left "No XML_File_ID elements found."
    (x:_) -> x
  where
    parse :: String -> [String]
    parse =
      runLA (xreadDoc
               >>> hasName "message"
               />  hasName "XML_File_ID"
               >>> getChildren
               >>> getText)

    read_either_integer :: String -> Either String Integer
    read_either_integer s =
      let msg = "Could not parse XML_File_ID " ++ s ++ " as an integer."
      in
        maybeToEither msg (readMaybe s)

    elements = parse doc
    parse_results = map read_either_integer elements


--
-- Tasty Tests
--

-- | A list of all tests for this module.
--
xml_tests :: TestTree
xml_tests =
  testGroup
    "XML tests"
    [ xml_file_id_tests ]


-- | Ensure that we parse the correct XML_File_ID out of some known
--   examples.
--
xml_file_id_tests :: TestTree
xml_file_id_tests =
  testCase "XML_File_ID is parsed correctly" $ do
    let xmlfids = ["19908216", "19908216", "19908245", "19908246", "19908247"]
    mapM_ check xmlfids
  where
    check :: String -> Assertion
    check xmlfid = do
      xml <- readFile ("test/xml/" ++ xmlfid ++ ".xml")
      let actual = parse_xmlfid xml
      -- The maybeToEither should always succeed here, so the error
      -- message goes unused.
      let expected = maybeToEither "derp" (readMaybe xmlfid)
      actual @?= expected
