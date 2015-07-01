module Main
where

import LTR.Transaction (_testTransaction)
import Mhailist.BuildMessage (_testBuildMessage)
import Mhailist.Address (_testAddress)
import Mhailist.List (_testList)
import Mhailist.Message (_testMessage)
import Util.String (_testString)
import UnitTest (_unitTest)

main = do _testTransaction
          _testBuildMessage
          _testAddress
          _testList
          _testMessage
          _testString
          _unitTest
