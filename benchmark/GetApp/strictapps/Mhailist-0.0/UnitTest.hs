{-# OPTIONS_GHC #-}
module UnitTest (

    runTests,
    approximate, approx, approxE,
    (@?~=), (~?~=),

    -- From Test.HUnit
    Test (..),
    test, (~?=), (~=?),
    assertEqual, (@=?),
    (~:),

    -- * Debugging assistance
    module Debug.Trace,

    _unitTest,

) where

import Debug.Trace
import Numeric (floatToDigits)
import System.Exit
import Test.HUnit

------------------------------------------------------------

runTests :: Test -> IO Counts
runTests = runTestTT

------------------------------------------------------------

{-
01:22 <quicksilver> it could be in some lib on hackage, though.
01:23 <quicksilver> you could knock up some funky syntax so you could write:
01:23 <quicksilver> (x + y - z) =~= Tolerance 0.0001 6.315
01:23 <quicksilver> and then define "around = Tolerance 0.000001"
01:23 <quicksilver> to write
01:23 <quicksilver> (x+y-z) =~= around 6.315
-}

---------

data Approximation = Approximation ([Int], Int)
    deriving (Show, Read, Eq)

-- | Produce an approximation of n to the given number of significant digits.
approximate :: RealFloat n => Int -> n -> Approximation
approximate significant n =
    let (digits, exponent) = (floatToDigits 10 n)
        (sigdigs, rest) = splitAt significant $ digits ++ repeat 0
        (most, last) = splitAt (significant - 1) sigdigs
     in if head rest < 5
           then Approximation (sigdigs, exponent)
           else Approximation (most ++ [(head last + 1)], exponent)

-- | Produce an approximation of n to the number of significant digits in n.
approx :: RealFloat n => n -> Approximation
approx n = Approximation (floatToDigits 10 n)

-- | Produce an approximation of n to the number of significant digits in n,
-- but scaled with the given exponent.
approxE :: RealFloat n => n -> Int -> Approximation
approxE m exp = Approximation (digits, exp)
    where (digits, _) = (floatToDigits 10 m)

sigdigs :: Approximation -> Int
sigdigs (Approximation (digits, _)) = length digits

infix 1 @?~=, ~?~=

-- | Assert that the RealFloat on the left is the same as the approximation
-- on the right, to the accuracy of the approximation.
--
(@?~=) :: (RealFloat n) => n -> Approximation -> Assertion
actual @?~= expected =
    let actualSigdigs = approximate (sigdigs expected) actual
     in actualSigdigs == expected @?
         "Expected: " ++ (show expected) ++ " Actual: " ++ (show actualSigdigs)

-- | Create a Test to verify that the RealFloat on the left is equal to the
-- Approximation on the right, to the approximation's number of significant
-- digits. The approxmation is usually created with approx or approxE.
--
(~?~=) :: (RealFloat n) => n -> Approximation -> Test
actual ~?~= expected = TestCase (actual @?~= expected)

---------

_unitTest = runTests $ test
    [ "approx_long"  ~: approximate 3 3.14159 ~?= Approximation ([3,1,4], 1)
    , "approx_short" ~: approximate 5 7       ~?= Approximation ([7,0,0,0,0], 1)
    , "approx_exp"   ~: approximate 4 8890123 ~?= Approximation ([8,8,9,0], 7)
    , "approx_round" ~: approximate 4 1.2345  ~?= Approximation ([1,2,3,5], 1)
    , "approx_1a"    ~: approx 3              ~?= Approximation ([3], 1)
    , "approx_1b"    ~: approx 300            ~?= Approximation ([3], 3)
    , "approx_negex" ~: approx 0.00378        ~?= Approximation ([3, 7, 8], -2)
    , "approx_small" ~: approxE 2.1 (-3)      ~?= Approximation ([2, 1], -3)
    , "approx_eq_1"  ~: 3.141592654          ~?~= approx 3
    , "approx_round" ~: 3.141592654          ~?~= approx 3.142
    ]

