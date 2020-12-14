module TestLib where

import Lib
import Test.HUnit

testIntegerToDigits :: Test
testIntegerToDigits = TestCase $ assertEqual "" "000000000000000000000000000000001010" (integerToDigits 10)

testDigitsToInteger :: Test
testDigitsToInteger = TestCase $ assertEqual "" 10 (digitsToInteger "000000000000000000000000000000001010")

runTests :: IO Counts
runTests = runTestTT $ TestList [testIntegerToDigits, testDigitsToInteger]
