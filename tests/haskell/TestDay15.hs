module TestDay15 where

import Day15
import Test.HUnit

testSpokenNumber :: Test
testSpokenNumber = TestCase $ assertEqual "" 436 (spokenNumber 2020 [0, 3, 6])

runTests :: IO Counts
runTests = runTestTT $ TestList [testSpokenNumber]
