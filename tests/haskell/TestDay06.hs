module TestDay06 where

import Day06
import Test.HUnit

example =
  unlines
    [ "abc",
      "",
      "a",
      "b",
      "c",
      "",
      "ab",
      "ac",
      "",
      "a",
      "a",
      "a",
      "a",
      "",
      "b"
    ]

testPart1 :: Test
testPart1 = TestList [TestCase $ assertEqual "" 11 (solvePart1 example)]

testPart2 :: Test
testPart2 = TestList [TestCase $ assertEqual "" 6 (solvePart2 example)]

runTests :: IO Counts
runTests = runTestTT $ TestList [testPart1, testPart2]
