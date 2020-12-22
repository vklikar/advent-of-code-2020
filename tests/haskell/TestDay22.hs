module TestDay22 where

import Day22
import Test.HUnit

example =
  unlines
    [ "Player 1:",
      "9",
      "2",
      "6",
      "3",
      "1",
      "",
      "Player 2:",
      "5",
      "8",
      "4",
      "7",
      "10"
    ]

testPart1 :: Test
testPart1 = TestCase $ assertEqual "" 306 (solvePart1 example)

testPart2 :: Test
testPart2 = TestCase $ assertEqual "" 291 (solvePart2 example)

runTests :: IO Counts
runTests = runTestTT $ TestList [testPart1, testPart2]
