module TestDay16 where

import Day16
import Test.HUnit

example1 =
  unlines
    [ "class: 1-3 or 5-7",
      "row: 6-11 or 33-44",
      "seat: 13-40 or 45-50",
      "",
      "your ticket:",
      "7,1,14",
      "",
      "nearby tickets:",
      "7,3,47",
      "40,4,50",
      "55,2,20",
      "38,6,12"
    ]

example2 =
  unlines
    [ "class: 0-1 or 4-19",
      "row: 0-5 or 8-19",
      "seat: 0-13 or 16-19",
      "",
      "your ticket:",
      "11,12,13",
      "",
      "nearby tickets:",
      "3,9,18",
      "15,1,5",
      "5,14,9"
    ]

testPart1 :: Test
testPart1 = TestCase $ assertEqual "" 71 (solvePart1 example1)

testPart2 :: Test
testPart2 = TestCase $ assertEqual "" (11 * 12 * 13) (solvePart2' (\_ _ -> True) $ parseInput example2)

runTests :: IO Counts
runTests = runTestTT $ TestList [testPart1, testPart2]
