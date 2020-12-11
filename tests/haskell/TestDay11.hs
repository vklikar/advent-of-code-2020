module TestDay11 where

import Day11
import Test.HUnit

example =
  unlines
    [ "L.LL.LL.LL",
      "LLLLLLL.LL",
      "L.L.L..L..",
      "LLLL.LL.LL",
      "L.LL.LL.LL",
      "L.LLLLL.LL",
      "..L.L.....",
      "LLLLLLLLLL",
      "L.LLLLLL.L",
      "L.LLLLL.LL"
    ]

testPart1 :: Test
testPart1 = TestCase $ assertEqual "" 37 (solvePart1 example)

testPart2 :: Test
testPart2 = TestCase $ assertEqual "" 26 (solvePart2 example)

runTests :: IO Counts
runTests = runTestTT $ TestList [testPart1, testPart2]
