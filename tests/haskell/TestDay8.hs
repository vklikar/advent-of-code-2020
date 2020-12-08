module TestDay8 where

import Day8
import Test.HUnit

example =
  unlines
    [ "nop +0",
      "acc +1",
      "jmp +4",
      "acc +3",
      "jmp -3",
      "acc -99",
      "acc +1",
      "jmp -4",
      "acc +6"
    ]

testPart1 :: Test
testPart1 = TestCase $ assertEqual "" 5 (solvePart1 example)

testPart2 :: Test
testPart2 = TestCase $ assertEqual "" 8 (solvePart2 example)

runTests :: IO Counts
runTests = runTestTT $ TestList [testPart1, testPart2]
