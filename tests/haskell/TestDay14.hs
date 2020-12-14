module TestDay14 where

import Day14
import Test.HUnit

example1 =
  unlines
    [ "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
      "mem[8] = 11",
      "mem[7] = 101",
      "mem[8] = 0"
    ]

example2 =
  unlines
    [ "mask = 000000000000000000000000000000X1001X",
      "mem[42] = 100",
      "mask = 00000000000000000000000000000000X0XX",
      "mem[26] = 1"
    ]

testApplyMask1 :: Test
testApplyMask1 =
  TestCase $
    assertEqual
      ""
      "000000000000000000000000000001001001"
      (applyMask1 "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" "000000000000000000000000000000001011")

testPart1 :: Test
testPart1 = TestCase $ assertEqual "" 165 (solvePart1 example1)

testPart2 :: Test
testPart2 = TestCase $ assertEqual "" 208 (solvePart2 example2)

runTests :: IO Counts
runTests = runTestTT $ TestList [testApplyMask1, testPart1, testPart2]
