module TestDay13 where

import Day13
import Test.HUnit

testPart1 :: Test
testPart1 = TestCase $ assertEqual "" 295 (solvePart1' (939, [7, 13, 0, 0, 59, 0, 31, 19]))

testPart2 :: Test
testPart2 =
  TestList
    [ TestCase $ assertEqual "" 1068781 (solvePart2' 0 ([7, 13, 0, 0, 59, 0, 31, 19])),
      TestCase $ assertEqual "" 3417 (solvePart2' 0 ([17, 0, 13, 19])),
      TestCase $ assertEqual "" 754018 (solvePart2' 0 ([67, 7, 59, 61])),
      TestCase $ assertEqual "" 779210 (solvePart2' 0 ([67, 0, 7, 59, 61])),
      TestCase $ assertEqual "" 1261476 (solvePart2' 0 ([67, 7, 0, 59, 61])),
      TestCase $ assertEqual "" 1202161486 (solvePart2' 0 ([1789, 37, 47, 1889]))
    ]

runTests :: IO Counts
runTests = runTestTT $ TestList [testPart1, testPart2]
