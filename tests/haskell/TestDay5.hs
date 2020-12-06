module TestDay5 where

import Day5
import Test.HUnit

testGetHalf :: Test
testGetHalf =
  TestList
    [ TestCase $ assertEqual "" (0, 63) (getHalf 0 127 0),
      TestCase $ assertEqual "" (32, 63) (getHalf 0 63 1),
      TestCase $ assertEqual "" (32, 47) (getHalf 32 63 0),
      TestCase $ assertEqual "" (40, 47) (getHalf 32 47 1),
      TestCase $ assertEqual "" (44, 47) (getHalf 40 47 1),
      TestCase $ assertEqual "" (44, 45) (getHalf 44 47 0),
      TestCase $ assertEqual "" (44, 44) (getHalf 44 45 0)
    ]

testPoistion :: Test
testPoistion =
  TestList
    [ TestCase $ assertEqual "" 44 (getPosition 0 127 [0, 1, 0, 1, 1, 0, 0]),
      TestCase $ assertEqual "" 5 (getPosition 0 7 [1, 0, 1])
    ]

testSeatId :: Test
testSeatId =
  TestList
    [ TestCase $ assertEqual "" 357 (getSeatId "FBFBBFFRLR"),
      TestCase $ assertEqual "" 567 (getSeatId "BFFFBBFRRR"),
      TestCase $ assertEqual "" 119 (getSeatId "FFFBBBFRRR"),
      TestCase $ assertEqual "" 820 (getSeatId "BBFFBBFRLL")
    ]

runTests :: IO Counts
runTests = runTestTT $ TestList [testGetHalf, testPoistion, testSeatId]
