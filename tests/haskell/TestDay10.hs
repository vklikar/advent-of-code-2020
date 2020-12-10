module TestDay10 where

import Day10
import Test.HUnit

example1 =
  [ 16,
    10,
    15,
    5,
    1,
    11,
    7,
    19,
    6,
    12,
    4
  ]

example2 =
  [ 28,
    33,
    18,
    42,
    31,
    14,
    46,
    20,
    48,
    47,
    24,
    23,
    49,
    45,
    19,
    38,
    39,
    11,
    1,
    32,
    25,
    35,
    8,
    17,
    7,
    9,
    4,
    2,
    34,
    10,
    3
  ]

testPart1 :: Test
testPart1 =
  TestList
    [ TestCase $ assertEqual "" 35 (getJoltDiffProduct example1),
      TestCase $ assertEqual "" 220 (getJoltDiffProduct example2)
    ]

testPart2 :: Test
testPart2 =
  TestList
    [ TestCase $ assertEqual "" 8 (getConnectionsCount example1),
      TestCase $ assertEqual "" 19208 (getConnectionsCount example2)
    ]

runTests :: IO Counts
runTests = runTestTT $ TestList [testPart1, testPart2]
