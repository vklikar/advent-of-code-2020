module TestDay9 where

import Day9
import Test.HUnit

example =
  [ 35,
    20,
    15,
    25,
    47,
    40,
    62,
    55,
    65,
    95,
    102,
    117,
    150,
    182,
    127,
    219,
    299,
    277,
    309,
    576
  ]

testFindNumberWithoutSum :: Test
testFindNumberWithoutSum = TestCase $ assertEqual "" 127 (findNumberWithoutSum 5 example)

testFindWeakness :: Test
testFindWeakness = TestCase $ assertEqual "" 62 (findWeakness 127 example)

runTests :: IO Counts
runTests = runTestTT $ TestList [testFindNumberWithoutSum, testFindWeakness]
