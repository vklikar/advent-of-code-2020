module TestDay5 where

import Day5
import Test.HUnit

testParseInput :: Test
testParseInput =
  TestList
    [ TestCase $ assertEqual "" 567 (head $ parseInput "BFFFBBFRRR"),
      TestCase $ assertEqual "" 119 (head $ parseInput "FFFBBBFRRR"),
      TestCase $ assertEqual "" 820 (head $ parseInput "BBFFBBFRLL")
    ]

runTests :: IO Counts
runTests = runTestTT $ TestList [testParseInput]
