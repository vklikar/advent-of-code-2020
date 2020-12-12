module TestDay12 where

import Day12
import Test.HUnit

example =
  unlines
    [ "F10",
      "N3",
      "F7",
      "R90",
      "F11"
    ]

testRotate :: Test
testRotate =
  TestList
    [ TestCase $ assertEqual "" (4, -10) (rotate (10, 4) 90),
      TestCase $ assertEqual "" (-10, -4) (rotate (10, 4) 180),
      TestCase $ assertEqual "" (-4, 10) (rotate (10, 4) 270)
    ]

testPart1 :: Test
testPart1 = TestCase $ assertEqual "" 25 (solvePart1 example)

testPart2 :: Test
testPart2 = TestCase $ assertEqual "" 286 (solvePart2 example)

runTests :: IO Counts
runTests = runTestTT $ TestList [testRotate, testPart1, testPart2]
