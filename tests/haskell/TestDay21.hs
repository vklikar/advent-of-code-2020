module TestDay21 where

import Day21
import Test.HUnit

example =
  unlines
    [ "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)",
      "trh fvjkl sbzzf mxmxvkd (contains dairy)",
      "sqjhc fvjkl (contains soy)",
      "sqjhc mxmxvkd sbzzf (contains fish)"
    ]

testPart1 :: Test
testPart1 = TestCase $ assertEqual "" 5 (solvePart1 example)

testPart2 :: Test
testPart2 = TestCase $ assertEqual "" "mxmxvkd,sqjhc,fvjkl" (solvePart2 example)

runTests :: IO Counts
runTests = runTestTT $ TestList [testPart1, testPart2]
