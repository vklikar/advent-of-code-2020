module TestDay23 where

import Day23
import Test.HUnit

example = 389125467

testPart1 :: Test
testPart1 = TestCase $ assertEqual "" 67384529 (solvePart1 example)

-- testPart2 :: Test
-- testPart2 = TestCase $ assertEqual "" 149245887792 (solvePart2 example)

runTests :: IO Counts
runTests = runTestTT $ TestList [testPart1]
