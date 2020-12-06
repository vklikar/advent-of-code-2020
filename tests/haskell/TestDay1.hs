module TestDay1 where

import Day1
import Test.HUnit

testPart2 :: Test
testPart2 = TestList [TestCase $ assertEqual "" 514579 (puzzleProduct 2020 2 [1721, 979, 366, 299, 675, 1456])]

testPart3 :: Test
testPart3 = TestList [TestCase $ assertEqual "" 241861950 (puzzleProduct 2020 3 [1721, 979, 366, 299, 675, 1456])]

runTests :: IO Counts
runTests = runTestTT $ TestList [testPart2, testPart3]
