module TestDay2 where

import Day2
import Test.HUnit

-- Part 1
testValidAtLeast :: Test
testValidAtLeast = TestList [TestCase $ assertEqual "" True (passwordsValidByRepetition 1 3 'a' "abcde")]

testValidAtMost :: Test
testValidAtMost = TestList [TestCase $ assertEqual "" True (passwordsValidByRepetition 2 9 'c' "ccccccccc")]

testInvalid :: Test
testInvalid = TestList [TestCase $ assertEqual "" False (passwordsValidByRepetition 1 3 'b' "cdefg")]

-- Part 2
testValidFirstPosition :: Test
testValidFirstPosition = TestList [TestCase $ assertEqual "" True (passwordsValidByPosition 1 3 'a' "abcde")]

testValidSecondPoistion :: Test
testValidSecondPoistion = TestList [TestCase $ assertEqual "" True (passwordsValidByPosition 1 3 'a' "cbade")]

testInvalidBothPositions :: Test
testInvalidBothPositions = TestList [TestCase $ assertEqual "" False (passwordsValidByPosition 2 9 'c' "ccccccccc")]

testInvalidNoPosition :: Test
testInvalidNoPosition = TestList [TestCase $ assertEqual "" False (passwordsValidByPosition 1 3 'b' "cdefg")]

testValidMoreOccurrences :: Test
testValidMoreOccurrences = TestList [TestCase $ assertEqual "" True (passwordsValidByPosition 1 3 'a' "baacc")]

testInvalidMoreOccurrences :: Test
testInvalidMoreOccurrences = TestList [TestCase $ assertEqual "" False (passwordsValidByPosition 2 3 'a' "baacc")]

runTests :: IO Counts
runTests =
  runTestTT $
    TestList
      [ testValidFirstPosition,
        testValidSecondPoistion,
        testInvalidBothPositions,
        testInvalidNoPosition,
        testValidMoreOccurrences,
        testInvalidMoreOccurrences
      ]
