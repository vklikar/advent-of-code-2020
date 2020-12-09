module Main (main) where

import Test.HUnit
import TestDay1
import TestDay2
import TestDay3
import TestDay4
import TestDay5
import TestDay6
import TestDay7
import TestDay8
import TestDay9

main :: IO Counts
main = do
  TestDay1.runTests
  TestDay2.runTests
  TestDay3.runTests
  TestDay4.runTests
  TestDay5.runTests
  TestDay6.runTests
  TestDay7.runTests
  TestDay8.runTests
  TestDay9.runTests
