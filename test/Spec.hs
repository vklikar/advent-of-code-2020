module Main (main) where

import Test.HUnit
import TestDay1
import TestDay2

main :: IO Counts
main = do
  TestDay1.runTests
  TestDay2.runTests
