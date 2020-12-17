module Main (main) where

import Test.HUnit
import TestDay01
import TestDay02
import TestDay03
import TestDay04
import TestDay05
import TestDay06
import TestDay07
import TestDay08
import TestDay09
import TestDay10
import TestDay11
import TestDay12
import TestDay13
import TestDay14
import TestDay15
import TestDay16
import TestDay17
import TestLib

main :: IO Counts
main = do
  TestLib.runTests
  TestDay01.runTests
  TestDay02.runTests
  TestDay03.runTests
  TestDay04.runTests
  TestDay05.runTests
  TestDay06.runTests
  TestDay07.runTests
  TestDay08.runTests
  TestDay09.runTests
  TestDay10.runTests
  TestDay11.runTests
  TestDay12.runTests
  TestDay13.runTests
  TestDay14.runTests
  TestDay15.runTests
  TestDay16.runTests
  TestDay17.runTests
