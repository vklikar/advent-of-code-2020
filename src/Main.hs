module Main where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day5Amendment
import qualified Day6

main :: IO ()
main = do
  readFile "data/day1.txt" >>= print . Day1.solvePart1
  readFile "data/day1.txt" >>= print . Day1.solvePart2
  readFile "data/day2.txt" >>= print . Day2.solvePart1
  readFile "data/day2.txt" >>= print . Day2.solvePart2
  readFile "data/day3.txt" >>= print . Day3.solvePart1
  readFile "data/day3.txt" >>= print . Day3.solvePart2
  readFile "data/day4.txt" >>= print . Day4.solvePart1
  readFile "data/day4.txt" >>= print . Day4.solvePart2
  readFile "data/day5.txt" >>= print . Day5.solvePart1
  readFile "data/day5.txt" >>= print . Day5.solvePart2
  readFile "data/day5.txt" >>= print . Day5Amendment.solvePart1
  readFile "data/day5.txt" >>= print . Day5Amendment.solvePart2
  readFile "data/day6.txt" >>= print . Day6.solvePart1
  readFile "data/day6.txt" >>= print . Day6.solvePart2
