module Main where

import qualified Day1
import qualified Day10
import qualified Day11
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9

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
  readFile "data/day6.txt" >>= print . Day6.solvePart1
  readFile "data/day6.txt" >>= print . Day6.solvePart2
  readFile "data/day7.txt" >>= print . Day7.solvePart1
  readFile "data/day7.txt" >>= print . Day7.solvePart2
  readFile "data/day8.txt" >>= print . Day8.solvePart1
  readFile "data/day8.txt" >>= print . Day8.solvePart2
  readFile "data/day9.txt" >>= print . Day9.solvePart1
  readFile "data/day9.txt" >>= print . Day9.solvePart2
  readFile "data/day10.txt" >>= print . Day10.solvePart1
  readFile "data/day10.txt" >>= print . Day10.solvePart2
  readFile "data/day11.txt" >>= print . Day11.solvePart1
  readFile "data/day11.txt" >>= print . Day11.solvePart2
