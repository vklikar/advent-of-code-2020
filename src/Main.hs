module Main where

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14

main :: IO ()
main = do
  readFile "data/day01.txt" >>= print . Day01.solvePart1
  readFile "data/day01.txt" >>= print . Day01.solvePart2
  readFile "data/day02.txt" >>= print . Day02.solvePart1
  readFile "data/day02.txt" >>= print . Day02.solvePart2
  readFile "data/day03.txt" >>= print . Day03.solvePart1
  readFile "data/day03.txt" >>= print . Day03.solvePart2
  readFile "data/day04.txt" >>= print . Day04.solvePart1
  readFile "data/day04.txt" >>= print . Day04.solvePart2
  readFile "data/day05.txt" >>= print . Day05.solvePart1
  readFile "data/day05.txt" >>= print . Day05.solvePart2
  readFile "data/day06.txt" >>= print . Day06.solvePart1
  readFile "data/day06.txt" >>= print . Day06.solvePart2
  readFile "data/day07.txt" >>= print . Day07.solvePart1
  readFile "data/day07.txt" >>= print . Day07.solvePart2
  readFile "data/day08.txt" >>= print . Day08.solvePart1
  readFile "data/day08.txt" >>= print . Day08.solvePart2
  readFile "data/day09.txt" >>= print . Day09.solvePart1
  readFile "data/day09.txt" >>= print . Day09.solvePart2
  readFile "data/day10.txt" >>= print . Day10.solvePart1
  readFile "data/day10.txt" >>= print . Day10.solvePart2
  readFile "data/day11.txt" >>= print . Day11.solvePart1
  readFile "data/day11.txt" >>= print . Day11.solvePart2
  readFile "data/day12.txt" >>= print . Day12.solvePart1
  readFile "data/day12.txt" >>= print . Day12.solvePart2
  readFile "data/day13.txt" >>= print . Day13.solvePart1
  readFile "data/day13.txt" >>= print . Day13.solvePart2
  readFile "data/day14.txt" >>= print . Day14.solvePart1
  readFile "data/day14.txt" >>= print . Day14.solvePart2
