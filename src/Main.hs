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
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18

main :: IO ()
main = do
  readFile "data/day01.txt" >>= print' "01a" . Day01.solvePart1
  readFile "data/day01.txt" >>= print' "01b" . Day01.solvePart2
  readFile "data/day02.txt" >>= print' "02a" . Day02.solvePart1
  readFile "data/day02.txt" >>= print' "02b" . Day02.solvePart2
  readFile "data/day03.txt" >>= print' "03a" . Day03.solvePart1
  readFile "data/day03.txt" >>= print' "03b" . Day03.solvePart2
  readFile "data/day04.txt" >>= print' "04a" . Day04.solvePart1
  readFile "data/day04.txt" >>= print' "04b" . Day04.solvePart2
  readFile "data/day05.txt" >>= print' "05a" . Day05.solvePart1
  readFile "data/day05.txt" >>= print' "05b" . Day05.solvePart2
  readFile "data/day06.txt" >>= print' "06a" . Day06.solvePart1
  readFile "data/day06.txt" >>= print' "06b" . Day06.solvePart2
  readFile "data/day07.txt" >>= print' "07a" . Day07.solvePart1
  readFile "data/day07.txt" >>= print' "07b" . Day07.solvePart2
  readFile "data/day08.txt" >>= print' "08a" . Day08.solvePart1
  readFile "data/day08.txt" >>= print' "08b" . Day08.solvePart2
  readFile "data/day09.txt" >>= print' "09a" . Day09.solvePart1
  readFile "data/day09.txt" >>= print' "09b" . Day09.solvePart2
  readFile "data/day10.txt" >>= print' "10a" . Day10.solvePart1
  readFile "data/day10.txt" >>= print' "10b" . Day10.solvePart2
  readFile "data/day11.txt" >>= print' "11a" . Day11.solvePart1
  readFile "data/day11.txt" >>= print' "11b" . Day11.solvePart2
  readFile "data/day12.txt" >>= print' "12a" . Day12.solvePart1
  readFile "data/day12.txt" >>= print' "12b" . Day12.solvePart2
  readFile "data/day13.txt" >>= print' "13a" . Day13.solvePart1
  readFile "data/day13.txt" >>= print' "13b" . Day13.solvePart2
  readFile "data/day14.txt" >>= print' "14a" . Day14.solvePart1
  readFile "data/day14.txt" >>= print' "14b" . Day14.solvePart2
  readFile "data/day15.txt" >>= print' "15a" . Day15.solvePart1
  -- readFile "data/day15.txt" >>= print' "15b" . Day15.solvePart2
  readFile "data/day16.txt" >>= print' "16a" . Day16.solvePart1
  readFile "data/day16.txt" >>= print' "16b" . Day16.solvePart2
  readFile "data/day17.txt" >>= print' "17a" . Day17.solvePart1
  readFile "data/day17.txt" >>= print' "17b" . Day17.solvePart2
  readFile "data/day18.txt" >>= Day18.solvePart1 >>= print' "18a"
  readFile "data/day18.txt" >>= Day18.solvePart2 >>= print' "18b"

print' :: (Show a) => String -> a -> IO ()
print' n result = do
  putStr $ n ++ ": "
  print result
