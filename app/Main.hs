module Main where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4

main :: IO ()
main = do
  readFile "app/day1.txt" >>= print . Day1.solvePart1
  readFile "app/day1.txt" >>= print . Day1.solvePart2
  readFile "app/day2.txt" >>= print . Day2.solvePart1
  readFile "app/day2.txt" >>= print . Day2.solvePart2
  readFile "app/day3.txt" >>= print . Day3.solvePart1
  readFile "app/day3.txt" >>= print . Day3.solvePart2
  readFile "app/day4.txt" >>= print . Day4.solvePart1
  readFile "app/day4.txt" >>= print . Day4.solvePart2
