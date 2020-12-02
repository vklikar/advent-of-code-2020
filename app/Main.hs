module Main where

import Day1
import Day2

main :: IO ()
main = do
  readFile "app/day1.txt" >>= print . Day1.solvePart1
  readFile "app/day1.txt" >>= print . Day1.solvePart2
  readFile "app/day2.txt" >>= print . Day2.solvePart1
  readFile "app/day2.txt" >>= print . Day2.solvePart2
