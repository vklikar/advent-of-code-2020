module Day03 where

import Lib (count)

solvePart1 :: String -> Int
solvePart1 = traverseMap [(3, 1)] . parseInput

solvePart2 :: String -> Int
solvePart2 = traverseMap [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)] . parseInput

parseInput :: String -> [String]
parseInput = lines

traverseMap :: [(Int, Int)] -> [String] -> Int
traverseMap slopes input =
  product [count '#' $ adjust right down (expandMap input) | (right, down) <- slopes]

expandMap :: [String] -> [String]
expandMap = map cycle

adjust :: Int -> Int -> [String] -> String
adjust right down input = map head $ adjust' right down (drop down input) 1

adjust' :: Int -> Int -> [String] -> Int -> [String]
adjust' _ _ [] _ = []
adjust' right down (x : xs) depth =
  drop (right * depth) x : adjust' right down (drop (down - 1) xs) (depth + 1)
