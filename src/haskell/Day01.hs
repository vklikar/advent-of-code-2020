module Day01 where

import Control.Monad

solvePart1 :: String -> Int
solvePart1 = puzzleProduct 2020 2 . parseInput

solvePart2 :: String -> Int
solvePart2 = puzzleProduct 2020 3 . parseInput

parseInput :: String -> [Int]
parseInput = map read . words

puzzleProduct :: Int -> Int -> [Int] -> Int
puzzleProduct result n xs = product $ head (filter ((== result) . sum) (replicateM n xs))
