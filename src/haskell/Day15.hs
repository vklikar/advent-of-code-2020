module Day15 where

import Data.List.Split
import qualified Data.Map as M

solvePart1 :: String -> Int
solvePart1 = spokenNumber 2020 . parseInput

-- Too high RAM usage and takes too long to compute. Will probably have to use the State monad to
-- make it more efficient or somehow change the algorithm.
-- solvePart2 :: String -> Int
-- solvePart2 = spokenNumber 30000000 . parseInput

parseInput :: String -> [Int]
parseInput = map read . splitOn "," . head . lines

spokenNumber :: Int -> [Int] -> Int
spokenNumber = f (-1) 1 M.empty
  where
    f last' _ _ 0 _ = last'
    f last' i m n [] = f x' (i + 1) (M.alter (const $ Just i') last' m) (n - 1) []
      where
        i' = i - 1
        x' = i' - M.findWithDefault i' last' m
    f _ i m n (x : xs) = f x (i + 1) (M.alter (const $ Just i) x m) (n - 1) xs
