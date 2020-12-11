module Day06 where

import Data.List
import Data.List.Split

solvePart1 :: String -> Int
solvePart1 = sum . map (length . nub . concat) . parseInput

solvePart2 :: String -> Int
solvePart2 = sum . map (length . foldl intersect ['a' .. 'z']) . parseInput

parseInput :: String -> [[String]]
parseInput = map lines . splitOn "\n\n"
