module Day10 where

import Data.List
import qualified Data.Map as M
import Lib

solvePart1 :: String -> Int
solvePart1 = getJoltDiffProduct . parseInput

solvePart2 :: String -> Int
solvePart2 = getConnectionsCount . parseInput

parseInput :: String -> [Int]
parseInput = map read . lines

processNumbers :: [Int] -> [Int]
processNumbers numbers = 0 : sortedNumbers ++ [last sortedNumbers + 3]
  where
    sortedNumbers = sort numbers

getJoltDiffProduct :: [Int] -> Int
getJoltDiffProduct numbers = count 1 diffs * count 3 diffs
  where
    diffs = zipWith (-) (tail numbers') numbers'
    numbers' = processNumbers numbers

getConnectionsCount :: [Int] -> Int
getConnectionsCount numbers = getConnectionsCount' revNumbers memory
  where
    memory = M.fromList $ zip revNumbers (repeat 0)
    revNumbers = reverse numbers'
    numbers' = processNumbers numbers

getConnectionsCount' :: [Int] -> M.Map Int Int -> Int
getConnectionsCount' [] m = m M.! 0
getConnectionsCount' (x : xs) m = getConnectionsCount' xs (M.adjust (const y) x m)
  where
    y' = sum [M.findWithDefault 0 (x + i) m | i <- [1 .. 3]]
    y = if y' == 0 then 1 else y'
