module Day23 where

import Data.List
import Data.Maybe
import Data.Tuple
import Lib

solvePart1 :: Int -> Int
solvePart1 = listToInt . takeWhile (/= 1) . drop 1 . dropWhile (/= 1) . cycle . play 100 . parseInput

-- too slow
solvePart2 :: Int -> Int
solvePart2 input = product . take 2 . drop 1 . dropWhile (/= 1) . cycle $ play 10000000 cups'
  where
    cups = parseInput input
    cups' = cups ++ [maximum cups + 1 .. 1000000]

parseInput :: Int -> [Int]
parseInput = intToList

play :: Int -> [Int] -> [Int]
play 0 cups = cups
play n (currentCup : xs) = play (n - 1) xs'
  where
    (pickUp, otherCups) = splitAt 3 xs
    destinationCup = head . uncurry (++) . swap . span (> currentCup - 1) . reverse . sort $ otherCups
    destinationCupIndex = fromJust $ elemIndex destinationCup otherCups
    (a, b) = splitAt (destinationCupIndex + 1) otherCups
    xs' = a ++ pickUp ++ b ++ [currentCup]
