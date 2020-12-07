module Day5 where

import Data.List

solvePart1 :: String -> Int
solvePart1 = maximum . parseInput

solvePart2 :: String -> Int
solvePart2 = head . detachedSeats . sort . (\\) [0 .. 1023] . parseInput

parseInput :: String -> [Int]
parseInput = map (foldl (\x y -> 2 * x + y) 0 . map bin) . lines

bin :: Char -> Int
bin 'F' = 0
bin 'B' = 1
bin 'L' = 0
bin 'R' = 1

detachedSeats :: [Int] -> [Int]
detachedSeats xs = ys
  where
    (_, ys, _) = unzip3 $ filter isSeatDetached $ zip3 xs (drop 1 xs) (drop 2 xs)

isSeatDetached :: (Int, Int, Int) -> Bool
isSeatDetached (l, seat, r) = l + 1 /= seat && seat /= r - 1
