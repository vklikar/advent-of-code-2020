module Day5 where

import Data.List

solvePart1 :: String -> Int
solvePart1 = maximum . map getSeatId . parseInput

solvePart2 :: String -> Int
solvePart2 = head . deatchedSeats . (\\) [0 .. 1023] . map getSeatId . parseInput

parseInput :: String -> [String]
parseInput = lines

deatchedSeats :: [Int] -> [Int]
deatchedSeats xs = ys
  where
    (_, ys, _) = unzip3 $ filter isSeatDetached $ zip3 xs (drop 1 xs) (drop 2 xs)

isSeatDetached :: (Int, Int, Int) -> Bool
isSeatDetached (l, seat, r) = l + 1 /= seat && seat /= r - 1

getSeatId :: String -> Int
getSeatId xs = row * 8 + column
  where
    (rowXs, columnXs) = splitAt 7 $ map bin xs
    row = getPosition 0 127 rowXs
    column = getPosition 0 7 columnXs

bin :: Char -> Int
bin 'F' = 0
bin 'B' = 1
bin 'L' = 0
bin 'R' = 1

getPosition :: Int -> Int -> [Int] -> Int
getPosition lo _ [] = lo
getPosition lo hi (x : xs) = getPosition newLo newHi xs
  where
    (newLo, newHi) = getHalf lo hi x

getHalf :: Int -> Int -> Int -> (Int, Int)
getHalf lo hi half = [(lo, center), (center + 1, hi)] !! half
  where
    center = lo + (hi - lo) `div` 2
