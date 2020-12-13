module Day13 where

import Data.Bifunctor
import Data.List
import Data.List.Split
import qualified Data.Text as T
import Lib

solvePart1 :: String -> Int
solvePart1 = solvePart1' . parseInput

solvePart2 :: String -> Int
solvePart2 = solvePart2' 100000000000000 . snd . parseInput

parseInput :: String -> (Int, [Int])
parseInput = bimap read (map (read . T.unpack . T.replace "x" "0" . T.pack) . splitOn ",") . listToPair . lines

solvePart1' :: (Int, [Int]) -> Int
solvePart1' (earliestDeparture, buses) = a * (b - earliestDeparture)
  where
    (a, b) = head $ sortOn snd $ takeableBuses buses
    takeableBuses [] = []
    takeableBuses (0 : xs) = takeableBuses xs
    takeableBuses (x : xs) = (x, t) : takeableBuses xs
      where
        (q, r) = earliestDeparture `quotRem` x
        t = if r == 0 then q * x else q * x + x

solvePart2' :: Int -> [Int] -> Int
-- minExpectedTimestamp can always be set to 0, but it will take longer.
-- Even with minExpectedTimestamp, it's quite slow. It took about 47 seconds to calculate the
-- result. I believe it can be transformed into a mathematical expression to get the result
-- straight away, but I'm happy with this solution for now.
solvePart2' minExpectedTimestamp buses = f start enumeratedBuses'
  where
    enumeratedBuses = filter (\(a, _) -> a > 0) $ zip buses [0 ..]
    maxBus = last $ sortOn fst enumeratedBuses
    start = (minExpectedTimestamp - cycleStart * fst maxBus) `div` (cycleLength * fst maxBus)
    enumeratedBuses' = filter (\(_, b) -> b /= snd maxBus) enumeratedBuses
    busCycles = map (modCycle maxBus) enumeratedBuses'
    (cycleStart, cycleLength) = last $ sortOn snd busCycles
    f n xs
      | all busAtRequiredPosition' xs = timestamp cycleStart (cycleLength * n) maxBus
      | otherwise = f (n + 1) xs
      where
        busAtRequiredPosition' = busAtRequiredPosition cycleStart (cycleLength * n) maxBus

modCycle :: (Int, Int) -> (Int, Int) -> (Int, Int)
modCycle referenceBus bus = (fstModEq0, sndModEq0 - fstModEq0)
  where
    fstModEq0 = f 1
    sndModEq0 = f (fstModEq0 + 1)
    f n
      | busAtRequiredPosition 0 n referenceBus bus = n
      | otherwise = f (n + 1)

busAtRequiredPosition :: Int -> Int -> (Int, Int) -> (Int, Int) -> Bool
busAtRequiredPosition cycleStart n (a, b) (a', b') = (timestamp cycleStart n (a, b) + b') `mod` a' == 0

timestamp :: Int -> Int -> (Int, Int) -> Int
timestamp cycleStart n (a, b) = a * cycleStart + a * n - b
