module Day11 where

import qualified Data.Matrix as Matrix
import Data.Maybe
import Lib

solvePart1 :: String -> Int
solvePart1 input = countOccupiedSeats 4 (Just 1) layout
  where
    layout = parseInput input

solvePart2 :: String -> Int
solvePart2 input = countOccupiedSeats 5 Nothing layout
  where
    layout = parseInput input

parseInput :: String -> Matrix.Matrix Char
parseInput = Matrix.fromLists . lines

countOccupiedSeats :: Int -> Maybe Int -> Matrix.Matrix Char -> Int
countOccupiedSeats becomeEmptyMin lookDistance layout =
  count '#' $ Matrix.toList $ simulate (Matrix.fromLists [""]) becomeEmptyMin lookDistance layout

simulate :: Matrix.Matrix Char -> Int -> Maybe Int -> Matrix.Matrix Char -> Matrix.Matrix Char
simulate prev becomeEmptyMin lookDistance layout
  | prev == layout = layout
  | otherwise = simulate layout becomeEmptyMin lookDistance layout'
  where
    layout' = applyRules (1, 1) becomeEmptyMin lookDistance layout layout

applyRules :: (Int, Int) -> Int -> Maybe Int -> Matrix.Matrix Char -> Matrix.Matrix Char -> Matrix.Matrix Char
applyRules (i, j) becomeEmptyMin lookDistance newLayout layout
  | i > Matrix.nrows layout = newLayout
  | seat == 'L' && occupiedSeats == 0 = f (setSeat '#')
  | seat == '#' && occupiedSeats >= becomeEmptyMin = f (setSeat 'L')
  | otherwise = f newLayout
  where
    seat = fromMaybe '.' $ getSeat (i, j) layout
    j' = if j < Matrix.ncols layout then j + 1 else 0
    i' = if j' == 0 then i + 1 else i
    occupiedSeats =
      count
        '#'
        [ firstSeatInDirection (i, j) (di, dj) lookDistance 0 layout
          | di <- [-1 .. 1],
            dj <- [-1 .. 1],
            (di, dj) /= (0, 0)
        ]
    setSeat x = Matrix.setElem x (i, j) newLayout
    f x = applyRules (i', j') becomeEmptyMin lookDistance x layout

firstSeatInDirection :: (Int, Int) -> (Int, Int) -> Maybe Int -> Int -> Matrix.Matrix Char -> Char
firstSeatInDirection (i, j) (di, dj) lookDistance step layout
  | isNothing seat || Just step == lookDistance = '.'
  | seat == Just '#' || seat == Just 'L' = fromJust seat
  | otherwise = firstSeatInDirection (i + di, j + dj) (di, dj) lookDistance (step + 1) layout
  where
    seat = getSeat (i + di, j + dj) layout

getSeat :: (Int, Int) -> Matrix.Matrix Char -> Maybe Char
getSeat (i, j) = Matrix.safeGet i j
