module Day11 where

import Lib

solvePart1 :: String -> Int
solvePart1 input = countOccupiedSeats 4 1 layout
  where
    layout = parseInput input

solvePart2 :: String -> Int
solvePart2 input = countOccupiedSeats 5 (max (length layout) (length $ head layout) - 1) layout
  where
    layout = parseInput input

parseInput :: String -> [String]
parseInput = lines

countOccupiedSeats :: Int -> Int -> [String] -> Int
countOccupiedSeats becomeEmptyMin lookDistance layout =
  sum $ map (count '#') $ simulate [] becomeEmptyMin lookDistance layout

simulate :: [String] -> Int -> Int -> [String] -> [String]
simulate prev becomeEmptyMin lookDistance layout
  | prev == layout = layout
  | otherwise = simulate layout becomeEmptyMin lookDistance layout'
  where
    layout' = applyRules (0, 0) becomeEmptyMin lookDistance layout w h layout
    w = length $ head layout
    h = length layout

applyRules :: (Int, Int) -> Int -> Int -> [String] -> Int -> Int -> [String] -> [String]
applyRules (i, j) becomeEmptyMin lookDistance newLayout w h layout
  | i == h = newLayout
  | seat == 'L' && occupiedSeats == 0 = f (setSeat '#')
  | seat == '#' && occupiedSeats >= becomeEmptyMin = f (setSeat 'L')
  | otherwise = f newLayout
  where
    seat = getSeat (i, j) w h layout
    j' = if j < (w - 1) then j + 1 else 0
    i' = if j' == 0 then i + 1 else i
    occupiedSeats =
      count
        '#'
        [ firstSeatInDirection (i, j) (di, dj) lookDistance 0 w h layout
          | di <- [-1 .. 1],
            dj <- [-1 .. 1],
            (di, dj) /= (0, 0)
        ]
    setSeat x = setMatrixElem (i, j) x newLayout
    f x = applyRules (i', j') becomeEmptyMin lookDistance x w h layout

firstSeatInDirection :: (Int, Int) -> (Int, Int) -> Int -> Int -> Int -> Int -> [String] -> Char
firstSeatInDirection (i, j) (di, dj) lookDistance step w h layout
  | step == lookDistance = '.'
  | seat == '#' || seat == 'L' = seat
  | otherwise = firstSeatInDirection (i + di, j + dj) (di, dj) lookDistance (step + 1) w h layout
  where
    seat = getSeat (i + di, j + dj) w h layout

getSeat :: (Int, Int) -> Int -> Int -> [String] -> Char
getSeat = getMatrixElem '.'
