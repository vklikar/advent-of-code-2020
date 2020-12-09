module Day9 where

solvePart1 :: String -> Int
solvePart1 = findNumberWithoutSum 25 . parseInput

solvePart2 :: String -> Int
solvePart2 input = findWeakness invalidNumber numbers
  where
    numbers = parseInput input
    invalidNumber = findNumberWithoutSum 25 numbers

parseInput :: String -> [Int]
parseInput = map read . words

findNumberWithoutSum :: Int -> [Int] -> Int
findNumberWithoutSum preambleSize xs = f (take preambleSize xs) (drop preambleSize xs)
  where
    f preamble (x : xs)
      | isNumberValid x preamble preamble = f (drop 1 preamble ++ [x]) xs
      | otherwise = x

isNumberValid :: Int -> [Int] -> [Int] -> Bool
isNumberValid _ [] _ = False
isNumberValid n (_ : xs) [] = isNumberValid n xs (tail xs)
isNumberValid n (x : xs) (y : ys)
  | x + y == n = True
  | otherwise = isNumberValid n (x : xs) ys

findWeakness :: Int -> [Int] -> Int
findWeakness invalidNumber xs = minimum contiguousNumbers + maximum contiguousNumbers
  where
    contiguousNumbers = f xs xs [] invalidNumber
    f (x : xs) (y : ys) zs n
      | total == n = zs
      | total < n = f xs (y : ys) (x : zs) n
      | otherwise = f ys ys [] n
      where
        total = sum zs
