module Day8 where

import Data.Bifunctor
import Lib

solvePart1 :: String -> Int
solvePart1 = acc . executeProgram . parseInput

solvePart2 :: String -> Int
solvePart2 input =
  acc $ head $ filter (all snd) [executeProgram x | x <- makeAllPossiblePrograms instructions]
  where
    instructions = parseInput input

parseInput :: String -> [(String, Int)]
parseInput = map (second readArgument . listToPair . words) . lines

readArgument :: String -> Int
readArgument x = (read . dropWhile (== '+')) x :: Int

acc :: [(Int, Bool)] -> Int
acc = sum . map fst

makeAllPossiblePrograms :: [(String, Int)] -> [[(String, Int)]]
makeAllPossiblePrograms input = makeAllPossiblePrograms' 0 input input

makeAllPossiblePrograms' :: Int -> [(String, Int)] -> [(String, Int)] -> [[(String, Int)]]
makeAllPossiblePrograms' _ [] _ = []
makeAllPossiblePrograms' i (x : xs) ys
  | fst x == "acc" = ys : followingPrograms
  | otherwise = swappedNopJmp : followingPrograms
  where
    head' = take i ys
    tail' = drop (i + 1) ys
    swappedNopJmp = head' ++ [first swapNopJmp x] ++ tail'
    followingPrograms = makeAllPossiblePrograms' (i + 1) xs ys

swapNopJmp :: String -> String
swapNopJmp "nop" = "jmp"
swapNopJmp "jmp" = "nop"

executeProgram :: [(String, Int)] -> [(Int, Bool)]
executeProgram input = executeProgram' 0 [] (length input) input

executeProgram' :: Int -> [Int] -> Int -> [(String, Int)] -> [(Int, Bool)]
executeProgram' index visited lenXs xs
  | index == lenXs = [] -- program terminated normally
  | nextIndex `elem` visited = [(increment, False)] -- break before going into an infinite loop
  | otherwise = (increment, True) : executeProgram' nextIndex newVisited lenXs xs
  where
    (op, value) = xs !! index
    (increment, nextIndex) = executeInstruction op value index
    newVisited = index : visited

executeInstruction :: String -> Int -> Int -> (Int, Int)
executeInstruction "acc" value i = (value, i + 1)
executeInstruction "nop" _ i = (0, i + 1)
executeInstruction "jmp" value i = (0, i + value)
