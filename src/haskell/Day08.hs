module Day08 where

import Data.Bifunctor
import Data.Either
import Lib

solvePart1 :: String -> Int
solvePart1 = fromLeft 0 . executeProgram . parseInput

solvePart2 :: String -> Int
solvePart2 input =
  (head . rights) [executeProgram x | x <- makeAllPossiblePrograms instructions]
  where
    instructions = parseInput input

parseInput :: String -> [(String, Int)]
parseInput = map (second readArgument . listToPair . words) . lines

readArgument :: String -> Int
readArgument x = (read . dropWhile (== '+')) x :: Int

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

executeProgram :: [(String, Int)] -> Either Int Int
executeProgram input = executeProgram' 0 [] 0 (length input) input

executeProgram' :: Int -> [Int] -> Int -> Int -> [(String, Int)] -> Either Int Int
executeProgram' index visited n lenXs xs
  | index == lenXs = Right n -- program terminated normally
  | nextIndex `elem` visited = Left n -- break before going into an infinite loop
  | otherwise = executeProgram' nextIndex newVisited (n + increment) lenXs xs
  where
    (op, value) = xs !! index
    (increment, nextIndex) = executeInstruction op value index
    newVisited = index : visited

executeInstruction :: String -> Int -> Int -> (Int, Int)
executeInstruction "acc" value i = (value, i + 1)
executeInstruction "nop" _ i = (0, i + 1)
executeInstruction "jmp" value i = (0, i + value)
