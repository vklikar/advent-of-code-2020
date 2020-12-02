module Day2 where

import Data.List (elemIndices)
import Data.List.Split

solvePart1 :: String -> Int
solvePart1 input = length $ filter (== True) evaluatedPasswords
  where
    evaluatedPasswords =
      [ passwordsValidByRepetition atLeast atMost letter password
        | (atLeast, atMost, letter, password) <- parseInput input
      ]

solvePart2 :: String -> Int
solvePart2 input = length $ filter (== True) evaluatedPasswords
  where
    evaluatedPasswords =
      [ passwordsValidByPosition p1 p2 letter password
        | (p1, p2, letter, password) <- parseInput input
      ]

parseInput :: String -> [(Int, Int, Char, String)]
parseInput = map (parseInputLine . words) . lines

parseInputLine :: [String] -> (Int, Int, Char, String)
parseInputLine [w1, w2, password] = (number1, number2, letter, password)
  where
    letter = head w2
    [number1, number2] = map read (splitOn "-" w1)

passwordsValidByRepetition :: Int -> Int -> Char -> String -> Bool
passwordsValidByRepetition atLeast atMost letter password =
  atLeast <= numberOfOccurrences && numberOfOccurrences <= atMost
  where
    numberOfOccurrences = count letter password

passwordsValidByPosition :: Int -> Int -> Char -> String -> Bool
passwordsValidByPosition p1 p2 letter password =
  isAtPosition p1 letter password /= isAtPosition p2 letter password

count :: Char -> String -> Int
count x = length . filter (== x)

isAtPosition :: Int -> Char -> String -> Bool
isAtPosition p char string = any ((== p) . (+ 1)) (elemIndices char string)
