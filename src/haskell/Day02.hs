module Day02 where

import Data.Attoparsec.Text
import Data.List (elemIndices)
import Data.Either
import qualified Data.Text as T
import Lib

data PasswordPolicy = PasswordPolicy
  { _number1 :: Int,
    _number2 :: Int,
    _letter :: Char,
    _password :: String
  }
  deriving (Show)

solvePart1 :: String -> Int
solvePart1 input = length $ filter (== True) evaluatedPasswords
  where
    evaluatedPasswords =
      [ passwordsValidByRepetition atLeast atMost letter password
        | (PasswordPolicy atLeast atMost letter password) <- parseInput input
      ]

solvePart2 :: String -> Int
solvePart2 input = length $ filter (== True) evaluatedPasswords
  where
    evaluatedPasswords =
      [ passwordsValidByPosition p1 p2 letter password
        | (PasswordPolicy p1 p2 letter password) <- parseInput input
      ]

parseInput :: String -> [PasswordPolicy]
parseInput = rights . map (parseOnly parsePasswordPolicy . T.pack) . lines

parsePasswordPolicy :: Parser PasswordPolicy
parsePasswordPolicy = do
  n1 <- decimal
  char '-'
  n2 <- decimal
  char ' '
  l <- letter
  string ": "
  password <- many1 letter
  return $ PasswordPolicy n1 n2 l password

passwordsValidByRepetition :: Int -> Int -> Char -> String -> Bool
passwordsValidByRepetition atLeast atMost letter password =
  atLeast <= numberOfOccurrences && numberOfOccurrences <= atMost
  where
    numberOfOccurrences = Lib.count letter password

passwordsValidByPosition :: Int -> Int -> Char -> String -> Bool
passwordsValidByPosition p1 p2 letter password =
  isAtPosition p1 letter password /= isAtPosition p2 letter password

isAtPosition :: Int -> Char -> String -> Bool
isAtPosition p char string = any ((== p) . (+ 1)) (elemIndices char string)
