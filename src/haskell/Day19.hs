{-# LANGUAGE FlexibleContexts #-}

module Day19 where

import Data.Attoparsec.Text as A
import Data.Either
import Data.List hiding (group)
import qualified Data.Map as M
import qualified Data.Text as T
import Lib
import Text.Regex.PCRE

type RuleNode = Either Int Char

solvePart1 :: String -> Int
solvePart1 = countMatchingMessages 0 . parseInput

solvePart2 :: String -> Int
solvePart2 = countMatchingMessages 0 . parseInput . replace8 . replace11
  where
    replace8 = replace "8: 42" "8: 42 | 42 8"
    replace11 = replace "11: 42 31" "11: 42 31 | 42 11 31"

parseInput :: String -> (M.Map Int [[RuleNode]], [String])
parseInput = fromRight (M.empty, []) . parseOnly parseInput' . T.pack

parseInput' :: Parser (M.Map Int [[RuleNode]], [String])
parseInput' = do
  rules <- many1 parseRule
  endOfLine
  messages <- many1 parseMessage
  return (M.fromList rules, messages)

parseRule :: Parser (Int, [[RuleNode]])
parseRule = do
  key <- decimal
  string ": "
  subrules <- choice [parseSubRules, parseLetter]
  endOfLine
  return (key, subrules)

parseSubRules :: Parser [[RuleNode]]
parseSubRules = do parseNumbers `sepBy1` " | "

parseNumbers :: Parser [RuleNode]
parseNumbers = do
  numbers <- decimal `sepBy1` char ' '
  let nodes = map Left numbers
  return nodes

parseLetter :: Parser [[RuleNode]]
parseLetter = do
  char '"'
  l <- letter
  char '"'
  let node = Right l
  return [[node]]

parseMessage :: Parser String
parseMessage = do
  message <- many1 letter
  endOfLine
  return message

countMatchingMessages :: Int -> (M.Map Int [[RuleNode]], [String]) -> Int
countMatchingMessages n (rules, messages) = Lib.count True $ map (=~ regex) messages
  where
    regex = '^' : rulesToRegex 0 rules ++ "$"

rulesToRegex :: Int -> M.Map Int [[RuleNode]] -> String
rulesToRegex k rules = v'
  where
    v = rules M.! k
    v' = group $ intercalate "|" [group $ foo [substituteNode k rules node | node <- subrule] | subrule <- v]
    foo xs = xs'
      where
        recursive = any snd xs
        nonRecursiveNodes = filter (/= "") $ map fst xs
        maxLoops = 4 -- PCRE couldn't handle more, but it was enough for the solution
        -- (TDFA worked with a higher n, but it was too slow and consumed too much memory)
        xs'
          | recursive && length nonRecursiveNodes > 1 =
            group $ intercalate "|" [concatMap (group . (++ "{" ++ show n ++ "}")) nonRecursiveNodes | n <- [2 .. maxLoops]]
          | recursive = concatMap (group . (++ "+")) nonRecursiveNodes
          | otherwise = concat nonRecursiveNodes

group :: String -> String
group x
  | all (`elem` "ab") x = x
  | otherwise = "(?:" ++ x ++ ")"

substituteNode :: Int -> M.Map Int [[RuleNode]] -> RuleNode -> (String, Bool)
substituteNode _ _ (Right x) = ([x], False)
substituteNode k rules (Left x)
  | k == x = ("", True)
  | otherwise = (rulesToRegex x rules, False)
