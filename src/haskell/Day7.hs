module Day7 where

import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Lib

solvePart1 :: String -> Int
solvePart1 input =
  count True [containsColor color "shiny gold" rules | color <- M.keys rules, color /= "shiny gold"]
  where
    rules = parseInput input

solvePart2 :: String -> Int
solvePart2 input = countBags "shiny gold" rules
  where
    rules = parseInput input

parseInput :: String -> M.Map String (M.Map String Int)
parseInput = M.fromList . map parseRule . lines

parseRule :: String -> (String, M.Map String Int)
parseRule rule = (color, content)
  where
    (color, content') = listToPair $ splitOn " bags contain " rule
    content'' = map words $ splitOn ", " (head $ splitOn "." content')
    content = M.fromList [(parseColor (c1 ++ " " ++ c2), parseNumber n) | (n : c1 : c2 : _) <- content'']

parseNumber :: String -> Int
parseNumber "no" = 0
parseNumber n = read n :: Int

parseColor :: String -> String
parseColor "other bags" = ""
parseColor color = color

-- could be improved so as not to look for the same bags repeatedly
containsColor :: String -> String -> M.Map String (M.Map String a) -> Bool
containsColor color containedColor rules
  | color == "" = False
  | color == containedColor = True
  | otherwise = or [containsColor color containedColor rules | color <- M.keys content]
  where
    content = fromJust $ M.lookup color rules

-- could be improved so as not to count the same bags repeatedly
countBags :: String -> M.Map String (M.Map String Int) -> Int
countBags "" _ = 0
countBags color rules =
  sum [n + n * countBags color rules | (color, n) <- M.toList content]
  where
    content = fromJust $ M.lookup color rules
