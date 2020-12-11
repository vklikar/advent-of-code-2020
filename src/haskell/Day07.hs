{-# LANGUAGE FlexibleContexts #-}

module Day07 where

import qualified Data.Map as M
import Data.Maybe
import Lib
import Text.Regex.PCRE

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
parseRule x = (bag, content)
  where
    bagRegex = "^\\w+ \\w+"
    contentRegex = "\\d+ \\w+ \\w+"
    bag = x =~ bagRegex :: String
    content = M.fromList $ map (parseContent . words) (getAllTextMatches (x =~ contentRegex) :: [String])

parseContent :: [String] -> (String, Int)
parseContent ("no" : _) = ("", 0)
parseContent (n : c1 : c2 : _) = (c1 ++ " " ++ c2, read n :: Int)

containsColor :: String -> String -> M.Map String (M.Map String a) -> Bool
containsColor color containedColor rules
  | color == "" = False
  | color == containedColor = True
  | otherwise = or [containsColor color containedColor rules | color <- M.keys content]
  where
    content = fromJust $ M.lookup color rules

countBags :: String -> M.Map String (M.Map String Int) -> Int
countBags "" _ = 0
countBags color rules =
  sum [n + n * countBags color rules | (color, n) <- M.toList content]
  where
    content = fromJust $ M.lookup color rules
