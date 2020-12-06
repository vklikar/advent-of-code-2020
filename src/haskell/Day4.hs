module Day4 where

import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Lib

solvePart1 :: String -> Int
solvePart1 = count True . map validateReqFields . parseInput

solvePart2 :: String -> Int
solvePart2 = count True . map validateReqFieldsAndRules . parseInput

parseInput :: String -> [Map.Map String String]
parseInput = map (Map.fromList . map pairs . words) . splitOn "\n\n"

requiredFields :: [String]
requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

validateReqFields :: Map.Map String a -> Bool
validateReqFields xs = and [Map.member r xs | r <- requiredFields]

validateReqFieldsAndRules :: Map.Map String String -> Bool
validateReqFieldsAndRules xs =
  and [checkField k v | (k, v) <- Map.assocs xs] && validateReqFields xs

checkField :: String -> String -> Bool
checkField "byr" value = isNumberBetween 1920 2002 value
checkField "iyr" value = isNumberBetween 2010 2020 value
checkField "eyr" value = isNumberBetween 2020 2030 value
checkField "hgt" value
  | unit == "cm" = isNumberBetween 150 193 v
  | unit == "in" = isNumberBetween 59 76 v
  | otherwise = False
  where
    (v, unit) = span isDigit value
checkField "hcl" value =
  length value == 7
    && isPrefixOf "#" value
    && all ((== True) . (`elem` "0123456789abcdef")) (tail value)
checkField "ecl" value = value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
checkField "pid" value = length value == 9 && all isDigit value
checkField "cid" _ = True

isNumberBetween :: Int -> Int -> String -> Bool
isNumberBetween l h value = intValue >= l && intValue <= h
  where
    intValue = read value :: Int

pairs :: String -> (String, String)
pairs = listToPair . splitOn ":"
