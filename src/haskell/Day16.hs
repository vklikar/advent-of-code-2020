module Day16 where

import Data.Attoparsec.Text as A
import Data.Bifunctor
import Data.Either
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Lib

type Rule = (String, [(Int, Int)])

solvePart1 :: String -> Int
solvePart1 = solvePart1' . parseInput

solvePart2 :: String -> Int
solvePart2 = solvePart2' (\k _ -> "departure" `isPrefixOf` k) . parseInput

parseInput :: String -> ([Rule], [Int], [[Int]])
parseInput = fromRight ([], [], []) . parseOnly parseNotes . T.pack

parseNotes :: Parser ([Rule], [Int], [[Int]])
parseNotes = do
  rules <- many1 parseRule
  endOfLine
  manyTill anyChar endOfLine
  myTicket <- parseTicket
  endOfLine
  manyTill anyChar endOfLine
  nearbyTickets <- many1 parseTicket
  return (rules, myTicket, nearbyTickets)

parseRule :: Parser Rule
parseRule = do
  field <- manyTill anyChar (string ": ")
  a <- decimal
  char '-'
  b <- decimal
  string " or "
  c <- decimal
  char '-'
  d <- decimal
  endOfLine
  return (field, [(a, b), (c, d)])

parseTicket :: Parser [Int]
parseTicket = do
  numbers <- decimal `sepBy1` char ','
  endOfLine
  return numbers

solvePart1' :: ([Rule], [Int], [[Int]]) -> Int
solvePart1' (rules, _, nearbyTickets) = sum $ map (errorRate rules) nearbyTickets

solvePart2' :: (String -> Int -> Bool) -> ([Rule], [Int], [[Int]]) -> Int
solvePart2' filter' (rules, myTicket, nearbyTickets) = (product . map snd . M.toList . M.filterWithKey filter') myTicket'
  where
    validTickets = filter ((== 0) . errorRate rules) nearbyTickets
    fields = map fst rules
    fieldPositions = mapPositionsToFields rules fields validTickets
    myTicket' = M.fromList $ map (second (myTicket !!)) (f S.empty (S.fromList [0 .. length fields - 1]) fieldPositions)
    f _ _ [] = []
    f s numbers ((k, v) : xs) = (k, v') : f (s `S.union` v) v xs
      where
        v' = S.elemAt 0 $ numbers S.\\ v

mapPositionsToFields :: [Rule] -> [String] -> [[Int]] -> [(String, S.Set Int)]
mapPositionsToFields rules fields tickets = sortBy (\(_, a) (_, b) -> compare (length b) (length a)) (M.toList fieldPositions')
  where
    fieldPositions = concat [concat [[(field, i) | field <- getWrongFields rules n] | (n, i) <- zip t [0 ..]] | t <- tickets]
    fieldPositions' = foldl' (\m (k, v) -> addToSetInMap k m v) (M.fromList (zip fields (repeat S.empty))) fieldPositions

errorRate :: [Rule] -> [Int] -> Int
errorRate rules ticket = sum [n | n <- ticket, not $ inRanges ranges n]
  where
    ranges = foldl' (++) [] (map snd rules)

inRanges :: [(Int, Int)] -> Int -> Bool
inRanges ranges n = or [n >= a && n <= b | (a, b) <- ranges]

getWrongFields :: [Rule] -> Int -> [String]
getWrongFields rules n = [field | (field, ranges) <- rules, not $ inRanges ranges n]
