module Day22 where

import Data.Attoparsec.Text as A hiding (take)
import Data.Either
import qualified Data.Text as T

solvePart1 :: String -> Int
solvePart1 input = countScore . uncurry playCombat $ decks
  where
    decks = parseInput input

solvePart2 :: String -> Int
solvePart2 input = countScore . uncurry (playRecursiveCombat []) $ decks
  where
    decks = parseInput input

parseInput :: String -> ([Int], [Int])
parseInput = fromRight ([], []) . parseOnly parseDecks . T.pack

parseDecks :: Parser ([Int], [Int])
parseDecks = do
  string "Player 1:"
  endOfLine
  player1 <- decimal `sepBy1` endOfLine
  endOfLine
  endOfLine
  string "Player 2:"
  endOfLine
  player2 <- decimal `sepBy1` endOfLine
  return (player1, player2)

countScore :: ([Int], [Int]) -> Int
countScore = sum . zipWith (*) [1 ..] . reverse . uncurry (++)

playCombat :: [Int] -> [Int] -> ([Int], [Int])
playCombat xs [] = (xs, [])
playCombat [] ys = ([], ys)
playCombat (x : xs) (y : ys)
  | x > y = playCombat (xs ++ [x, y]) ys
  | otherwise = playCombat xs (ys ++ [y, x])

playRecursiveCombat :: [([Int], [Int])] -> [Int] -> [Int] -> ([Int], [Int])
playRecursiveCombat _ xs [] = (xs, [])
playRecursiveCombat _ [] ys = ([], ys)
playRecursiveCombat history deck1@(x : xs) deck2@(y : ys)
  | (deck1, deck2) `elem` history = (xs, [])
  | length xs >= x && length ys >= y =
    if null . snd $ playRecursiveCombat [] (take x xs) (take y ys)
      then playAfterPlayer1Won
      else playAfterPlayer2Won
  | x > y = playAfterPlayer1Won
  | otherwise = playAfterPlayer2Won
  where
    playAfterPlayer1Won = playRecursiveCombat ((deck1, deck2) : history) (xs ++ [x, y]) ys
    playAfterPlayer2Won = playRecursiveCombat ((deck1, deck2) : history) xs (ys ++ [y, x])
