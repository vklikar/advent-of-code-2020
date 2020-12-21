module Day21 where

import Data.Attoparsec.Text as A
import Data.Bifunctor
import Data.Either
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T

solvePart1 :: String -> Int
solvePart1 input = length . filter (`notElem` ingredientsWithAllergens) . concatMap fst $ foods
  where
    foods = parseInput input
    ingredientsWithAllergens = concat (M.elems $ allergensIngredients foods)

solvePart2 :: String -> String
solvePart2 input = intercalate "," . map snd . sort . f [] $ sortOn (length . snd) . M.toList . allergensIngredients $ foods
  where
    foods = parseInput input
    f _ [] = []
    f acc (x@(allergen, ingredients) : xs) =
      (allergen, head ingredients) :
      f acc' (sortOn (length . snd) . map (second (filter (`notElem` acc'))) $ xs)
      where
        acc' = ingredients ++ acc

parseInput :: String -> [([String], [String])]
parseInput = fromRight [] . parseOnly parseFoods . T.pack

parseFoods :: Parser [([String], [String])]
parseFoods = do many1 parseFood

parseFood :: Parser ([String], [String])
parseFood = do
  ingredients <- many1 letter `sepBy1` char ' '
  string " (contains "
  allergens <- many1 letter `sepBy1` string ", "
  char ')'
  endOfLine
  return (ingredients, allergens)

allergensIngredients :: [([String], [String])] -> M.Map String [String]
allergensIngredients = M.fromListWith intersect . foldr (\x -> (++) ([(a, fst x) | a <- snd x])) []
