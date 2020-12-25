module Day24 where

import Data.Attoparsec.Text as A hiding (take)
import Data.Either
import Data.List
import qualified Data.Text as T

solvePart1 :: String -> Int
solvePart1 = length . filter (odd . length) . group . sort . tiles . parseInput

solvePart2 :: String -> Int
solvePart2 input = length . simulate 100 $ blackTiles
  where
    blackTiles = nub . concat . filter (odd . length) . group . sort . tiles . parseInput $ input

parseInput :: String -> [[String]]
parseInput = map (map T.unpack) . fromRight [] . parseOnly parseTiles . T.pack

parseTiles :: Parser [[T.Text]]
parseTiles = do many1 (choice [string "e", string "se", string "sw", string "w", string "nw", string "ne"]) `sepBy1` endOfLine

tiles :: [[String]] -> [(Double, Double)]
tiles = map (foldl' addDirs (0, 0) . map parseDir)

parseDir :: String -> (Double, Double)
parseDir "e" = (1.0, 0.0)
parseDir "se" = (0.5, -0.5)
parseDir "sw" = (-0.5, -0.5)
parseDir "w" = (-1.0, 0.0)
parseDir "nw" = (-0.5, 0.5)
parseDir "ne" = (0.5, 0.5)

addDirs :: (Double, Double) -> (Double, Double) -> (Double, Double)
addDirs (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

simulate :: Int -> [(Double, Double)] -> [(Double, Double)]
simulate 0 xs = xs
simulate n xs = simulate (n - 1) $ nub $ f xs xs
  where
    f [] _ = []
    f (a : as) bs
      | blackNeighborsCount == 0 || blackNeighborsCount > 2 = whiteToBlackTiles ++ f as bs
      | otherwise = a : whiteToBlackTiles ++ f as bs
      where
        blackNeighbors = getNeighbors True bs a
        blackNeighborsCount = length blackNeighbors
        whiteNeighbors = getNeighbors False bs a
        whiteToBlackTiles = filter ((== 2) . length . getNeighbors True bs) whiteNeighbors

getNeighbors :: Bool -> [(Double, Double)] -> (Double, Double) -> [(Double, Double)]
getNeighbors black xs x = filter filterFunc neighbors
  where
    offsets = map parseDir ["e", "se", "sw", "w", "nw", "ne"]
    neighbors = map (addDirs x) offsets
    filterFunc = if black then (`elem` xs) else (`notElem` xs)
