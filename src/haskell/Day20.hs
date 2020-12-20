module Day20 where

import Data.Attoparsec.Text as A hiding (take)
import Data.Either
import Data.List
import qualified Data.Map as M
import qualified Data.Matrix as Matrix
import qualified Data.Text as T
import qualified Data.Vector as V
import Lib

solvePart1 :: String -> Int
solvePart1 = solvePart1' . parseInput

solvePart2 :: String -> Int
solvePart2 = solvePart2' . parseInput

parseInput :: String -> M.Map Int (Matrix.Matrix Char)
parseInput = fromRight M.empty . parseOnly parseTiles . T.pack

parseTiles :: Parser (M.Map Int (Matrix.Matrix Char))
parseTiles = do
  tiles <- parseTile `sepBy1` string "\n\n"
  return (M.fromList tiles)

parseTile :: Parser (Int, Matrix.Matrix Char)
parseTile = do
  string "Tile "
  tileId <- decimal
  char ':'
  endOfLine
  matrix <- many1 (choice [char '#', char '.']) `sepBy1` endOfLine
  return (tileId, Matrix.fromLists matrix)

solvePart1' :: M.Map Int (Matrix.Matrix Char) -> Int
solvePart1' tiles =
  product $
    nub $
      concat $
        filter ((== 2) . length) $
          group [aId | (aId, a) <- M.assocs tiles, (bId, b) <- M.assocs tiles, a /= b, tilesFit a b]

solvePart2' :: M.Map Int (Matrix.Matrix Char) -> Int
solvePart2' tiles = Lib.count '#' $ Matrix.toList imageWithSeaMonsters
  where
    links = findLinks tiles
    edgeLinks = filter (\(a, b) -> a `elem` (edgeTiles ++ cornerTiles) && b `elem` (edgeTiles ++ cornerTiles)) links
    linksToRegularTiles = filter (\(a, b) -> b `elem` regularTiles) links
    (cornerTiles, edgeTiles, regularTiles) = categorize tiles
    edgePath = findEdgePath (head cornerTiles) edgeLinks
    matrixEdgeTilesIds = edgeTilesToMatrix cornerTiles edgePath
    matrixAllTilesIds = regularTilesToMatrix linksToRegularTiles matrixEdgeTilesIds
    matrixAllTiles = Matrix.mapPos (\_ x -> tiles M.! x) matrixAllTilesIds
    matrixMatchingTiles = rotateTiles matrixAllTiles
    image = composeImage matrixMatchingTiles
    imageWithSeaMonsters = head $ [image' | t <- transformations, let (cnt, image') = findMonsters (t image), cnt > 0]
    monster =
      [ "                  #",
        "#    ##    ##    ###",
        " #  #  #  #  #  #"
      ]
    (monsterWidth, monsterHeight) = (20, 3)
    monsterRelativePosition = concatMap (\as -> zip (repeat $ fst as) (snd as)) $ zip [0 .. 2] $ map (elemIndices '#') monster
    moveMonster i j = map (\(a, b) -> (a + i, b + j)) monsterRelativePosition
    monsterFound (i, j) m = and [m Matrix.! (x, y) == '#' | (x, y) <- moveMonster i j]
    markMonster m [] = m
    markMonster m ((x, y) : xs) = markMonster (Matrix.setElem 'O' (x, y) m) xs
    findMonsters m = f 0 (1, 1) (Matrix.nrows m - monsterHeight, Matrix.ncols m - monsterWidth) m
      where
        f n (i, j) (iMax, jMax) m
          | i > iMax = (n, m)
          | monsterFound (i, j) m = f (n + 1) (i', j') (iMax, jMax) (markMonster m monsterFoundCoords)
          | otherwise = f n (i', j') (iMax, jMax) m
          where
            monsterFoundCoords = map (\(a, b) -> (a + i, b + j)) monsterRelativePosition
            (i', j') = if j == jMax then (i + 1, 1) else (i, j + 1)

composeImage :: Matrix.Matrix (Matrix.Matrix Char) -> Matrix.Matrix Char
composeImage m = m'
  where
    xs =
      Matrix.toLists
        ( Matrix.mapPos
            ( \_ x ->
                Matrix.toLists $
                  Matrix.submatrix 2 (Matrix.nrows x - 1) 2 (Matrix.ncols x - 1) x
            )
            m
        )
    m' = Matrix.fromLists $ concatMap (foldl1' (zipWith (++))) xs

rotateTiles :: Matrix.Matrix (Matrix.Matrix Char) -> Matrix.Matrix (Matrix.Matrix Char)
rotateTiles m = f (1, 1) (Matrix.nrows m, Matrix.ncols m - 1) m
  where
    f (i, j) (iMax, jMax) m
      | i > iMax = m
      | otherwise = f (i', j') (iMax, jMax) m'
      where
        tile = m Matrix.! (i, j)
        rightNeighbor = m Matrix.! (i, j + 1)
        downNeighbor = m Matrix.! (i + 1, j)
        (tTile, tRight, tDown) =
          head
            [ (tTile, tRight, tDown)
              | tTile <- transformations,
                tRight <- transformations,
                (lastMatrixCol . tTile) tile == (headMatrixCol . tRight) rightNeighbor,
                tDown <- transformations,
                i == iMax || (lastMatrixRow . tTile) tile == (headMatrixRow . tDown) downNeighbor
            ]
        m'
          | i == iMax =
            Matrix.setElem (tRight rightNeighbor) (i, j + 1) $
              Matrix.setElem (tTile tile) (i, j) m
          | otherwise =
            Matrix.setElem (tDown downNeighbor) (i + 1, j) $
              Matrix.setElem (tRight rightNeighbor) (i, j + 1) $
                Matrix.setElem (tTile tile) (i, j) m
        (i', j') = if j == jMax then (i + 1, 1) else (i, j + 1)

transformations :: [Matrix.Matrix a -> Matrix.Matrix a]
transformations = [ro . fl | ro <- rotations, fl <- flips]
  where
    rotations = [id, rotateMatrixLeft, rotateMatrixRight, rotateMatrix180]
    flips = [id, flipMatrixRows, flipMatrixCols, flipMatrixRows . flipMatrixCols]

regularTilesToMatrix :: [(Int, Int)] -> Matrix.Matrix Int -> Matrix.Matrix Int
regularTilesToMatrix linksToRegularTiles m = f (2, 2) (Matrix.nrows m - 1, Matrix.ncols m - 1) linksToRegularTiles m
  where
    f (i, j) (iMax, jMax) linksToRegularTiles m
      | i > iMax = m
      | otherwise = f (i', j') (iMax, jMax) linksToRegularTiles' m'
      where
        leftNeighbor = m Matrix.! (i, j - 1)
        upNeighbor = m Matrix.! (i -1, j)
        leftLinks = map snd $ filter (\(a, b) -> a == leftNeighbor) linksToRegularTiles
        upLinks = map snd $ filter (\(a, b) -> a == upNeighbor) linksToRegularTiles
        intersection = head $ leftLinks `intersect` upLinks
        linksToRegularTiles' = filter (\(a, b) -> a /= upNeighbor && b /= intersection) linksToRegularTiles
        m' = Matrix.setElem intersection (i, j) m
        (i', j') = if j == jMax then (i + 1, 2) else (i, j + 1)

edgeTilesToMatrix :: [Int] -> [Int] -> Matrix.Matrix Int
edgeTilesToMatrix cornerTiles edgePath = m
  where
    indices = findIndices (`elem` cornerTiles) edgePath ++ [length edgePath]
    edgePath' = edgePath ++ [head edgePath]
    [a, b, c, d] = [(drop i . take (j + 1)) edgePath' | (i, j) <- zip indices (tail indices)]
    ncols = length a
    nrows = length b
    m =
      setMatrixCol ncols b $
        setMatrixCol 1 (reverse d) $
          setMatrixRow nrows (reverse c) $
            setMatrixRow 1 a $
              Matrix.zero nrows ncols

findEdgePath :: Int -> [(Int, Int)] -> [Int]
findEdgePath x links
  | null tilesFromX = [x]
  | otherwise = x : findEdgePath (head tilesFromX) links'
  where
    tilesFromX = map snd $ filter ((== x) . fst) links
    links' = filter (\(a, b) -> a /= x && b /= x) links

findLinks :: M.Map Int (Matrix.Matrix Char) -> [(Int, Int)]
findLinks tiles = [(aId, bId) | (aId, a) <- M.assocs tiles, (bId, b) <- M.assocs tiles, a /= b, tilesFit a b]

categorize :: M.Map Int (Matrix.Matrix Char) -> ([Int], [Int], [Int])
categorize tiles = (cornerTiles, edgeTiles, regularTiles)
  where
    [cornerTiles, edgeTiles, regularTiles] =
      map (nub . concat) $
        groupBy (\a b -> length a == length b) $
          sortBy (\a b -> compare (length a) (length b)) $
            group [aId | (aId, a) <- M.assocs tiles, (bId, b) <- M.assocs tiles, a /= b, tilesFit a b]

tilesFit :: Matrix.Matrix Char -> Matrix.Matrix Char -> Bool
tilesFit a b = not (null intersection)
  where
    intersection = allEdges a `intersect` allEdges b

allEdges :: Matrix.Matrix Char -> [V.Vector Char]
allEdges m = edges ++ map V.reverse edges
  where
    edges = [headMatrixRow m, lastMatrixRow m, headMatrixCol m, lastMatrixCol m]
