module Day17 where

import qualified Data.Vector as V
import Lib

-- Grid format: W (Z (X (Y))), i.e.
--     X (Y) = 2D square
--     Z (X (Y)) = 3D cube
--     W (Z (X (Y))) = 4D tesseract
--
-- indexing done via (x, y, z, w)
newtype Grid = Grid {values :: V.Vector (V.Vector (V.Vector (V.Vector Char)))} deriving (Eq)

prettyGrid :: Grid -> String
prettyGrid (Grid values) = prettyW 0 values
  where
    prettyW i ws
      | V.null ws = "\n"
      | otherwise = "\nw=" ++ show i ++ "\n" ++ prettyZ 0 (V.head ws) ++ prettyW (i + 1) (V.tail ws)
    prettyZ i zs
      | V.null zs = "\n"
      | otherwise = "\nz=" ++ show i ++ "\n" ++ prettyX (V.head zs) ++ prettyZ (i + 1) (V.tail zs)
    prettyX xs
      | V.null xs = ""
      | otherwise = prettyY (V.head xs) ++ prettyX (V.tail xs)
    prettyY ys
      | V.null ys = "\n"
      | otherwise = V.head ys : prettyY (V.tail ys)

instance Show Grid where
  show = prettyGrid

solvePart1 :: String -> Int
solvePart1 = countActiveCubes . simulate False 6 . parseInput

solvePart2 :: String -> Int
solvePart2 = countActiveCubes . simulate True 6 . parseInput

parseInput :: String -> Grid
parseInput = listsToGrid . (: []) . lines

listsToGrid :: [[[Char]]] -> Grid
listsToGrid = Grid . V.singleton . V.fromList . map (V.fromList . map V.fromList)

countActiveCubes :: Grid -> Int
countActiveCubes (Grid ws) = (V.sum . V.map (V.sum . V.map (V.sum . V.map (V.length . V.filter (== '#'))))) ws

simulate :: Bool -> Int -> Grid -> Grid
simulate _ 0 g = g
simulate use4D n g = simulate use4D (n - 1) $ f gWrapped gWrapped (coordsToCheck gWrapped)
  where
    gWrapped = wrapGrid use4D g
    f gIn gOut [] = gOut
    f gIn gOut (a : as) = f gIn (applyRules use4D gIn gOut a) as

coordsToCheck :: Grid -> [(Int, Int, Int, Int)]
coordsToCheck g = [(x, y, z, w) | x <- [0 .. lenX - 1], y <- [0 .. lenY - 1], z <- [0 .. lenZ - 1], w <- [0 .. lenW - 1]]
  where
    (lenX, lenY, lenZ, lenW) = gridLengths g

wrapGrid :: Bool -> Grid -> Grid
wrapGrid use4D g@(Grid values) = g'
  where
    g' = if use4D then g4D else g3D
    g3D = Grid values'
    g4D = Grid (wrapperW V.++ values' V.++ wrapperW)
    (wrapperX, wrapperY, wrapperZ, wrapperW) = gridWrappers g
    values' =
      V.map
        ( \w ->
            wrapperZ
              V.++ V.map
                ( \z ->
                    wrapperX
                      V.++ V.map
                        ( \x -> wrapperY V.++ x V.++ wrapperY
                        )
                        z
                      V.++ wrapperX
                )
                w
              V.++ wrapperZ
        )
        values

gridWrappers ::
  Grid ->
  ( V.Vector (V.Vector Char),
    V.Vector Char,
    V.Vector (V.Vector (V.Vector Char)),
    V.Vector (V.Vector (V.Vector (V.Vector Char)))
  )
gridWrappers g = (V.singleton wrapperX, V.singleton wrapperY, V.singleton wrapperZ, V.singleton wrapperW)
  where
    (lenX, lenY, lenZ, lenW) = gridLengths g
    wrapperW = V.replicate (lenZ + 2) wrapperZ
    wrapperZ = V.replicate (lenX + 2) wrapperX
    wrapperX = V.replicate (lenY + 2) wrapperY
    wrapperY = '.'

gridLengths :: Grid -> (Int, Int, Int, Int)
gridLengths (Grid values) = (lenX, lenY, lenZ, lenW)
  where
    lenW = V.length values
    lenZ = if lenW > 0 then V.length (V.head values) else 0
    lenX = if lenZ > 0 then V.length (V.head $ V.head values) else 0
    lenY = if lenX > 0 then V.length (V.head $ V.head $ V.head values) else 0

applyRules :: Bool -> Grid -> Grid -> (Int, Int, Int, Int) -> Grid
applyRules use4D g g' (x, y, z, w)
  | cube == '#' && (activeNeighborsCount /= 2 && activeNeighborsCount /= 3) = updateGrid g' (x, y, z, w) '.'
  | cube == '.' && (activeNeighborsCount == 3) = updateGrid g' (x, y, z, w) '#'
  | otherwise = g'
  where
    cube = g ! (x, y, z, w)
    neighbors = map (g !) (neighborCoords use4D (x, y, z, w))
    activeNeighborsCount = count '#' neighbors

(!) :: Grid -> (Int, Int, Int, Int) -> Char
(!) (Grid values) (x, y, z, w) = y'
  where
    w' = if w >= 0 && w < V.length values then values V.! w else V.singleton (V.singleton V.empty)
    z' = if z >= 0 && z < V.length w' then w' V.! z else V.singleton V.empty
    x' = if x >= 0 && x < V.length z' then z' V.! x else V.empty
    y' = if y >= 0 && y < V.length x' then x' V.! y else '.'

updateGrid :: Grid -> (Int, Int, Int, Int) -> Char -> Grid
updateGrid (Grid ws) (x, y, z, w) v = Grid ws'
  where
    ws' = ws `V.unsafeUpd` [(w, zs')]
    zs = ws V.! w
    zs' = zs `V.unsafeUpd` [(z, xs')]
    xs = zs V.! z
    xs' = xs `V.unsafeUpd` [(x, ys')]
    ys = xs V.! x
    ys' = ys `V.unsafeUpd` [(y, v)]

neighborCoords :: Bool -> (Int, Int, Int, Int) -> [(Int, Int, Int, Int)]
neighborCoords use4D (x, y, z, w) =
  [ (x', y', z', w')
    | x' <- pointRange x 1,
      y' <- pointRange y 1,
      z' <- pointRange z 1,
      w' <- if use4D then pointRange w 1 else [0],
      (x', y', z', w') /= (x, y, z, w),
      isMaxDiff x x' 1,
      isMaxDiff y y' 1,
      isMaxDiff z z' 1,
      isMaxDiff w w' 1
  ]

pointRange :: Int -> Int -> [Int]
pointRange a scope = [a - scope .. a + scope]

isMaxDiff :: Int -> Int -> Int -> Bool
isMaxDiff a b max' = diff >= 0 && diff <= max'
  where
    diff = abs (a - b)
