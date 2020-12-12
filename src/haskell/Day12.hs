module Day12 where

solvePart1 :: String -> Int
solvePart1 = manhattanDistance navigateByDirection 0 (0, 0) . parseInput

solvePart2 :: String -> Int
solvePart2 = manhattanDistance navigateByWaypoint (10, 1) (0, 0) . parseInput

parseInput :: String -> [(Char, Int)]
parseInput = map (\x -> (head x, read (tail x))) . lines

manhattanDistance :: (a -> (Int, Int) -> (Char, Int) -> (a, (Int, Int))) -> a -> (Int, Int) -> [(Char, Int)] -> Int
manhattanDistance _ _ (e, n) [] = abs e + abs n
manhattanDistance f a ship (x : xs) = manhattanDistance f a' ship' xs
  where
    (a', ship') = f a ship x

navigateByDirection :: Int -> (Int, Int) -> (Char, Int) -> (Int, (Int, Int))
navigateByDirection direction (e, n) ('E', x) = (direction, (e + x, n))
navigateByDirection direction (e, n) ('N', x) = (direction, (e, n + x))
navigateByDirection direction (e, n) ('W', x) = (direction, (e - x, n))
navigateByDirection direction (e, n) ('S', x) = (direction, (e, n - x))
navigateByDirection direction (e, n) ('L', x) = (nextDirection direction (- x), (e, n))
navigateByDirection direction (e, n) ('R', x) = (nextDirection direction x, (e, n))
navigateByDirection direction ship ('F', x) = navigateByDirection direction ship (degreesToCardinalPoint direction, x)

navigateByWaypoint :: (Int, Int) -> (Int, Int) -> (Char, Int) -> ((Int, Int), (Int, Int))
navigateByWaypoint (we, wn) ship ('E', x) = ((we + x, wn), ship)
navigateByWaypoint (we, wn) ship ('N', x) = ((we, wn + x), ship)
navigateByWaypoint (we, wn) ship ('W', x) = ((we - x, wn), ship)
navigateByWaypoint (we, wn) ship ('S', x) = ((we, wn - x), ship)
navigateByWaypoint waypoint ship ('L', x) = (rotate waypoint (- x), ship)
navigateByWaypoint waypoint ship ('R', x) = (rotate waypoint x, ship)
navigateByWaypoint (we, wn) (se, sn) ('F', x) = ((we, wn), (se + we * x, sn + wn * x))

nextDirection :: Int -> Int -> Int
nextDirection direction n = (direction + n) `mod` 360

degreesToCardinalPoint :: Int -> Char
degreesToCardinalPoint 0 = 'E'
degreesToCardinalPoint 90 = 'S'
degreesToCardinalPoint 180 = 'W'
degreesToCardinalPoint 270 = 'N'

rotate :: (Int, Int) -> Int -> (Int, Int)
rotate (e, n) d
  | d == 0 = (e, n)
  | abs d == 180 = (- e, - n)
  | d == 90 || d == -270 = (n, - e)
  | d == 270 || d == -90 = rotate (- e, - n) 90
