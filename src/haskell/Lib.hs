module Lib where

import Data.List
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Matrix as Matrix
import qualified Data.Set as S
import qualified Data.Vector as V

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

listToPair :: [a] -> (a, a)
listToPair [x, y] = (x, y)

combinations :: Int -> [a] -> [[a]]
combinations n xs = filter ((n ==) . length) $ subsequences xs

getMatrixElem :: a -> (Int, Int) -> Int -> Int -> [[a]] -> a
getMatrixElem def (i, j) w h matrix
  | i >= h || j >= w || i < 0 || j < 0 = def
  | otherwise = (matrix !! i) !! j

setMatrixElem :: (Int, Int) -> a -> [[a]] -> [[a]]
setMatrixElem (i, j) x yys = setElem i (setElem j x (yys !! i)) yys

setElem :: Int -> a -> [a] -> [a]
setElem n x ys = take n ys ++ x : drop (n + 1) ys

integerToDigits :: Integer -> String
integerToDigits x = f 36 x
  where
    f n 0 = replicate n '0'
    f n x = f (n -1) (x `div` 2) ++ [integerToDigit (x `mod` 2)]

digitsToInteger :: String -> Integer
digitsToInteger = foldl' (\x y -> 2 * x + y) 0 . map digitToInteger

integerToDigit :: Integer -> Char
integerToDigit 1 = '1'
integerToDigit 0 = '0'

digitToInteger :: Char -> Integer
digitToInteger '1' = 1
digitToInteger '0' = 0

addToSetInMap k m v = M.alter (const $ Just s') k m
  where
    s = M.findWithDefault S.empty k m
    s' = S.insert v s

replace old new = intercalate new . splitOn old

rotateMatrixRight :: Matrix.Matrix a -> Matrix.Matrix a
rotateMatrixRight = Matrix.transpose . flipMatrixRows

rotateMatrixLeft :: Matrix.Matrix a -> Matrix.Matrix a
rotateMatrixLeft = flipMatrixRows . Matrix.transpose

rotateMatrix180 :: Matrix.Matrix a -> Matrix.Matrix a
rotateMatrix180 = rotateMatrixRight . rotateMatrixRight

flipMatrixRows :: Matrix.Matrix a -> Matrix.Matrix a
flipMatrixRows m = flipMatrixRowsOrCols Matrix.switchRows 1 (Matrix.nrows m) m

flipMatrixCols :: Matrix.Matrix a -> Matrix.Matrix a
flipMatrixCols m = flipMatrixRowsOrCols Matrix.switchCols 1 (Matrix.ncols m) m

flipMatrixRowsOrCols :: (Ord t1, Num t1) => (t1 -> t1 -> t2 -> t2) -> t1 -> t1 -> t2 -> t2
flipMatrixRowsOrCols f a b m
  | a >= b = m
  | otherwise = flipMatrixRowsOrCols f (a + 1) (b - 1) (f a b m)

headMatrixRow :: Matrix.Matrix a -> V.Vector a
headMatrixRow = Matrix.getRow 1

lastMatrixRow :: Matrix.Matrix a -> V.Vector a
lastMatrixRow m = Matrix.getRow (Matrix.nrows m) m

headMatrixCol :: Matrix.Matrix a -> V.Vector a
headMatrixCol = Matrix.getCol 1

lastMatrixCol :: Matrix.Matrix a -> V.Vector a
lastMatrixCol m = Matrix.getCol (Matrix.ncols m) m

setMatrixRow :: Int -> [a] -> Matrix.Matrix a -> Matrix.Matrix a
setMatrixRow i = f (i, 1)
  where
    f _ [] m = m
    f (i, j) (x : xs) m = f (i, j + 1) xs (Matrix.setElem x (i, j) m)

setMatrixCol :: Int -> [a] -> Matrix.Matrix a -> Matrix.Matrix a
setMatrixCol j = f (1, j)
  where
    f _ [] m = m
    f (i, j) (x : xs) m = f (i + 1, j) xs (Matrix.setElem x (i, j) m)
