module Lib where

import Data.List

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

listToPair :: [a] -> (a,a)
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
