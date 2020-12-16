module Lib where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

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
