module Lib where

import Data.List

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

listToPair :: [a] -> (a,a)
listToPair [x, y] = (x, y)

combinations :: Int -> [a] -> [[a]]
combinations n xs = filter ((n ==) . length) $ subsequences xs
