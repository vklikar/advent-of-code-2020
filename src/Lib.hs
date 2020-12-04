module Lib where

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

listToPair :: [a] -> (a,a)
listToPair [x, y] = (x, y)
