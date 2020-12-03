module Lib where

count :: Char -> String -> Int
count x = length . filter (== x)
