module Day25 where

import Lib

solvePart1 :: String -> Int
solvePart1 input = encrypt fstLoopSize sndPublicKey
  where
    publicKeys = parseInput input
    sndPublicKey = snd publicKeys
    fstLoopSize = getLoopSize . fst $ publicKeys

parseInput :: String -> (Int, Int)
parseInput = listToPair . map read . words

getLoopSize :: Int -> Int
getLoopSize pk = f 1 1
  where
    f result n
      | t == pk = n
      | otherwise = f t (n + 1)
      where
        t = transformSubjectNumber 7 result

encrypt :: Int -> Int -> Int
encrypt loopSize pk = f 1 loopSize pk
  where
    f result 0 _ = result
    f result n pk = f (transformSubjectNumber pk result) (n - 1) pk

transformSubjectNumber :: Int -> Int -> Int
transformSubjectNumber subjectNumber value = (subjectNumber * value) `mod` 20201227