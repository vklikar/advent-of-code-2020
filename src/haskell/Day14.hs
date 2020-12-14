module Day14 where

import Data.Attoparsec.Text as A
import Data.Either
import qualified Data.Map as M
import qualified Data.Text as T
import Lib

solvePart1 :: String -> Integer
solvePart1 = memorySum . executeProgram M.empty decoder1 . parseInput

solvePart2 :: String -> Integer
solvePart2 = memorySum . executeProgram M.empty decoder2 . parseInput

parseInput :: String -> [(String, [(Integer, String)])]
parseInput = fromRight [] . parseOnly parseProgram . T.pack

parseProgram :: Parser [(String, [(Integer, String)])]
parseProgram = do many1 parseBlock

parseBlock :: Parser (String, [(Integer, String)])
parseBlock = do
  mask <- parseMask
  write <- many1 parseWrite
  return (mask, write)

parseMask :: Parser String
parseMask = do
  string "mask = "
  mask <- A.takeWhile (/= '\n')
  endOfLine
  return $ T.unpack mask

parseWrite :: Parser (Integer, String)
parseWrite = do
  string "mem["
  address <- decimal
  string "] = "
  value <- decimal
  endOfLine
  return (address, integerToDigits value)

memorySum :: M.Map Integer String -> Integer
memorySum = M.foldl' (+) 0 . M.map digitsToInteger

executeProgram :: t -> (t -> a -> t) -> [a] -> t
executeProgram memory _ [] = memory
executeProgram memory decoder (x : xs) = executeProgram updatedMemory decoder xs
  where
    updatedMemory = decoder memory x

decoder1 :: M.Map Integer String -> (String, [(Integer, String)]) -> M.Map Integer String
decoder1 memory (_, []) = memory
decoder1 memory (mask, (address, value) : xs) = decoder1 updatedMemory (mask, xs)
  where
    valueToWrite = applyMask1 mask value
    updatedMemory = writeToMemory memory address valueToWrite

applyMask1 :: String -> String -> String
applyMask1 = zipWith applyMaskBit1

applyMaskBit1 :: Char -> Char -> Char
applyMaskBit1 '1' _ = '1'
applyMaskBit1 '0' _ = '0'
applyMaskBit1 'X' x = x

decoder2 :: M.Map Integer String -> (String, [(Integer, String)]) -> M.Map Integer String
decoder2 memory (_, []) = memory
decoder2 memory (mask, (address, value) : xs) = decoder2 updatedMemory (mask, xs)
  where
    address' = applyMask2 mask (integerToDigits address)
    expandedAddresses = expandFloatingBits address'
    updatedMemory = f memory expandedAddresses
    f memory' [] = memory'
    f memory' (x : xs) = f (writeToMemory memory' (digitsToInteger x) value) xs

applyMask2 :: String -> String -> String
applyMask2 = zipWith applyMaskBit2

applyMaskBit2 :: Char -> Char -> Char
applyMaskBit2 '1' _ = '1'
applyMaskBit2 '0' x = x
applyMaskBit2 'X' _ = 'X'

expandFloatingBits :: String -> [String]
expandFloatingBits = f [""]
  where
    f result [] = result
    f result ('X' : xs) = f (map (++ "1") result ++ map (++ "0") result) xs
    f result (x : xs) = f (map (++ [x]) result) xs

writeToMemory :: M.Map Integer String -> Integer -> String -> M.Map Integer String
writeToMemory memory address value = M.alter (const $ Just value) address memory

