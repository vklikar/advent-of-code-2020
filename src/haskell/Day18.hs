module Day18 where

import Language.Haskell.Interpreter hiding (eval)
import Lib

infixl 7 +?

infixl 8 +!

class AddPart1 a where
  (+?) :: a -> a -> a

instance AddPart1 Int where
  (+?) = (+)

class AddPart2 a where
  (+!) :: a -> a -> a

instance AddPart2 Int where
  (+!) = (+)

-- solutions made using Haskell Interpreter: working, but quite slow
solvePart1 :: String -> IO Int
solvePart1 input = sum <$> mapM (eval . replace "+" "+?") (parseInput input)

solvePart2 :: String -> IO Int
solvePart2 input = sum <$> mapM (eval . replace "+" "+!") (parseInput input)

parseInput :: String -> [String]
parseInput = lines

eval :: String -> IO Int
eval s = do
  Right result <- runInterpreter $ setImports ["Prelude", "Day18"] >> interpret s (as :: Int)
  return result
