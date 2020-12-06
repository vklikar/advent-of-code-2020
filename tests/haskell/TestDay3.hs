module TestDay3 where

import Day3
import Test.HUnit

example =
  [ "..##.......",
    "#...#...#..",
    ".#....#..#.",
    "..#.#...#.#",
    ".#...##..#.",
    "..#.##.....",
    ".#.#.#....#",
    ".#........#",
    "#.##...#...",
    "#...##....#",
    ".#..#...#.#"
  ]

exampleExpanded =
  [ "..##.........##.........##.........##.........##.........##.......",
    "#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..",
    ".#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.",
    "..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#",
    ".#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.",
    "..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....",
    ".#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#",
    ".#........#.#........#.#........#.#........#.#........#.#........#",
    "#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...",
    "#...##....##...##....##...##....##...##....##...##....##...##....#",
    ".#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#"
  ]

testAdjust :: Test
testAdjust = TestList [TestCase $ assertEqual "" ".#.##.####" (adjust 3 1 exampleExpanded)]

testExampleExpanded :: Test
testExampleExpanded = TestList [TestCase $ assertEqual "" 7 (traverseMap [(3, 1)] exampleExpanded)]

testExample :: Test
testExample = TestList [TestCase $ assertEqual "" 7 (traverseMap [(3, 1)] example)]

-- Part 2
testMultipleSlopes :: Test
testMultipleSlopes = TestList [TestCase $ assertEqual "" 336 (traverseMap [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)] example)]

runTests :: IO Counts
runTests = runTestTT $ TestList [testAdjust, testExampleExpanded, testExample, testMultipleSlopes]
