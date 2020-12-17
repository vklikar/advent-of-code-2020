module TestDay17 where

import Day17
import Test.HUnit

example =
  unlines
    [ ".#.",
      "..#",
      "###"
    ]

testNeighbors :: Test
testNeighbors = TestCase $ assertEqual "" 26 (length $ neighborCoords False (1, 2, 3, 0))

testGet :: Test
testGet =
  TestList
    [ TestCase $ assertEqual "" '#' ((listsToGrid [["...", "..#", "..."]]) ! (1, 2, 0, 0)),
      TestCase $ assertEqual "" '.' ((listsToGrid [["...", "..#", "..."]]) ! (3, 3, 3, 0))
    ]

testWrapGrid :: Test
testWrapGrid =
  TestCase $
    assertEqual
      ""
      ( listsToGrid
          [ replicate 5 ".....",
            [ ".....",
              ".###.",
              ".###.",
              ".###.",
              "....."
            ],
            replicate 5 "....."
          ]
      )
      (wrapGrid False $ (listsToGrid [["###", "###", "###"]]))

testPart1 :: Test
testPart1 = TestCase $ assertEqual "" 112 (solvePart1 example)

testPart2 :: Test
testPart2 = TestCase $ assertEqual "" 848 (solvePart2 example)

runTests :: IO Counts
runTests = runTestTT $ TestList [testNeighbors, testGet, testWrapGrid, testPart1, testPart2]
