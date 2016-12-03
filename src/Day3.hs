module Day3 where

import Test.Hspec

import Data.List (sort, transpose)

parse :: String -> [[Integer]]
parse s = pack3 (map read (words s))

pack3 [] = []
pack3 (x:y:z:xs) = [x, y, z]:pack3 xs

-- Problem DSL

isTriangle l = (a + b) > c
  where
    [a, b, c] = sort l

-- utils
score = length . filter isTriangle

-- FIRST problem
day = score

-- SECOND problem

tr code = pack3 (mconcat (transpose code))

day' = score . tr

-- tests and data

test = hspec $ do
  describe "firstProblem" $ do
    it "works" $ do
      day <$> content `shouldReturn` 917
  describe "secondProblem" $ do
    it "works" $ do
      day' <$> content `shouldReturn` 1649

fileContent = readFile "content/day3"
content = parse <$> fileContent
