module Day3 where

import Test.Hspec

import Data.List (sort, transpose)

import Data.List.Split (chunksOf)

parse :: String -> [[Integer]]
parse = chunksOf 3 . map read . words

-- Problem DSL

isTriangle l = (a + b) > c
  where
    [a, b, c] = sort l

-- utils
score = length . filter isTriangle

-- FIRST problem
day = score

-- SECOND problem

tr = chunksOf 3 . mconcat . transpose

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
