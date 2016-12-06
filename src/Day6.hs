module Day6 where

import Test.Hspec

import Data.List (transpose, sortOn)
import Utils

-- utils
countString s = sortOn (flip count s) ['a' .. 'z']

-- FIRST problem
day = map (last . countString) . transpose

-- SECOND problem
day' = map (head . countString) . transpose

-- tests and data

test = hspec $ do
  describe "firstProblem" $ do
    it "works" $ do
      day <$> content `shouldReturn` "wkbvmikb"
  describe "secondProblem" $ do
    it "works" $ do
      day' <$> content `shouldReturn` "evakwaga"

fileContent = readFile "content/day6"
content = lines <$> fileContent
