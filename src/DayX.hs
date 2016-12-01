module DayX where

import Test.Hspec

import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P

-- Input DSL

-- Parsing

parse s = P.parse parser "" s

parser = undefined


-- Problem DSL


-- utils


-- FIRST problem
day code = code

-- SECOND problem
day' code = code * 2

-- tests and data

test = hspec $ do
  describe "firstProblem" $ do
    it "works" $ do
      day 1 `shouldBe` (1 :: Int)
  describe "secondProblem" $ do
    it "works" $ do
      day' 2 `shouldBe` 4

fileContent = readFile "content/dayX"
content = parse <$> fileContent
