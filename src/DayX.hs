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

-- comment out and add tests
-- test = hspec $ it "works" $ do

fileContent = readFile "content/dayX"
content = parse <$> fileContent
