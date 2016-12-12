{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
module DayX where

import Test.Hspec

import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P

import Utils

-- Parsing
parser = undefined

-- Input DSL


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
content = parse parser <$> fileContent
