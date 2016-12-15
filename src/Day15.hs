{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
module Day15 where

import Test.Hspec

import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P

import Data.List
import Utils

-- I took too many time to understand the rules and parse the input...
-- 8h41
-- 8h55
-- 8h55

-- Parsing
parser = parseAll

data Disc = Disc Int Int deriving (Show)

parseAll = parseDisc `P.sepBy` (P.string "\n")

parseDisc :: P.Parser Disc
parseDisc = Disc <$ (P.string "Disc #" *> number)
            <*> (P.string " has " *> number)
            <* (P.string " positions; at time=" *> number)
            <*> (P.string ", it is at position " *> number <* P.string ".")


number = read <$> P.many (P.oneOf ['0' .. '9'])

-- Code

checkDisk timeOffset (Disc nPos startingPos) = (startingPos + timeOffset) `mod` nPos == 0
checkAllDisk discs timeOffset = all (\(tx, disk) -> checkDisk (timeOffset + tx) disk) (zip [1.. ] discs)

-- Input DSL
-- Problem DSL


-- utils


-- FIRST problem
day discs = find (checkAllDisk discs) [0 ..]

-- SECOND problem
day' discs = day (discs ++ [Disc  11 0])

-- tests and data

-- comment out and add tests
test = hspec $ it "works" $ do
  day [Disc 5 4, Disc 2 1] `shouldBe` Just 5
  day <$> content `shouldReturn` Just 400589
  day' <$> content `shouldReturn` Just 3045959

fileContent = readFile "content/day15"
content = parse parser <$> fileContent
