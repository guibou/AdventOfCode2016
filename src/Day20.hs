{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
module Day20 where

import Test.Hspec

import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P

import Utils

import Data.List.Split

import Data.List
import Data.Ord

-- Parsing
data Range = Range Int Int deriving (Show)

parser s = map parseLine (lines s)

parseLine l = let [a, b] = splitOn "-" l
              in Range (read a) (read b)

inRange (Range a b) v = v >= a && v <= b

-- Input DSL

lowBound (Range a _) = a

sortIp ips = sortBy (comparing lowBound) ips

compactIp (r0@(Range a b):((Range b' c):xs))
  | inRange r0 b' = compactIp (Range a (max b c) : xs)
  | otherwise = r0 : compactIp ((Range b' c) : xs)
compactIp l = l

-- Problem DSL


-- utils
validIp ips x= all (\ip -> not (inRange ip x)) ips

-- FIRST problem
day ips = find (validIp ips) [0 .. ]

-- SECOND problem
day' ips = countAvailable ips 4294967295

countAvailable ((Range _ b):(xs@((Range c _):_))) maxRange = (c - b - 1) + countAvailable xs maxRange
countAvailable [(Range _ d)] maxRange = maxRange - d
countAvailable [] _ = error "WTF"

-- tests and data

-- comment out and add tests
test = hspec $ it "works" $ do
  day <$> content `shouldReturn` (Just 4793564)
  day' <$> content `shouldReturn` 146

fileContent = readFile "content/day20"
content = regIp . parser <$> fileContent

regIp = compactIp . sortIp

ipTests = regIp [Range 5 8, Range 0 2, Range 4 7]

-- 11h44
-- 11h53 4793564
-- 11h57
