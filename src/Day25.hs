{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Day25 where

import Test.Hspec

import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P

import Utils

import qualified Data.Map.Strict as Map
import Data.Map (Map)

import Data.List (find)

import AsmBunny

-- Parsing
parser :: P.Parser [Asm]
parser = instruction `P.sepBy` P.string "\n"

instruction = P.choice [copy, dec, inc, jump, out]

-- Input DSL

-- Input DSL
eval :: [Asm] -> Map Register Int -> [Int]
eval l  m = go m 0
  where
        go :: Map Register Int -> Int -> [Int]
        go m offset
          | offset < length l = case (l !! offset) of
              Inc r -> go (increment r m) (offset + 1)
              Dec r -> go (decrement r m) (offset + 1)
              Copy a b -> go (cp a b m) (offset + 1)
              Jump v doffset -> if (getROI v m) /= 0
                                then go m (offset + getROI doffset m)
                                else go m (offset + 1)
              Out roi -> getROI roi m : go m (offset + 1)
          | otherwise = []

-- Problem DSL


-- utils
blork n content = eval content (Map.singleton (Register 'a') n)

-- FIRST problem
day content = find (\x -> take 100 (blork x content) == take 100 (cycle [0, 1])) [0..]

test = hspec $ it "works" $ do
  day <$> content `shouldReturn` (Just 175)

fileContent = readFile "content/day25"
content = parse parser <$> fileContent

simpleExample = parse parser "\
\cpy 2 a\n\
\tgl a\n\
\tgl a\n\
\tgl a\n\
\cpy 1 a\n\
\dec a\n\
\dec a"

-- begin: 15:09
-- start1 : 15h19
