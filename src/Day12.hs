{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
module Day12 where

import Test.Hspec

import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P

import Utils
import Data.Functor (($>))

import qualified Data.Map as Map
import Data.Map (Map)

import AsmBunny

-- Parsing
parser :: P.Parser [Asm]
parser = instruction `P.sepBy` P.string "\n"

instruction = P.choice [copy, dec, inc, jump]

-- Problem DSL
eval :: [Asm] -> Map Register Int -> Map Register Int
eval l m = go m 0
  where
        go m offset
          | offset < length l = case (l !! offset) of
              Inc r -> go (increment r m) (offset + 1)
              Dec r -> go (decrement r m) (offset + 1)
              Copy a b -> go (cp a b m) (offset + 1)
              Jump v doffset -> if (getROI v m) /= 0
                                then go m (offset + getROI doffset m)
                                else go m (offset + 1)
          | otherwise = m

-- FIRST problem
day code = get 'a' (eval code Map.empty)

-- SECOND problem
day' code = get 'a' (eval code (Map.singleton (Register 'c') 1))

-- tests and data

-- comment out and add tests
test = hspec $ it "works" $ do
  day (parse parser exampleCode) `shouldBe` 42
  day <$> content `shouldReturn` 318007
  day' <$> content `shouldReturn` 9227661

fileContent = readFile "content/day12"
content = parse parser <$> fileContent

exampleCode = "\
\cpy 41 a\n\
\inc a\n\
\inc a\n\
\dec a\n\
\jnz a 2\n\
\dec a"
