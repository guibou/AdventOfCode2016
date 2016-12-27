{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
module Day12 where

import Test.Hspec

import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P

import Utils

import AsmBunny

-- Parsing
parser :: P.Parser [Asm]
parser = instruction `P.sepBy` P.string "\n"

instruction = P.choice [copy, dec, inc, jump]

-- Problem DSL
eval :: [Asm] -> Computer -> Computer
eval l = go
  where
        go m = case l !? (pc m) of
          Nothing -> m
          Just instr -> go (evalAsm instr m)

-- FIRST problem
day code = get 'a' (eval code emptyComputer)

-- SECOND problem
day' code = get 'a' (eval code (computerWithRegisters [((Register 'c'), 1)]))

-- tests and data

-- comment out and add tests
test = hspec $ it "works" $ do
  day exampleCode `shouldBe` 42
  day <$> content `shouldReturn` 318007
  day' <$> content `shouldReturn` 9227661

fileContent = readFile "content/day12"
content = parse parser <$> fileContent

exampleCode = parse parser "\
\cpy 41 a\n\
\inc a\n\
\inc a\n\
\dec a\n\
\jnz a 2\n\
\dec a"
