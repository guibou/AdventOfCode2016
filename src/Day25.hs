{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Day25 where

import Test.Hspec

import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P

import Utils

import Data.List (find)
import Data.Functor (($>))

import AsmBunny

-- Parsing
parser :: P.Parser [AsmOut]
parser = instruction `P.sepBy` P.string "\n"

instruction = P.choice (out: map (BasicAsm <$>) [copy, dec, inc, jump])

out = (P.string "out" $> Out) <*> parseRegisterOrInt

-- Input DSL
data AsmOut = BasicAsm Asm | Out RegisterOrInt deriving (Show)


-- Input DSL
eval :: [AsmOut] -> Computer -> [Int]
eval l = go
  where
        go m = case l !? (pc m) of
          Nothing -> []
          Just instr -> let (val, m') = evalAsmOut instr m
                        in case val of
                             Just v -> v : go m'
                             Nothing -> go m'

evalAsmOut (BasicAsm asm) computer = (Nothing, evalAsm asm computer)
evalAsmOut (Out roi) computer = (Just (getROI roi m), incPc computer)
  where m = registers computer

-- Problem DSL


-- utils
blork n code = eval code (computerWithRegisters [(Register 'a', n)])

-- FIRST problem
day code = find (\x -> take 100 (blork x code) == take 100 (cycle [0, 1])) [0..]

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
