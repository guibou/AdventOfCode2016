{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Day23 where

import Test.Hspec

import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P

import Utils

import qualified Data.Map.Strict as Map
import Data.Map (Map)

import Debug.Trace

import AsmBunny

-- Parsing
parser :: P.Parser [Asm]
parser = instruction `P.sepBy` P.string "\n"

instruction = P.choice [copy, dec, inc, jump, toggle]

-- Input DSL
eval :: [Asm] -> Map Register Int -> Map Register Int
eval l'  m = eval2 l' m 0
  where
        eval2 l' m offset = go (optim l') m offset
        go l m offset
          | offset < length l = case (l !! offset) of
              Inc r -> go l (increment r m) (offset + 1)
              Dec r -> go l (decrement r m) (offset + 1)
              Copy a b -> go l (cp a b m) (offset + 1)
              Jump v doffset -> if (getROI v m) /= 0
                                then go l m (offset + getROI doffset m)
                                else go l m (offset + 1)
              Toggle roi -> let deltaO = getROI roi m
                            in eval2 (toggleEval l (offset + deltaO)) m (offset + 1)
              Skip -> go l m (offset + 1)
              Add ra rb -> go l (add ra rb m) (offset + 1)
              AddMul ra rb rc -> go l (addmul ra rb rc m) (offset + 1)
          | otherwise = m

pattern INC a = Inc (Register a)
pattern DEC a = Dec (Register a)
pattern JNZ r o = Jump (RegisterRI (Register r)) (IntRI o)
pattern CPY a b = Copy (RegisterRI (Register a)) (Register b)
pattern CLEAR r = Copy (IntRI 0) (Register r)


{-
a += (b * d);
d = 0;
c = 0;
-}

optim :: [Asm] -> [Asm]
optim (CPY 'b' 'c' : INC 'a' : DEC 'c' : JNZ 'c' (-2) : DEC 'd' : JNZ 'd' (-5) : xs) = AddMul (Register 'a') (Register 'b') (Register 'd') : CLEAR 'd' : CLEAR 'c' : Skip : Skip : Skip : optim xs
optim (INC 'a': DEC 'c': JNZ 'c' (-2) : xs) = Add (Register 'a') (Register 'c') : CLEAR 'c' : Skip : optim xs
optim (DEC 'd': INC 'c': JNZ 'd' (-2) : xs) = Add (Register 'c') (Register 'd') : CLEAR 'd' : Skip : optim xs
optim (INC 'a': INC 'd': JNZ 'd' (-2) : xs) = Add (Register 'a') (Register 'd') : CLEAR 'd' : Skip : optim xs
optim (x:xs) = x : optim xs
optim [] = []

-- utils
toggleEval l offset = modify l offset toggle'

modify l offset f
  | offset >= length l = l
  | otherwise = let item = l !! offset
                    previous = take offset l
                    next = drop (offset + 1) l

                in previous ++ [traceShow (item, f item) (f item)] ++ next

toggle' (Inc r) = Dec r
toggle' (Dec r) = Inc r
toggle' (Toggle (RegisterRI r)) = Inc r
toggle' (Toggle (IntRI _)) = Skip
toggle' (Jump a (RegisterRI b)) = Copy a b
toggle' (Copy a b) = Jump a (RegisterRI b)
toggle' Skip = Skip

add ra rb m = Map.insert ra (getRegister ra m + getRegister rb m) m
addmul ra rb rc m = Map.insert ra (getRegister rb m * getRegister rc m) m

-- Problem DSL


-- utils


-- FIRST problem
day code = get 'a' (eval code (Map.singleton (Register 'a') 7))

-- SECOND problem
day' code = get 'a' (eval code (Map.singleton (Register 'a') 12))

-- tests and data

-- comment out and add tests
test = hspec $ it "works" $ do
  day <$> content `shouldReturn` 11662
  day' <$> content `shouldReturn` 479008222

fileContent = readFile "content/day23"
content = parse parser <$> fileContent


-- 11h05 : start
-- 11h16 : pause
-- 11h55 : reprise
-- 12h10 : star 1
-- 12h51 : star 2

simpleExample = parse parser "\
\cpy 2 a\n\
\tgl a\n\
\tgl a\n\
\tgl a\n\
\cpy 1 a\n\
\dec a\n\
\dec a"
