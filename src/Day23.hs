{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Day23 where

import Test.Hspec

import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P

import Data.Functor (($>))
import Utils

import qualified Data.Map.Strict as Map

import Debug.Trace

import AsmBunny

-- Parsing
parser :: P.Parser [AsmToggle]
parser = instruction `P.sepBy` P.string "\n"

instruction = P.choice (toggle : map (BasicAsm <$>) [copy, dec, inc, jump])

toggle = (P.string "tgl" $> Toggle) <*> parseRegisterOrInt

-- Input DSL
data AsmToggle = BasicAsm Asm
               | Toggle RegisterOrInt
               | Skip
               deriving (Show)
data AsmToggleOptimised = BasicAsmToggle AsmToggle
                        | Add Register Register
                        | AddMul Register Register Register
               deriving (Show)

eval :: [AsmToggle] -> Computer -> Computer
eval lNotOptimised = go
  where
        lOptimised = optim lNotOptimised
        go m
          | pc m < length lOptimised = let instr = lOptimised !! pc m
                              in case instr of
                                   BasicAsmToggle (Toggle roi) -> let delta0 = getROI roi (registers m)
                                                 in eval (toggleEval lNotOptimised (pc m + delta0)) (incPc m)
                                   BasicAsmToggle (BasicAsm asm) -> go $ evalAsm asm m
                                   BasicAsmToggle Skip -> go $ incPc m
                                   Add ra rb -> go $ incPc (modifyRegisters (add ra rb) m)
                                   AddMul ra rb rc -> go $ incPc (modifyRegisters (addmul ra rb rc) m)
          | otherwise = m



pattern INC :: Char -> AsmToggle
pattern INC a = BasicAsm (Inc (Register a))

pattern DEC :: Char -> AsmToggle
pattern DEC a = BasicAsm (Dec (Register a))

pattern JNZ :: Char -> Int -> AsmToggle
pattern JNZ r o = BasicAsm (Jump (RegisterRI (Register r)) (IntRI o))

pattern CPY :: Char -> Char -> AsmToggle
pattern CPY a b = BasicAsm (Copy (RegisterRI (Register a)) (Register b))

pattern CLEAR :: Char -> AsmToggleOptimised
pattern CLEAR r = BasicAsmToggle (BasicAsm (Copy (IntRI 0) (Register r)))


{-
a += (b * d);
d = 0;
c = 0;
-}

skip = BasicAsmToggle Skip

nullOptim = map BasicAsmToggle

optim :: [AsmToggle] -> [AsmToggleOptimised]
optim (CPY 'b' 'c' : INC 'a' : DEC 'c' : JNZ 'c' (-2) : DEC 'd' : JNZ 'd' (-5) : xs) = AddMul (Register 'a') (Register 'b') (Register 'd') : CLEAR 'd' : CLEAR 'c' : skip : skip : skip : optim xs
optim (INC 'a': DEC 'c': JNZ 'c' (-2) : xs) = Add (Register 'a') (Register 'c') : CLEAR 'c' : skip : optim xs
optim (DEC 'd': INC 'c': JNZ 'd' (-2) : xs) = Add (Register 'c') (Register 'd') : CLEAR 'd' : skip : optim xs
optim (INC 'a': INC 'd': JNZ 'd' (-2) : xs) = Add (Register 'a') (Register 'd') : CLEAR 'd' : skip : optim xs
optim (x:xs) = BasicAsmToggle x : optim xs
optim [] = []

-- utils
toggleEval l offset = modify l offset toggle'

modify l offset f
  | offset >= length l = l
  | otherwise = let item = l !! offset
                    previous = take offset l
                    next = drop (offset + 1) l

                in previous ++ [traceShow (item, f item) (f item)] ++ next

toggle' (BasicAsm (Inc r)) = BasicAsm (Dec r)
toggle' (BasicAsm (Dec r)) = BasicAsm (Inc r)
toggle' (BasicAsm (Jump a (RegisterRI b))) = BasicAsm (Copy a b)
toggle' (BasicAsm (Copy a b)) = BasicAsm (Jump a (RegisterRI b))
toggle' (Toggle (RegisterRI r)) = BasicAsm (Inc r)
toggle' (Toggle (IntRI _)) = Skip
toggle' Skip = Skip

add ra rb m = Map.insert ra (getRegister ra m + getRegister rb m) m
addmul ra rb rc m = Map.insert ra (getRegister rb m * getRegister rc m) m

-- Problem DSL


-- utils


-- FIRST problem
day code = get 'a' (eval code (computerWithRegisters [(Register 'a', 7)]))

-- SECOND problem
day' code = get 'a' (eval code (computerWithRegisters [(Register 'a', 12)]))

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
