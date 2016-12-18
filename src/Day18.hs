{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
module Day18 where

import Test.Hspec

import Utils

-- Parsing
parser = map toStatus

data Status = Safe | Trap deriving (Show, Eq)

toStatus '.' = Safe
toStatus '^' = Trap
toStatus _ = undefined

-- Input DSL
nextRow row = go ((Safe : row))

go (a:l@(b:c:_)) = nextTrap a b c : go l
go [a, b] = [nextTrap a b Safe]
go _ = undefined

nextTrap Trap Trap Safe = Trap
nextTrap Safe Trap Trap = Trap
nextTrap Trap Safe Safe = Trap
nextTrap Safe Safe Trap = Trap
nextTrap _ _ _ = Safe

-- Problem DSL

doit n items = sum (map (countIf (==Safe)) (take n (iterate nextRow items)))

-- FIRST problem
day = doit 40

-- SECOND problem
day' = doit 400000

-- tests and data

-- comment out and add tests
test = hspec $ it "works" $ do
  day input `shouldBe` 1987
  day' input `shouldBe` 19984714

input = parser $ ".^.^..^......^^^^^...^^^...^...^....^^.^...^.^^^^....^...^^.^^^...^^^^.^^.^.^^..^.^^^..^^^^^^.^^^..^"

-- 7h13
-- star1: 7h43 (WTF did I lost myself inside the go function ?)
-- Laptop battery dead at 7h43
-- 9h50 -- new train with a power supply
