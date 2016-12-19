{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# LANGUAGE ViewPatterns #-}
module Day19 where

import Test.Hspec

import qualified Data.Sequence as Seq

-- FIRST problem

-- First solution I wrote in 7 minutes really efficient on list
dayLoop :: [Int] -> [Int] -> Int
dayLoop (x:_:xs) l = dayLoop (xs) (x:l)
dayLoop [x] [] = x
dayLoop l l' = dayLoop (l ++ reverse l') []

day nElves = dayLoop [1 .. nElves] []

-- I wrote this solution for fun later, to see how Sequence behaves on this problem
dayAlt nElves = loop (Seq.fromList [1 .. nElves])
dayAlt nElves = loop (Seq.fromList [1 .. nElves])

loop :: Seq.Seq Int -> Int
loop (Seq.viewl -> (x Seq.:< s)) = case Seq.viewl s of
  Seq.EmptyL -> x
  (_ Seq.:< s') -> loop (s' Seq.|> x)


-- SECOND problem
-- Written in 34 minutes (counting first solution)
-- I lose lot of time to discover the Sequence API and install containers-0.5.8.1 which contains `deleteAt`
-- ViewPatterns are cool !
loop' :: Seq.Seq Int -> Int
loop' (Seq.viewl -> (x Seq.:< s))
  | Seq.null s = x
  | otherwise = let s' = Seq.deleteAt ((Seq.length s - 1) `div` 2) s
                in loop' (s' Seq.|> x)

day' nElves = loop' (Seq.fromList [1 .. nElves])

-- tests and data
test = hspec $ it "works" $ do
  day input `shouldBe` 1808357
  dayAlt input `shouldBe` 1808357
  day' input `shouldBe` (1407007)

input = 3001330 :: Int

-- 12h30
-- 12h37 star 1.
-- Time to compile the new sequence package to get Seq.deleteAt
-- 13h04 star 2

