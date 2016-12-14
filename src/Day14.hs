{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
module Day14 where

import Test.Hspec

import qualified Data.ByteString.Char8 as BS

import Data.Monoid ((<>))
import Utils (md5)

import Data.List
import Data.Maybe

-- Input DSL
-- Problem DSL
ids idx = map (\x -> idx <> (BS.pack (show x))) [0 :: Int ..]

stretchedmd5 s = (iterate md5 s) !! 2017

allMd5 :: (BS.ByteString -> BS.ByteString) -> BS.ByteString -> [BS.ByteString]
allMd5 md5f idx = map md5f (ids idx)

n = 4
nKeys = 512
rep = 7
bigValue = 100000

{-
n = 3
nKeys = 64
rep = 5
bigValue = 1000
-}

findTriple bs = case findN n bs of
  (x:_) -> Just x
  _ -> Nothing

findN n bs
  | BS.length bs < n = []
  | all (\i -> c == at i) [1.. (n - 1)] = c : next
  | otherwise = next
  where at = BS.index bs
        c = at 0
        next = findN n (BS.drop 1 bs)

findBlork x = case findTriple x of
  Just c -> Just (c, findN rep x, x)
  Nothing -> Nothing

problem2 :: [BS.ByteString] -> [(Int, (Char, [Char], BS.ByteString))]
problem2 l = catMaybes ((map (\(i, mb) -> (i,) <$> mb)) ((zip [0..]) (map findBlork l)))
  where keep (_, (Just v)) = True
        keep _ = False

-- Problem DSL
filterKeys :: [(Int, (Char, [Char], BS.ByteString))] -> [(Int, BS.ByteString)]
filterKeys ((idx, (c, _, x)):xs) = if any (\(_, (_, l, _)) -> c `elem` l) (takeWhile (\(idx', _) -> idx' <= idx + bigValue) xs)
                                     then (idx, x) : filterKeys xs
                                     else filterKeys xs
filterKeys [] = []

-- utils
wrapit f code = take nKeys $ filterKeys (problem2 (allMd5 f code))

-- FIRST problem
day = wrapit md5

-- SECOND problem
day' = wrapit stretchedmd5

-- tests and data

-- comment out and add tests
test = hspec $ it "works" $ do
  --(fst . last) (day "yjdafjpo") `shouldBe` 86054
  (fst. last ) (day content) `shouldBe` 15035
  (fst . last ) (day' content) `shouldBe` 19968

content = "ihaygndm" :: BS.ByteString

otherProblem = "yjdafjpo" :: BS.ByteString

-- 8h52
-- 9h05,

-- res (9395299,"7781fe7777c1821e02a344d7d3b0f18b")]

add9 i = "9" ++ show i ++ "9"

fastSolution = length (takeWhile (\x -> read (add9 x) /= 9395299) (filter test [0 ..]))
  where
    test i = let s = add9 i
                 digits = map (\x -> read (x:[])) s
             in sum digits == 46 && product digits == 196830
