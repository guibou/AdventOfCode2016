{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
module Day14 where

import Test.Hspec

import qualified Data.ByteString.Char8 as BS

import Data.Monoid ((<>))
import Utils (md5)

-- Input DSL
-- Problem DSL
ids idx = map (\x -> idx <> (BS.pack (show x))) [0 :: Int ..]

stretchedmd5 s = (iterate md5 s) !! 2017
allMd5 md5f idx = map md5f (ids idx)

findTriple bs
  | BS.length bs < 3 = Nothing
  | at 0 == at 1 && at 1 == at 2 = Just (at 0)
  | otherwise = findTriple (BS.drop 1 bs)
  where at = BS.index bs

-- Problem DSL
filterKeys ((x, idx):xs) = case findTriple x of
  Nothing -> filterKeys xs
  Just c -> if any (\(v, _) -> (BS.replicate 5 c) `BS.isInfixOf` v) (take 1000 xs)
    then idx : filterKeys xs
    else filterKeys xs
filterKeys [] = []

-- utils
wrapit f code = filterKeys (zip (allMd5 f code) [0 :: Int ..]) !! 63

-- FIRST problem
day = wrapit md5

-- SECOND problem
day' = wrapit stretchedmd5

-- tests and data

-- comment out and add tests
test = hspec $ it "works" $ do
  day content `shouldBe` 15035
  day' content `shouldBe` 19968

content = "ihaygndm" :: BS.ByteString

-- 8h52
-- 9h05
