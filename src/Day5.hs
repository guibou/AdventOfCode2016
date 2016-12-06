{-# LANGUAGE OverloadedStrings #-}
module Day5 where

import Test.Hspec

import Crypto.Hash.MD5

import qualified Data.ByteString.Char8 as BS

import Data.List (find)

import Data.Monoid ((<>))
import Data.Char (chr)

import Data.ByteString.Base16

-- Problem DSL
ids idx = map (\x -> idx <> (BS.pack (show x))) [0..]

allMd5 idx = map (encode . (BS.take 4 . hash)) (ids idx)

isChar x = "00000" `BS.isPrefixOf` x
-- utils

allChars idx = filter isChar (allMd5 idx)

-- FIRST problem
day code = let
  codes = allChars code
  chars = map (flip BS.index 5) codes

  in take 8 chars

-- SECOND problem
day' code = let
  codes = allChars code
  in take 8 (map (findOffset codes) [0..7])

findOffset l i = let Just s = find (\x -> (BS.index x 5) == chr (i + 48)) l
                 in BS.index s 6

-- tests and data

test = hspec $ do
  describe "firstProblem" $ do
    it "dosometthing" $ do
      day "abc" `shouldBe` "18f47a30"
      day content `shouldBe` "f97c354d"

      day' "abc" `shouldBe` "05ace8e3"
      day' content `shouldBe` "863dde27"

content = "reyedfim"
