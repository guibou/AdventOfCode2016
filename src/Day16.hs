{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
module Day16 where

import Test.Hspec

import qualified Data.ByteString.Char8 as BS

import Data.List
import Data.Monoid ((<>))

-- Input DSL
step a = let b = (BS.map rep . BS.reverse) a
         in mconcat [a, "0", b]
  where rep '0' = '1'
        rep '1' = '0'

okLength len l = let Just res = find (\x -> BS.length x >= len) l
                 in BS.take len res

checksum (x:y:xs)
  | x == y = '1' : checksum xs
  | otherwise = '0' : checksum xs
checksum [] = []

checksum' v = let Just res = find (\x -> odd (length (x))) (iterate checksum (checksum (BS.unpack v)))
              in res

-- Problem DSL
doit len content = checksum' (okLength len (iterate step content))


-- utils


-- FIRST problem
day content = doit 272 content

-- SECOND problem(okLength 272 (iterate step content))
day' content = doit 35651584 content
-- tests and data

-- comment out and add tests
test = hspec $ it "works" $ do
  step "111100001010" `shouldBe` "1111000010100101011110000"
  checksum' "110010110100" `shouldBe` "100"

  doit 20 "10000" `shouldBe` "01100"

  day content `shouldBe` "01110011101111011"
  day' content `shouldBe` "11001111011000111"

content = "11110010111001001" :: BS.ByteString
