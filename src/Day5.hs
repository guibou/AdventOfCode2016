module Day5 where

import Test.Hspec

import Data.Hash.MD5
import Data.List (isPrefixOf, find)
import Safe (readMay)

-- Problem DSL
ids idx = map (\x -> idx ++ show x) [0..]
allMd5 idx = map (\x -> (x, (md5s . Str) x)) (ids idx)

isChar x = "00000" `isPrefixOf` x
-- utils

allChars idx = filter (isChar . snd) (allMd5 idx)

-- FIRST problem
day code = let
  codes = allChars code
  chars = map ((!!5) . snd) codes

  in take 8 chars

-- SECOND problem
day' code = let
  codes = allChars code
  in take 8 (map (findOffset codes) [0..7])

findOffset :: [String] -> Int -> Char
findOffset l i = let Just s = find (\x -> case readMay (x !! 5 : []) of
                                       Just i' -> i == i'
                                       Nothing -> False)
                                     l
                 in s !! 6

-- tests and data

test = hspec $ do
  describe "firstProblem" $ do
    it "dosometthing" $ do
      day "abc" `shouldBe` "18f47a30"
      day content `shouldBe` "f97c354d"

      day' "abc" `shouldBe` "05ace8e3"
      day' content `shouldBe` "863dde27"

content = "reyedfim"
