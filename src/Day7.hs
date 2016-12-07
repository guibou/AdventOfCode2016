module Day7 where

import Test.Hspec

import Data.List.Split
import Data.List (isInfixOf)

-- Input DSL

-- Parsing

parseAll = map parse . lines

parse = oneTwo . tokenize

tokenize = splitOneOf "[]"

oneTwo l = go [] [] l
  where
    go acca accb []  = (acca, accb)
    go acca accb [x] = (x:acca, accb)
    go acca accb (x:y:xs) = go (x:acca) (y:accb) xs

-- FIRST problem
isABBA l@(a:b:b':a':_)
  | a == a' && b == b' && a /= b = True
  | otherwise = isABBA (tail l)
isABBA _ = False


validTLS (supernet, hypernet) = any isABBA supernet && all (not . isABBA) hypernet

day = length . filter validTLS

-- SECOND problem
findABAs l@(a:b:a':_)
  | a == a' && b /= a = (a:b:a':[]) : findABAs (tail l)
  | otherwise = findABAs (tail l)
findABAs _ = []

getBAB [a, b, _] = [b, a, b]

validSSL (supernet, hypernet) = let abas = mconcat (map findABAs supernet)
                                in any (\aba -> any (\x -> (getBAB aba) `isInfixOf` x) hypernet) abas


day' = length . filter validSSL

-- tests and data

test = hspec $ it "works" $ do
  let vTLS = validTLS . parse
  vTLS "abba[mnop]qrst" `shouldBe` True
  vTLS "abcd[bddb]xyyx" `shouldBe` False
  vTLS "aaaa[qwer]tyui" `shouldBe` False
  vTLS "ioxxoj[asdfgh]zxcvbn" `shouldBe` True

  let vSSL = validSSL . parse
  vSSL "aba[bab]xyz" `shouldBe` True
  vSSL "xyx[xyx]xyx" `shouldBe` False
  vSSL "aaa[kek]eke" `shouldBe` True
  vSSL "zazbz[bzb]cdb" `shouldBe` True

  day <$> content `shouldReturn` 110

  day' <$> content `shouldReturn` 242

fileContent = readFile "content/day7"
content = parseAll <$> fileContent
