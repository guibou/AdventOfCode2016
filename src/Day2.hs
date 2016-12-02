module Day2 where

import Test.Hspec

import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P

-- Input DSL
data Instruction = U | D | L | R deriving (Show)

-- Parsing
parse s = P.parse parser "" s

parser :: P.Parser [[Instruction]]
parser = P.sepBy (P.many (parserInstruction)) (P.string "\n")

parserInstruction :: P.Parser Instruction
parserInstruction = (U <$ P.string "U") P.<|>
                    (D <$ P.string "D") P.<|>
                    (L <$ P.string "L") P.<|>
                    (R <$ P.string "R")

-- Problem DSL
keyPad :: Instruction -> Int -> Int
keyPad U 1 = 1
keyPad U 2 = 2
keyPad U 3 = 3
keyPad U x = x - 3
keyPad D 7 = 7
keyPad D 8 = 8
keyPad D 9 = 9
keyPad D x = x + 3
keyPad L 1 = 1
keyPad L 4 = 4
keyPad L 7 = 7
keyPad L x = x - 1
keyPad R 3 = 3
keyPad R 6 = 6
keyPad R 9 = 9
keyPad R x = x + 1


{-  1
  2 3 4
5 6 7 8 9
  A B C
    D
-}

keyPad' :: Instruction -> Int -> Int
keyPad' L v
  | v `elem` [1, 2, 5, 10, 13] = v
  | otherwise = v - 1
keyPad' R v
  | v `elem` [1, 4, 9, 12, 13] = v
  | otherwise = v + 1
keyPad' D 1 = 3

keyPad' D 2 = 6
keyPad' D 3 = 7
keyPad' D 4 = 8

keyPad' D 5 = 5
keyPad' D 6 = 10
keyPad' D 7 = 11
keyPad' D 8 = 12
keyPad' D 9 = 9

keyPad' D 10 = 10
keyPad' D 11 = 13
keyPad' D 12 = 12

keyPad' D 13 = 13
--
keyPad' U 1 = 1

keyPad' U 2 = 2
keyPad' U 3 = 1
keyPad' U 4 = 4

keyPad' U 5 = 5
keyPad' U 6 = 2
keyPad' U 7 = 3
keyPad' U 8 = 4
keyPad' U 9 = 9

keyPad' U 10 = 6
keyPad' U 11 = 7
keyPad' U 12 = 8

keyPad' U 13 = 11

-- utils

getValue f init xs = foldl (flip f) init xs

genericDay keypadF code = tail (scanl (getValue keypadF) 5 code)

-- FIRST problem
day code = genericDay keyPad code
-- SECOND problem

day' code = genericDay keyPad' code

-- tests and data

testData = [[U, L],
            [R, R, D, D, D],
            [L, U, R, D, L],
            [U, U, U, U, D]
            ]

test = hspec $ do
  describe "firstProblem" $ do
    it "works" $ do
      day testData `shouldBe` [1, 9, 8, 5]
      --day 1 `shouldBe` (2 :: Int)
  describe "secondProblem" $ do
    it "works" $ do
      day' testData `shouldBe` [5, 13, 11, 3]

  describe "finally" $ do
    it "works" $ do
      fmap (fmap day) content `shouldReturn` (Right [4, 7, 9, 7, 8])
      fmap (fmap day') content `shouldReturn` (Right [6, 5, 9, 10, 13])

fileContent = readFile "content/day2"
content = parse <$> fileContent
