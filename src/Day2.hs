module Day2 where

import Test.Hspec

import Utils

import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P

-- Input DSL
data Instruction = U | D | L | R deriving (Show)

-- Parsing
parser :: P.Parser [[Instruction]]
parser = P.sepBy (P.many (parserInstruction)) (P.string "\n")

parserInstruction :: P.Parser Instruction
parserInstruction = (U <$ P.string "U") P.<|>
                    (D <$ P.string "D") P.<|>
                    (L <$ P.string "L") P.<|>
                    (R <$ P.string "R")

-- Problem DSL

data KeyPad = KeyPad [[Char]] deriving (Show)

data Status = Status KeyPad (Int, Int) deriving (Show)

makeKeyPad s coord = Status (KeyPad (lines s)) coord

validCase k@(KeyPad s) (x, y) = y >= 0 && y < length s && x >= 0 && x < length (s !! y) && getKeyPad k (x, y) /= ' '

getKeyPad keyPad@(KeyPad s) (x, y) = s !! y !! x
getStatus (Status keyPad coord) = getKeyPad keyPad coord

moveKeyPad i (Status keyPad (x, y)) = Status keyPad (if validCase keyPad newCoord then newCoord else (x, y))
  where newCoord = case i of
          U -> (x, y - 1)
          D -> (x, y + 1)
          L -> (x - 1, y)
          R -> (x + 1, y)

keyPad = makeKeyPad "123\n\
                    \456\n\
                    \789" (1, 1)

keyPad' = makeKeyPad "  1  \n\
                     \ 234 \n\
                     \56789\n\
                     \ ABC \n\
                     \  D   " (0, 2)

-- utils

foldInstruction :: Status -> [Instruction] -> Status
foldInstruction keyPad xs = foldl (flip moveKeyPad) keyPad xs

genericDay :: Status -> [[Instruction]] -> [Char]
genericDay keypad code = map getStatus (tail (scanl foldInstruction keypad code))

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
      day testData `shouldBe` "1985"
      --day 1 `shouldBe` (2 :: Int)
  describe "secondProblem" $ do
    it "works" $ do
      day' testData `shouldBe` "5DB3"

  describe "finally" $ do
    it "works" $ do
      day <$> content `shouldReturn` "47978"
      day' <$> content `shouldReturn` "659AD"

fileContent = readFile "content/day2"
content = parse parser <$> fileContent
