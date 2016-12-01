{-# LANGUAGE PatternSynonyms #-}
module Day1 where

import Test.Hspec

import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P

-- Input DSL

data Instruction = Instruction Rotate Int deriving (Show)
data Rotate = RIGHT | LEFT | NO deriving (Show)

-- Parsing

parse s = P.parse parser "" s

parser = P.sepBy pInstruction (P.string ", ")

pInstruction :: P.Parser Instruction
pInstruction = do
  rotate <- (RIGHT <$ P.char 'R') P.<|> (LEFT <$ P.char 'L')
  count <- read <$> (P.many (P.oneOf "0123456789"))
  return (Instruction rotate count)

-- Problem DSL

data Heading = North | Weast | South | East deriving (Show, Bounded, Enum, Eq)
data Position = Position Heading (Int, Int) deriving (Show, Eq)

startingPoint = Position North (0, 0)

changeHeading LEFT h
  | h == minBound = maxBound
  | otherwise = pred h
changeHeading RIGHT h
  | h == maxBound = minBound
  | otherwise = succ h
changeHeading NO h = h

deltaMove North v = (v, 0)
deltaMove South v = (-v, 0)
deltaMove East v = (0, v)
deltaMove Weast v = (0, -v)

nextPosition (Position heading (x, y)) (Instruction direction step) = Position newHeading (x + dx, y + dy)
  where newHeading = changeHeading direction heading
        (dx, dy) = deltaMove newHeading step

-- utils

distance (x, y) = abs x + abs y

-- FIRST problem

finalPosition code = foldl nextPosition startingPoint code

day code = let Position _ finalCoord = finalPosition code
           in distance finalCoord

-- SECOND problem

findFirstDouble (x:xs)
  | x `elem` xs = x
  | otherwise = findFirstDouble xs

allPosition = scanl nextPosition startingPoint

transform' (Instruction d n) = (Instruction d 1) : replicate (n - 1) (Instruction NO 1)

transform = mconcat . map transform'

day' code = let
  poss = allPosition (transform code)
  in distance (findFirstDouble (map (\(Position _ p) -> p) poss))

-- tests and data

pattern R x = Instruction RIGHT x
pattern L x = Instruction LEFT x

test = hspec $ do
  describe "firstProblem" $ do
    it "works" $ do
      day [R 2, L 3] `shouldBe` 5
      day [R 2, R 2, R 2] `shouldBe` 2
      day [R 5, L 5, R 5, R 3] `shouldBe` 12
  describe "secondProblem" $ do
    it "works" $ do
      day' [R 8, R 4, R 4, R 8] `shouldBe` 4

fileContent = readFile "content/day1"
content = parse <$> fileContent

