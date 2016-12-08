module Day8 where

import Test.Hspec

import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P

import Utils
import Data.Functor (($>))
import Data.Matrix

-- Parsing
parser = parseLine `P.sepBy` (P.string "\n") <* P.eof

parseLine = (P.try parseRect) P.<|> parseRotate

parseRect :: P.Parser Command
parseRect = do
  _ <- P.string "rect "
  x <- parseInt
  _ <- P.string "x"
  y <- parseInt

  return (Rect x y)

parseRotate :: P.Parser Command
parseRotate = do
  _ <- P.string "rotate "
  ctor <- (P.try (P.string "row y=" $> RotateRow)) P.<|> (P.string "column x=" $> RotateCol)
  dxy <- parseInt
  _ <- P.string " by "
  value <- parseInt

  return $ ctor dxy value

parseInt :: P.Parser Int
parseInt = read <$> P.many (P.oneOf ['0' .. '9'])

-- Input DSL

data Command = Rect Int Int
              | RotateRow Int Int
              | RotateCol Int Int deriving (Show)


-- Problem DSL
newtype Display = Display (Matrix Bool)

instance Show Display where
  show (Display m) = unlines (map (map f) (toLists m))
    where f False = '.'
          f True = '#'

nRows = 6
nCols = 50

build = Display . matrix nRows nCols
emptyMatrix = build (const False)

mod' x n = ((x - 1) `mod` n) + 1

-- real functions

rectMatrix x y (Display m) = build f
  where f (row, col)
          | row <= y && col <= x = True
          | otherwise = getElem row col m

rotateColumn col' n (Display m) = build f
  where f (row, col)
          | (col' + 1) == col = getElem ((row - n) `mod'` nRows) col m
          | otherwise = getElem row col m

rotateRow row' n (Display m) = build f
  where f (row, col)
          | (row' + 1) == row = getElem row ((col - n) `mod'` nCols) m
          | otherwise = getElem row col m

-- utils
apply (Rect x y) = rectMatrix x y
apply (RotateRow x y) = rotateRow x y
apply (RotateCol x y) = rotateColumn x y

applyS cmds = foldl (flip apply) emptyMatrix cmds

-- FIRST problem
day code = let (Display m) = applyS code
           in count True (toList m)

-- SECOND problem
day' code = code * 2

-- tests and data

-- comment out and add tests
test = hspec $ it "works" $ do
  day <$> content `shouldReturn` 110

testLine = [Rect 3 2, RotateCol 1 1, RotateRow 0 4, RotateCol 1 1]

fileContent = readFile "content/day8"
content = parse parser <$> fileContent
