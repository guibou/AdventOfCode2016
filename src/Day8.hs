module Day8 where

import Test.Hspec

import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P

import Utils
import Data.Functor (($>))

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
data Matrix = Matrix [[Bool]]

nRows = 6
nCols = 50

disp (Matrix m) = unlines (map (map f) m)
  where f True = '#'
        f False = '.'

instance Show Matrix where
  show = disp

newMatrix = Matrix (replicate nRows (replicate nCols False))

genMatrix f = Matrix l'
  where l' = map (\row -> map (\col -> f (row, col)) [0 .. (nCols - 1)]) [0..(nRows - 1)]

getItem (Matrix m) (row, col) = ((m !! row) !! col)

copyMatrix m = genMatrix f
  where f = getItem m

-- real functions

rectMatrix x y m = genMatrix f
  where f (row, col)
          | row < y && col < x = True
          | otherwise = getItem m (row, col)

rotateColumn col' n m = genMatrix f
  where f (row, col)
          | col' == col = getItem m ((row - n) `mod` nRows, col)
          | otherwise = getItem m (row, col)

rotateRow row' n m = genMatrix f
  where f (row, col)
          | row' == row = getItem m (row, (col - n) `mod` nCols)
          | otherwise = getItem m (row, col)

-- utils
apply :: Command -> Matrix -> Matrix
apply (Rect x y) = rectMatrix x y
apply (RotateRow x y) = rotateRow x y
apply (RotateCol x y) = rotateColumn x y

applyS :: [Command] -> Matrix
applyS cmds = foldl (flip apply) newMatrix cmds

-- FIRST problem
day code = let (Matrix m) = applyS code
           in count True (mconcat m)

-- SECOND problem
day' code = code * 2

-- tests and data

-- comment out and add tests
test = hspec $ it "works" $ do
  day <$> content `shouldReturn` 110

testLine = [Rect 3 2, RotateCol 1 1, RotateRow 0 4, RotateCol 1 1]

fileContent = readFile "content/day8"
content = parse parser <$> fileContent
