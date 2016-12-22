{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
module Day21 where

import Test.Hspec

import Data.List.Split
import Data.List

-- Parsing
parser s = map parseLine (lines s)

parseLine l = let (cmd:xs) = splitOn " " l
                  readN i = read (xs !! i)
                  letterN i = head (xs !! i)
              in case cmd of
                    "rotate" -> case xs !! 0 of
                      "right" -> RotateRight (readN 1)
                      "left" -> RotateLeft (readN 1)
                      "based" -> RotateBased (letterN 5)
                    "move" -> Move (readN 1) (readN 4)
                    "swap" -> case xs !! 0 of
                      "letter" -> SwapC (letterN 1) (letterN 4)
                      "position" -> SwapP (readN 1) (readN 4)
                    "reverse" -> Reverse (readN 1) (readN 3)

-- Input DSL
data Op = SwapP Int Int | SwapC Char Char | RotateLeft Int | RotateRight Int | RotateBased Char | Reverse Int Int | Move Int Int deriving (Show)


-- Problem DSL


-- utils
applyRev :: Op -> String -> String
applyRev op s = case op of
  (SwapC a b) -> map (swapC b a) s
  (SwapP pa pb) -> swapP pb pa s
  (Reverse pa pb) -> rev pa pb s
  (RotateRight o) -> rotateL o s
  (RotateLeft o) -> rotateR o s
  (RotateBased c) -> rotateBased' c s
  (Move pa pb) -> move' pa pb s

applyOp :: Op -> String -> String
applyOp op s = case op of
  (SwapC a b) -> map (swapC a b) s
  (SwapP pa pb) -> swapP pa pb s
  (Reverse pa pb) -> rev pa pb s
  (RotateRight o) -> rotateR o s
  (RotateLeft o) -> rotateL o s
  (RotateBased c) -> rotateBased c s
  (Move pa pb) -> move pa pb s

move pa pb s = let c = s !! pa
                   start = take pa s
                   skip = drop (pa + 1) s

                   newS = start ++ skip
               in insertAt pb c newS

insertAt pb c s = let before = take pb s
                      after = drop pb s
                  in before ++ [c] ++ after

rotateBased c s = let Just idx = elemIndex c s
                      rIdx = idx + 1 + (if idx >= 4 then 1 else 0)
                  in rotateR rIdx s

rotateBased' c s = let (Just res) = find (\x -> rotateBased c x == s) (iterate (rotateL 1) s)
                   in res

move' a b s = let (Just res) = find (\x -> move a b x == s) $ do
                    a <- [0 .. length s - 1]
                    b <- [0 .. length s - 1]
                    return $ move a b s
              in res

swapC a b x
  | x == a = b
  | x == b = a
  | otherwise = x

swapP pa pb s = let s' = change pa (s !! pb) (zip [0..] s)
                    s'' = change pb (s !! pa) s'
                in map snd s''

change :: Int -> Char -> [(Int, Char)] -> [(Int, Char)]
change idxChange c ((idxTest, c'):xs)
  | idxTest == idxChange = (idxTest, c):xs
  | otherwise = (idxTest, c'):change idxChange c xs
change _ _ [] = error "WTF"

rev pa pb s = let start = take pa s
                  end = drop (pb + 1) s
                  middle = take (pb - pa + 1) (drop pa s)
              in start ++ reverse middle ++ end

rotateL 0 s = s
rotateL n s = rotateL (n - 1) (tail s ++ [head s])

rotateR 0 s = s
rotateR n s = rotateR (n - 1) (last s : init s)

-- FIRST problem
day start ops = foldl (flip applyOp) start ops

-- SECOND problem
day' start ops = foldl (flip applyRev) start (reverse ops)

day'2 start ops = head (filter (\x -> day x ops == start) (permutations start))
-- tests and data

-- comment out and add tests
test = hspec $ it "works" $ do
  day "abcde" (parser simpleEx) `shouldBe` "decab"
  day' "decab" (parser simpleEx) `shouldBe` "abcde"

  day "abcdefgh" <$> content `shouldReturn` "dbfgaehc"
  day' "fbgdceah" <$> content `shouldReturn` "aghfcdeb"

  day'2 "fbgdceah" <$> content `shouldReturn` "aghfcdeb"

fileContent = readFile "content/day21"
content = parser <$> fileContent

-- 10h40
-- 11h26 Haskell lists are not nice for string manipulation ;)
-- 11h40
simpleEx = "\
\swap position 4 with position 0\n\
\swap letter d with letter b\n\
\reverse positions 0 through 4\n\
\rotate left 1 step\n\
\move position 1 to position 4\n\
\move position 3 to position 0\n\
\rotate based on position of letter b\n\
\rotate based on position of letter d"
