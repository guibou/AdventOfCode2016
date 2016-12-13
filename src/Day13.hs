{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
module Day13 where

import Test.Hspec

import Data.Bits

import Control.Monad (guard)
import qualified Data.Set as Set

import Utils

-- Input DSL


-- utils
isWall :: Int -> (Int, Int) -> Bool
isWall key (x, y) = let res = x * x + 3 * x + 2 * x * y + y + y * y
                        res' = res + key
                        c = popCount res'
                    in odd c

type Pos = (Int, Int)

drawWall key = unlines $ do
  y <- [0 .. 6]
  return $ do
    x <- [0 .. 9]

    return $ if isWall key (x, y)
             then '#'
             else '.'

stepFunction :: Int -> Pos -> [Pos]
stepFunction key (x, y) = filter (not . isWall key) $ do
              (dx, dy) <- [(-1, 0), (1, 0), (0, 1), (0, -1)]

              let x' = x + dx
                  y' = y + dy

              guard $ x' >= 0 && y' >= 0
              guard $ x' /= x || y' /= y
              return (x', y')

-- FIRST problem
day key = let (_, _, d) = bfs sc (1, 1) steps
          in d
  where sc todos _ _ = (31, 39) `Set.member` todos
        steps = stepFunction key

-- SECOND problem
day' key = let (a, b, _) = bfs sc (1, 1) steps
           in length a + length b
  where sc _ _ depth = depth == 50
        steps = stepFunction key

-- tests and data

content = 1352 :: Int

-- 8h35
-- 9h
-- Yeah, virtual leaderboard ;)

-- comment out and add tests
test = hspec $ it "works" $ do
  isWall 10 (3, 5) `shouldBe` True
  isWall 10 (6, 5) `shouldBe` False

  drawWall 10 `shouldBe` "\
\.#.####.##\n\
\..#..#...#\n\
\#....##...\n\
\###.#.###.\n\
\.##..#..#.\n\
\..##....#.\n\
\#...##.###\n"

  day content `shouldBe` 90
  day' content `shouldBe` 135
