{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
module Day13 where

import Test.Hspec

import Data.Bits

import Control.Monad (guard)
import qualified Data.Set as Set

-- Input DSL


-- Problem DSL


-- utils
isWall :: (Int, Int) -> Int -> Bool
isWall (x, y) input = let res = x * x + 3 * x + 2 * x * y + y + y * y
                          res' = res + input
                          c = popCount res'
                      in odd c

type Pos = (Int, Int)

pathFinder :: Pos -> Pos -> Int -> Int
pathFinder target start key = go (Set.singleton start) (Set.empty) 0
  where go todos visited depth
          | target `Set.member` todos = depth
          | otherwise = let newSteps = Set.fromList (filter (not . isWall') (mconcat (map step (Set.toList todos))))
                            okSteps = Set.difference newSteps visited

                        in go okSteps (Set.union todos visited) (depth + 1)
        isWall' x = isWall x key
        step (x, y) = do
              (dx, dy) <- [(-1, 0), (1, 0), (0, 1), (0, -1)]

              let x' = x + dx
                  y' = y + dy

              guard $ x' >= 0 && y' >= 0
              guard $ x' /= x || y' /= y
              return (x', y')

pathFinder' :: Int -> Pos -> Int -> Int
pathFinder' maxDepth start key = go (Set.singleton start) (Set.empty) 0
  where go todos visited depth
          | depth == maxDepth = length visited + length todos
          | otherwise = let newSteps = Set.fromList (filter (not . isWall') (mconcat (map step (Set.toList todos))))
                            okSteps = Set.difference newSteps visited

                        in go okSteps (Set.union todos visited) (depth + 1)
        isWall' x = isWall x key
        step (x, y) = do
              (dx, dy) <- [(-1, 0), (1, 0), (0, 1), (0, -1)]

              let x' = x + dx
                  y' = y + dy

              guard $ x' >= 0 && y' >= 0
              guard $ x' /= x || y' /= y
              return (x', y')

drawWall key = unlines $ do
  y <- [0 .. 6]
  return $ do
    x <- [0 .. 9]

    return $ if isWall (x, y) key
             then '#'
             else '.'

-- FIRST problem
day key = pathFinder (31, 39) (1, 1) key

-- SECOND problem
day' key = pathFinder' 50 (1, 1) key

-- tests and data

content = 1352 :: Int

-- 8h35
-- 9h
-- Yeah, virtual leaderboard ;)

-- comment out and add tests
test = hspec $ it "works" $ do
  isWall (3, 5) 10 `shouldBe` True
  isWall (6, 5) 10 `shouldBe` False

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
