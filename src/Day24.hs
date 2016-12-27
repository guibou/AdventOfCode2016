{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
module Day24 where

import Test.Hspec

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import Control.Monad (guard)
import Data.List
import Data.Ord

import Data.Function.Memoize

import Debug.Trace

import Utils

-- Parsing
parser s = let l = lines s
           in map (map toItem) l
  where toItem '#' = Wall
        toItem '.' = Free
        toItem x = Number (read [x])

type Coord = (Int, Int)

isNumber (Number _) = True
isNumber _ = False

isWall Wall = True
isWall _ = False

makeGrid :: [[Item]] -> Map Coord Item
makeGrid items = Map.fromList $ do
  (l, v) <- zip [0..] items
  (c, v') <- zip [0..] v

  return ((l, c), v')

findNumbers m = let l = Map.toList m
                in filter (\(_, item) -> isNumber item) l

findCoordinate m n = let Just (c, _) = find (\(c, item) -> item == Number n) (Map.toList m)
                     in  c

-- Input DSL
data Item = Wall | Free | Number Int deriving (Show, Eq)

-- Problem DSL


-- utils
shortestPathBetweenTwo m a b = let (_, _, n) = bfs stopCriterion start stepFunction
                               in n
  where start = findCoordinate m a
        stop = findCoordinate m b
        stopCriterion todos _ _ = stop `Set.member` todos
        stepFunction (x, y) = do
          (x', y') <- [
            (x - 1, y),
            (x + 1, y),
            (x, y - 1),
            (x, y + 1)]

          guard $ not (isWall (m Map.! (x', y')))
          return (x', y')

-- FIRST problem
findShortestPaths backToZero m = let numbers = map (\(_, Number n) -> n) $ findNumbers m
                                     perms' = getPerm numbers
                                     perms = if not backToZero
                                             then perms'
                                             else map (++[0]) perms'

                                     getShortestPath = shortestPathBetweenTwo m
                                     getShortestPath' = memoize2 getShortestPath

                                     pathLengths = map (\x -> (x, traceShowId $ pathLen getShortestPath' x)) perms
                                 in minimumBy (comparing snd) pathLengths

getPerm :: [Int] -> [[Int]]
getPerm numbers = map (0:) $ permutations (filter (/=0) numbers)

pathLen get (a:b:xs) = pathLen get (b:xs) + get a b
pathLen _ _ = 0

day = snd . findShortestPaths False . makeGrid

-- SECOND problem
day' = snd . findShortestPaths True . makeGrid

-- tests and data

-- comment out and add tests
test = hspec $ it "works" $ do
  day testContent `shouldBe` 14
  day <$> content `shouldReturn` 464
  day' <$> content `shouldReturn` 652

fileContent = readFile "content/day24"
content = parser <$> fileContent

-- begin: 14h19
-- 14h57, a solution for the example code, but too low for the correct result ?? 
-- 15h00, I'm stupid ;) First star
-- 15h01, star 2
testContent = parser "\
\###########\n\
\#0.1.....2#\n\
\#.#######.#\n\
\#4.......3#\n\
\###########"
