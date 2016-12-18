{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
module Day17 where

import Test.Hspec

import Data.List

import Utils
import Data.Ord
import Data.Maybe

-- Input DSL
data Step = U | D | L | R deriving (Show)

displaySteps = concatMap show

hash key steps = take 4 . md5s $ key ++ displaySteps steps

openDoors key steps = let h = hash key steps
                          l = if isOpen h 0 then [U] else []
                          l' = if isOpen h 1 then D:l else l
                          l'' = if isOpen h 2 then L:l' else l'
                          l''' = if isOpen h 3 then R:l'' else l''
                      in l'''
  where isOpen h idx = (h !! idx) `elem` "bcdef"

filterMove pos move = x' >= 0 && x' < 4 && y' >= 0 && y' < 4
  where (x', y') = newPos pos move

dmove U = (0, -1)
dmove D = (0, 1)
dmove R = (1, 0)
dmove L = (-1, 0)

newPos (x, y) move =  (x + dx, y + dy)
  where (dx, dy) = dmove move

-- Problem DSL
search mf key = go (0, 0) []
  where go pos steps
          | pos == (3, 3) = Just steps
          | otherwise = let possibleMoves = filter (filterMove (id pos)) (openDoors key steps)
                            res = catMaybes (map (\move -> go (newPos pos move) (steps ++ [move])) possibleMoves)
                        in case res of
                             [] -> Nothing
                             l -> Just $ mf (comparing length) l

-- FIRST problem
day code = displaySteps <$> (search minimumBy code)

-- SECOND problem
day' code = length <$> (search maximumBy code)

-- tests and data

-- comment out and add tests
test = hspec $ it "works" $ do
  day input `shouldBe` (Just "RDDRULDDRR")
  day' input `shouldBe` (Just 766)

input = "ioramepc"

-- both parts in 32 minutes
