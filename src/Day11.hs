{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass, StandaloneDeriving, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
module Day11 where

import Test.Hspec

import Control.Monad (guard)
import Data.List

import Debug.Trace

import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.ByteString.Char8 (ByteString, split)
import qualified Data.ByteString.Char8 as BS

import Utils

-- Parsing
parser s = let l = BS.lines s
           in fmap parseLine l

parseLine l = let w = BS.words l
              in parseWords w

parseWords (x:w:xs)
  | "generator" `BS.isPrefixOf` w = (Generator (x)) : parseWords xs
  | "microchip" `BS.isPrefixOf` w = (Chip ((head (split '-' x)))) : parseWords xs
  | otherwise = parseWords (w:xs)
parseWords (_:xs) = parseWords xs
parseWords [] = []

-- Input DSL
data Item = Generator ByteString | Chip ByteString deriving (Show, Eq, Ord)
data Status = Status Int [[Item]] deriving (Show, Eq, Ord)

-- Problem DSL
possibleMoves floor = do
  x <- floor

  -- can be anything not already selected. The ordering relationship ensure that
  -- we don't select swapped pairs
  maybeY <- Nothing : fmap Just (filter (\v -> v /= x && v > x) floor)

  let move = case maybeY of
        Nothing -> [x]
        Just y -> [x, y]

  return move

possibleMoves2 (Status floorNb floors) = do
  let currentFloor = floors !! floorNb
      movesThisFloor = possibleMoves currentFloor

  move <- movesThisFloor

  let newCurrentFloor = filter (\x -> not (x `elem` move)) currentFloor

  guard $ checkFloor newCurrentFloor

  nextFloorNb <- [floorNb - 1, floorNb + 1]

  guard $ nextFloorNb >= 0 && nextFloorNb < nbFloors

  let newNextFloor = (floors !! nextFloorNb) ++ move

  guard $ checkFloor newNextFloor

  return . reduceProblem $ Status nextFloorNb (replace floorNb newCurrentFloor (replace nextFloorNb newNextFloor floors))

-- Uglyly written ;)
-- It transform a status to a conanical one shared amongs many others
reduceProblem (Status n l) = let allnames = getGenS (mconcat l)
                                 allnames2 = sortOn bli allnames

                                 bli s = (blu Map.! (Generator s),  blu Map.! (Chip s), s)

                                 blu = Map.fromList (mconcat (fmap (\(item, idx) -> fmap (,idx) item) (zip l [0..])))

                                 sortedNames = sort allnames

                                 replaces = Map.fromList (zip allnames2 sortedNames)

                                 rep aList = sort (fmap rep' aList)

                                 rep' (Generator x) = Generator (replaces Map.! x)
                                 rep' (Chip x) = Chip (replaces Map.! x)
                             in Status n (fmap rep l)

-- utils
replace :: Int -> t -> [t] -> [t]
replace 0 newItem (_:xs) = newItem : xs
replace n newItem (x:xs) = x : replace (n - 1) newItem xs

start = Status 0
nbFloors = 4

checkFloor floor = not (isRadioactive floor) || allChipProtected floor

isRadioactive = any isGenerator

allChipProtected floor = let chipS = getChipS floor
                             genS = getGenS floor
                         in all (`elem` genS) chipS

getChipS = fmap (\(Chip s) -> s) . filter (not . isGenerator)
getGenS = fmap (\(Generator s) -> s). filter (isGenerator)

isGenerator (Generator _) = True
isGenerator (Chip _) = False

bfs' floors = let (_, _, d) = bfs stopCriterion (Status 0 floors) step in d
  where stopCriterion todos done depth = case traceShow (depth, length todos, length done) $ find gameDone todos of
          Just _ -> True
          Nothing -> False
        step = possibleMoves2


gameDone (Status _ [[], [], [], _]) = True
gameDone _ = False

day code = bfs' code

-- SECOND problem
day' [a, b, c, d] = bfs' [a', b, c, d]
  where a' = sort (a ++ [Generator el, Chip el, Generator dil, Chip dil])
          where el = "elerium"
                dil = "dilithium"

-- tests and data

-- comment out and add tests
test = hspec $ do
  it "isRadioactive" $ do
    isRadioactive [Chip "hello", Chip "you"] `shouldBe` False
    isRadioactive [Chip "hello", Generator "you"] `shouldBe` True

  it "allChipProtected" $ do
    allChipProtected [Chip "hello"] `shouldBe` False
    allChipProtected [Chip "hello", Generator "you"] `shouldBe` False
    allChipProtected [Chip "you", Generator "you", Generator "blork"] `shouldBe` True
    allChipProtected [Chip "you", Generator "you"] `shouldBe` True
    allChipProtected [Chip "a", Chip "you", Generator "you"] `shouldBe` False

  it "checkFloor" $ do
    checkFloor [Chip "hello"] `shouldBe` True
    checkFloor [Chip "hello", Generator "you"] `shouldBe` False
    checkFloor [Chip "you", Generator "you", Generator "blork"] `shouldBe` True
    checkFloor [Chip "you", Generator "you"] `shouldBe` True
    checkFloor [Chip "a", Chip "you", Generator "you"] `shouldBe` False

  it "worksForAll" $ do
    day <$> content `shouldReturn` 37
    day' <$> content `shouldReturn` 61

fileContent = BS.readFile "content/day11"
content = parser <$> fileContent

exampleProblem = parser "\
\The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.\n\
\The second floor contains a hydrogen generator.\n\
\The third floor contains a lithium generator.\n\
\The fourth floor contains nothing relevant."
