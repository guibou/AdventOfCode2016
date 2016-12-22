{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
module Day22 where

import Test.Hspec

import Data.List.Split

import Data.List
import Data.Ord


import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad

import Utils (bfs)

-- Parsing
parser s = map parseLine (drop 2 (lines s))

parseLine l = let [a, b, c, _, _] = words l
                  size = read (init b)
                  used = read (init c)
                  [_, x, y] = splitOn "-" a

                  x' = read (tail x)
                  y' = read (tail y)
              in Node (x', y') size used

-- Input DSL
data Node = Node {
  getCoord :: (Int, Int),
  getSize :: Int,
  getUsed :: Int
  } deriving (Show, Eq, Ord)

getAvail n = getSize n - getUsed n

-- Problem DSL
pairs nodes = do
  n0 <- nodes
  n1 <- nodes

  return (n0, n1)

isViable (nA, nB) = notEmpty nA && nA /= nB && getUsed nA <= getAvail nB

notEmpty n = getUsed n > 0


-- utils
toNodeDict nodes = Map.fromList (map (\x -> (getCoord x, x)) nodes)

type Coord = (Int, Int)
type Cache = Map.Map (Int, Int) Node

neightboor :: Coord -> [Coord]
neightboor (x, y) = do
  (dx, dy) <- [(-1, 0),
               (1, 0),
               (0, -1),
               (0, 1)]
  let x' = x + dx
      y' = y + dy

  return (x', y')

nodePossibleMoves :: Node -> Cache -> [(Coord, Coord)]
nodePossibleMoves node nodes = do
  newCoord <- neightboor (getCoord node)

  let newNode = nodes Map.! newCoord

  guard $ newCoord `Map.member` nodes && isViable (node, newNode)

  return (getCoord node, getCoord newNode)

allPossiblesMoves :: Cache -> [(Coord, Coord)]
allPossiblesMoves nodes = let nodes' = Map.elems nodes
                          in concatMap (\x -> nodePossibleMoves x nodes) nodes'


doMove :: Cache -> (Coord, Coord) -> Cache
doMove nodes (cFrom, cTo) = let nodeFrom = nodes Map.! cFrom
                                nodeTo = nodes Map.! cTo
                                emptyFrom = nodeFrom {getUsed = 0}
                                filledTo = nodeTo {getUsed = getUsed nodeTo + getUsed nodeFrom}

                            in if isOk filledTo
                               then Map.insert cTo filledTo (Map.insert cFrom emptyFrom nodes)
                               else error "WTF"

isOk n = getUsed n <= getSize n

-- FIRST problem
day = length . filter isViable . pairs

-- SECOND problem
getFinalNode nodes = maximumBy (comparing (fst . getCoord)) . filter (\x -> snd (getCoord x) == 0) $ nodes

type Status = (Coord, [(Coord, Coord)], Cache)

day' nodes = let startingStatus = (getCoord (getFinalNode nodes), allPossiblesMoves cache, cache)
                 cache = toNodeDict nodes
                 (_, _, n) = bfs stopCriterion startingStatus stepFunction
             in n

stopCriterion todos _ _ = any (\(gPos, _, _) -> gPos == (0, 0)) todos

stepFunction :: Status -> [Status]
stepFunction (gPos, moves, cache) = do
  move@(cFrom, cTo) <- moves

  let cache' = doMove cache move
      gPos' = if cFrom == gPos
              then cTo
              else gPos
      newPossiblesMoves = upPossibleMoves moves (cFrom, cTo) cache'

  return (gPos', newPossiblesMoves, cache')

nubOrd s = Set.toList $ Set.fromList s

upPossibleMoves :: [(Coord, Coord)] -> (Coord, Coord) -> Cache -> [(Coord, Coord)]
upPossibleMoves currentMoves (cFrom, cTo) cache = let
  interestingPoints = cFrom : cTo : concatMap (\(x, y) -> [x, y]) currentMoves
  ns = concatMap neightboor interestingPoints

  pts = nubOrd (interestingPoints ++ ns)
  pts' = filter (`Map.member`cache) pts

  -- add back all the move involving cleared items
  moves' = concatMap (\x -> nodePossibleMoves (cache Map.! x) cache) pts'

  in nubOrd moves'

-- comment out and add tests
test = hspec $ it "works" $ do
  day <$> content `shouldReturn` 1007
  day' simpleProblem `shouldBe` 7

fileContent = readFile "content/day22"
content = parser <$> fileContent

-- 11h53
-- 12h02 first star
-- 12h40 working solution with the small subset, too long with the big one...

simpleProblem = parser "\
\root@ebhq-gridcenter# df -h\n\
\Filesystem              Size  Used  Avail  Use%\n\
\/dev/grid/node-x0-y0   10T    8T     2T   80%\n\
\/dev/grid/node-x0-y1   11T    6T     5T   54%\n\
\/dev/grid/node-x0-y2   32T   28T     4T   87%\n\
\/dev/grid/node-x1-y0    9T    7T     2T   77%\n\
\/dev/grid/node-x1-y1    8T    0T     8T    0%\n\
\/dev/grid/node-x1-y2   11T    7T     4T   63%\n\
\/dev/grid/node-x2-y0   10T    6T     4T   60%\n\
\/dev/grid/node-x2-y1    9T    8T     1T   88%\n\
\/dev/grid/node-x2-y2    9T    6T     3T   66%"

printGrid :: Bool -> Cache -> String
printGrid blork cache = unlines $ do
  y <- [0 .. maxy]
  return $ unwords $ do
    x <- [0 .. maxx]
    return $ getDesc (x, y)

      where getDesc coord = let node = cache Map.! coord
                            in if blork
                               then show (getUsed node) ++ "/" ++ show (getSize node)
                               else if getUsed node > minSize
                                    then "#"
                                    else "."

            maxy = maximum $ map (snd . getCoord) (Map.elems cache)
            maxx = maximum $ map (fst . getCoord) (Map.elems cache)
            minSize = minimum $ map getSize (Map.elems cache)

-- wallPos = (9, 4)
-- first 0 : (22, 25)
-- move the 0 to the top, means moving to (8, 0), hence (22 - 8) + (25 - 0) == 39 moves
-- then go to column 36 -> 28 moves (this moves Data to column 35)
-- then, each steps to bring the Data back one takes 5 moves
-- hence 5 * 35
-- temps final = 35 * 5 + 28 + 39 == 242
