{-# LANGUAGE ScopedTypeVariables #-}
module Utils where

import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P

import qualified Data.Set as Set
import Data.Set (Set)

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 (encode)

import Control.Parallel.Strategies (parBuffer, using, rdeepseq)

import Data.List.Split (chunksOf)

import qualified Data.ByteString.Char8 as BS

-- So I can use it in the shell
-- dayX <$$> content

(<$$>) f x = (fmap . fmap) f x

infixl 4 <$$>

-- Torus enum

-- |
-- >>> data Test = A | B | C | D deriving (Bounded, Enum, Show)
-- >>> succWrap A
-- B
-- >>> succWrap D
-- A
-- >>> predWrap D
-- C
-- >>> predWrap A
-- D
succWrap :: forall t. (Enum t, Bounded t) => t -> t
succWrap = nWrap 1

predWrap :: forall t. (Enum t, Bounded t) => t -> t
predWrap = nWrap (-1)

nWrap :: forall t. (Enum t, Bounded t) => Int -> t -> t
nWrap d e = let idx = fromEnum e
                m = (fromEnum (maxBound :: t)) + 1
            in toEnum ((idx + d) `mod` m)

count x l = countIf (==x) l

countIf p l = length (filter p l)


-- | Wrapper around parse, to avoid the Right unpacking which is painful
-- in a competitive context
parse :: P.Parser t -> String -> t
parse parser s = case P.parse parser "" s of
  Right r -> r
  Left err -> error (show err)

bfs :: Ord p => (Set p -> Set p -> Int -> Bool) -> p -> (p -> [p]) -> (Set p, Set p, Int)
bfs stopCriterion start stepFunction = go (Set.singleton start) (Set.empty) 0
  where go todos visited depth
          | stopCriterion todos visited depth = (todos, visited, depth)
          | otherwise = let newSteps = Set.fromList (mconcat (map stepFunction (Set.toList todos)))
                            okSteps = Set.difference newSteps visited

                        in go okSteps (Set.union todos visited) (depth + 1)

md5 = encode . hash
md5s = BS.unpack . encode . hash . BS.pack

parBufferChunks l = let chunks = (chunksOf 4096 l)
                    in mconcat chunks `using` parBuffer 20 rdeepseq

--

(!?) :: [a] -> Int -> Maybe a
l !? idx
  | idx < length l = Just (l !! idx)
  | otherwise = Nothing
