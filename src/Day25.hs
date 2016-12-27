{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Day25 where

import Test.Hspec

import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P

import Utils
import Data.Functor (($>))

import qualified Data.Map.Strict as Map
import Data.Map (Map)

import Data.List (find)

-- Parsing
parser :: P.Parser [Asm]
parser = instruction `P.sepBy` P.string "\n"

instruction = P.choice [copy, dec, inc, jump, out]

copy = (P.string "cpy" $> Copy) <*> parseRegisterOrInt <*> parseRegister
inc = (P.string "inc" $> Inc) <*> parseRegister
dec = (P.string "dec" $> Dec) <*> parseRegister
jump = (P.string "jnz" $> Jump) <*> parseRegisterOrInt <*> parseRegisterOrInt
out = (P.string "out" $> Out) <*> parseRegisterOrInt

parseRegisterOrInt = P.choice [P.try (RegisterRI <$> parseRegister),
                               IntRI <$> parseInt]

parseInt = do
  _ <- P.string " "
  minus <- P.optional (P.string "-")

  v <- read <$> P.many (P.oneOf "0123456789")

  return $ case minus of
    Just _ -> -v
    Nothing -> v

parseRegister = P.string " " *> (Register <$> P.oneOf ['a' .. 'z'])


-- Input DSL
data Register = Register Char deriving (Show, Ord, Eq)
data Asm = Copy RegisterOrInt Register | Inc Register | Dec Register | Jump RegisterOrInt RegisterOrInt | Out RegisterOrInt deriving (Show)
data RegisterOrInt = RegisterRI Register | IntRI Int deriving (Show)

-- Input DSL
eval :: [Asm] -> Map Register Int -> [Int]
eval l  m = go m 0
  where
        go :: Map Register Int -> Int -> [Int]
        go m offset
          | offset < length l = case (l !! offset) of
              Inc r -> go (increment r m) (offset + 1)
              Dec r -> go (decrement r m) (offset + 1)
              Copy a b -> go (cp a b m) (offset + 1)
              Jump v doffset -> if (getROI v m) /= 0
                                then go m (offset + getROI doffset m)
                                else go m (offset + 1)
              Out roi -> getROI roi m : go m (offset + 1)
          | otherwise = []

add ra rb m = Map.insert ra (getRegister ra m + getRegister rb m) m
addmul ra rb rc m = Map.insert ra (getRegister rb m * getRegister rc m) m

increment r m = Map.insert r (getRegister r m + 1) m

decrement r m = Map.insert r (getRegister r m - 1) m

cp v r m = Map.insert r (getROI v m) m

getROI v m = case v of
  RegisterRI r -> getRegister r m
  IntRI i -> i

getRegister r m = Map.findWithDefault 0 r m

getA m = m Map.! (Register 'a')

-- Problem DSL


-- utils
blork n content = eval content (Map.singleton (Register 'a') n)

-- FIRST problem
day content = find (\x -> take 100 (blork x content) == take 100 (cycle [0, 1])) [0..]

test = hspec $ it "works" $ do
  day <$> content `shouldReturn` (Just 175)

fileContent = readFile "content/day25"
content = parse parser <$> fileContent

simpleExample = parse parser "\
\cpy 2 a\n\
\tgl a\n\
\tgl a\n\
\tgl a\n\
\cpy 1 a\n\
\dec a\n\
\dec a"

-- begin: 15:09
-- start1 : 15h19
