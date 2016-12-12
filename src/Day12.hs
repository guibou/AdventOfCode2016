{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
module Day12 where

import Test.Hspec

import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P

import Utils
import Data.Functor (($>))

import qualified Data.Map as Map
import Data.Map (Map)

-- Parsing
parser :: P.Parser [Asm]
parser = instruction `P.sepBy` P.string "\n"

instruction = P.choice [copy, dec, inc, jump]

copy = (P.string "cpy" $> Copy) <*> parseRegisterOrInt <*> parseRegister
inc = (P.string "inc" $> Inc) <*> parseRegister
dec = (P.string "dec" $> Dec) <*> parseRegister
jump = (P.string "jnz" $> Jump) <*> parseRegisterOrInt <*> parseInt

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
data Asm = Copy RegisterOrInt Register | Inc Register | Dec Register | Jump RegisterOrInt Int deriving (Show)
data RegisterOrInt = RegisterRI Register | IntRI Int deriving (Show)

-- Problem DSL
eval :: [Asm] -> Map Register Int -> Map Register Int
eval l m = go m 0
  where
        go m offset
          | offset < length l = case (l !! offset) of
              Inc r -> go (increment r m) (offset + 1)
              Dec r -> go (decrement r m) (offset + 1)
              Copy a b -> go (cp a b m) (offset + 1)
              Jump v doffset -> if (getROI v m) /= 0
                                then go m (offset + doffset)
                                else go m (offset + 1)
          | otherwise = m

-- utils
increment r m = Map.insert r (getRegister r m + 1) m

decrement r m = Map.insert r (getRegister r m - 1) m

cp v r m = Map.insert r (getROI v m) m

getROI v m = case v of
  RegisterRI r -> getRegister r m
  IntRI i -> i

getRegister r m = Map.findWithDefault 0 r m

getA m = m Map.! (Register 'a')

-- FIRST problem
day code = getA (eval code Map.empty)

-- SECOND problem
day' code = getA (eval code (Map.singleton (Register 'c') 1))

-- tests and data

-- comment out and add tests
test = hspec $ it "works" $ do
  day (parse parser exampleCode) `shouldBe` 42
  day <$> content `shouldReturn` 318007
  day' <$> content `shouldReturn` 9227661

fileContent = readFile "content/day12"
content = parse parser <$> fileContent

exampleCode = "\
\cpy 41 a\n\
\inc a\n\
\inc a\n\
\dec a\n\
\jnz a 2\n\
\dec a"
