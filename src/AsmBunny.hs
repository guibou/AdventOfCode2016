{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}

module AsmBunny where

-- Shared items between days 12, 23 and 25

import Data.Functor (($>))

import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P

import qualified Data.Map as Map
import Data.Map (Map)

-- AST
data RegisterOrInt = RegisterRI Register | IntRI Int deriving (Show)
data Register = Register Char deriving (Show, Ord, Eq)

data Asm = Copy RegisterOrInt Register
         | Inc Register
         | Dec Register
         | Jump RegisterOrInt RegisterOrInt
         -- Day 23
         | Toggle RegisterOrInt
         | Skip
         | Add Register Register
         | AddMul Register Register Register
         -- Day 25
         | Out RegisterOrInt
         deriving (Show)

-- PARSING
copy, inc, dec, jump, toggle, out :: P.Parser Asm

-- All Days
copy = (P.string "cpy" $> Copy) <*> parseRegisterOrInt <*> parseRegister
inc = (P.string "inc" $> Inc) <*> parseRegister
dec = (P.string "dec" $> Dec) <*> parseRegister
jump = (P.string "jnz" $> Jump) <*> parseRegisterOrInt <*> parseRegisterOrInt

-- Day 23
toggle = (P.string "tgl" $> Toggle) <*> parseRegisterOrInt

-- Day 25
out = (P.string "out" $> Out) <*> parseRegisterOrInt

-- All Days
parseRegisterOrInt :: P.Parser RegisterOrInt
parseRegisterOrInt = P.choice [P.try (RegisterRI <$> parseRegister),
                               IntRI <$> parseInt]

parseInt :: P.Parser Int
parseInt = do
  _ <- P.string " "
  minus <- P.optional (P.string "-")

  v <- read <$> P.many (P.oneOf "0123456789")

  return $ case minus of
    Just _ -> -v
    Nothing -> v

parseRegister :: P.Parser Register
parseRegister = P.string " " *> (Register <$> P.oneOf ['a' .. 'z'])


-- UTILS
increment r m = Map.insert r (getRegister r m + 1) m

decrement r m = Map.insert r (getRegister r m - 1) m

cp v r m = Map.insert r (getROI v m) m

getROI v m = case v of
  RegisterRI r -> getRegister r m
  IntRI i -> i

getRegister r m = Map.findWithDefault 0 r m

get c m = m Map.! (Register c)
