{-# LANGUAGE ScopedTypeVariables #-}
module Utils where

import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P


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

count x l = length (filter (==x) l)


-- | Wrapper around parse, to avoid the Right unpacking which is painful
-- in a competitive context
parse :: P.Parser t -> String -> t
parse parser s = case P.parse parser "" s of
  Right r -> r
  Left err -> error (show err)
