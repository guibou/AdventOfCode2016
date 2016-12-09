module Day9 where

import Test.Hspec

spanSkip c l = let (a, b) = span (/=c) l
               in (a, tail b)

lengthContent [] _ = 0
lengthContent (c:xs) f = case c of
  '(' -> parseRep xs f
  ' ' -> parseRep xs f
  _ -> 1 + lengthContent xs f

parseRep l f = let (a, l') = spanSkip 'x' l
                   (c, l'') = spanSkip ')' l'
                   x0 = read a
                   x1 = read c
                   (cs, l''') = splitAt x0 l''
               in (x1 * (f cs)) + (lengthContent l''' f)

-- FIRST problem
day code = lengthContent code length

-- SECOND problem
day' code = lengthContent code day'

-- tests and data

-- comment out and add tests
test = hspec $ it "works" $ do
  day <$> content `shouldReturn` 110346
  day' <$> content `shouldReturn` 10774309173

fileContent = readFile "content/day9"

remSpaces = filter (/=' ')
content = remSpaces <$> fileContent
