module Day4 where

import Test.Hspec

import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P

import Data.List (sortBy, find, findIndex, intercalate)
import Data.Char (ord, chr)

import Utils
import Data.Monoid

-- Input DSL

data Room = Room {
  roomName :: String,
  roomId :: Int,
  roomChecksum :: String
  }

-- Parsing

parse s = P.parse (P.sepBy parser (P.char '\n')) "" s

parser = do
  name <- intercalate "-" <$> P.many (parseName <* P.char '-')
  sectorID <- parseSectorID

  P.char '['
  checksum <- parseName
  P.char ']'

  return $ Room name sectorID checksum

parseName :: P.Parser String
parseName = P.many (P.oneOf ['a' .. 'z'])

parseSectorID :: P.Parser Int
parseSectorID = read <$> P.many (P.oneOf ['0' .. '9'])

-- Problem DSL
checksum :: String -> String
checksum s = let counts = map (\x -> (x, count x s)) ['a' .. 'z']
             in map fst (take 5 (sortBy fSort counts))

fSort :: (Char, Int) -> (Char, Int) -> Ordering
fSort (a, b) (a', b') = compare b' b <> compare a a'

isValid room = checksum (roomName room) == roomChecksum room

-- utils

-- FIRST problem
day code = sum (map roomId (filter isValid code))

-- SECOND problem
realName room = map (rotateChar (roomId room)) (roomName room)

rotateChar _ '-' = ' '
rotateChar n c = let
  orda = ord 'a'
  in chr ((ord c - orda + n) `mod` 26 + orda)

day' code = let Just r = find (\x -> realName x == "northpole object storage") code
            in roomId r

-- tests and data

test = hspec $ do
  it "works" $ do
    day <$$> content `shouldReturn` Right 278221
    day' <$$> content `shouldReturn` Right 267

  it "rotates" $ do
    realName (Room "qzmt-zixmtkozy-ivhz" 343 undefined) `shouldBe` "very encrypted name"
  --parse "aaaaa-bbb-z-y-x-123[abxyz]" `shouldBe` Right (["aaaaa", "bbb", "z", "y", "x"], 123, "abxyz")

fileContent = readFile "content/day4"
content = parse <$> fileContent
