module Day10 where

import Test.Hspec

import qualified Text.Megaparsec.String as P
import qualified Text.Megaparsec as P

import qualified Data.Map as Map
import Data.Map (Map)

import Data.List

import Debug.Trace

import Utils

-- Parsing
parser = parseLine `P.sepBy` (P.string "\n")

parseLine = (GiveR <$> parseGive) P.<|> (ValueR <$> parseValue)

parseBot = Bot <$> (P.string "bot " *> number)

parseValue = do
  P.string "value "
  n <- number
  P.string " goes to "
  bot <- parseBot

  return $ Value n bot

parseOutputO = do
  P.string "output "
  n <- number
  return $ OutputO n

parseOutput = parseOutputO P.<|> (BotO <$> parseBot)

parseGive = do
  bot <- parseBot
  P.string " gives low to "
  outLow <- parseOutput
  P.string " and high to "
  outHigh <- parseOutput

  return $ Give bot outLow outHigh

number :: P.Parser Int
number = read <$> P.many (P.oneOf "0123456789")

-- Input DSL
data Value = Value Int Bot deriving (Show)
data Give = Give Bot Output Output deriving (Show)

data Rule = ValueR Value | GiveR Give deriving (Show)

data Bot = Bot Int deriving (Show, Ord, Eq)

data Output = BotO Bot | OutputO Int deriving (Show)

-- Problem DSL
type BotStatus = [Int]

splitValueGive :: [Rule] -> ([Value], [Give])
splitValueGive [] = ([], [])
splitValueGive (x:xs) = let (values, gives) = splitValueGive xs
                        in case x of
                             ValueR v -> (v : values, gives)
                             GiveR g -> (values, g : gives)

makeBotStatuses :: [Value] -> Map Bot BotStatus
makeBotStatuses l = Map.fromListWith (++) (map (\(Value v bot) -> (bot, [v])) l)

makeGiveDict :: [Give] -> Map Bot (Output, Output)
makeGiveDict g = Map.fromList (map (\(Give bot a b) -> (bot, (a, b))) g)

initProblem :: [Rule] -> (Map Bot BotStatus, Map Bot (Output, Output))
initProblem rules = (botStatuses, giveDict)
  where
    (values, gives) = splitValueGive rules
    botStatuses = makeBotStatuses values
    giveDict = makeGiveDict gives
-- utils


-- FIRST problem
doit code = go botStatuses [] []
  where
    (botStatuses, giveDict) = initProblem code
    go statuses finalBots outputs = let currentBot = find withTwo (Map.toList statuses)
                                    in case currentBot of
                                         Nothing -> (finalBots, outputs)
                                         Just (bot, status) -> let (newStatuses, updatedOutputs) = transfertBot statuses bot (giveDict Map.! bot)
                                                                   newFinalBots = if checkStatus status then bot:finalBots else finalBots
                                                               in go newStatuses newFinalBots (outputs ++ updatedOutputs)

transfertBot :: Map Bot BotStatus -> Bot -> (Output, Output) -> (Map Bot BotStatus, [(Int, Int)])
transfertBot statuses bot (lowTo, highTo) = let [lowVal, highVal] = sort (statuses Map.! bot)
                                                -- reset currentBot
                                                statuses' = Map.insert bot [] statuses
                                                -- add to the other bots
                                                (statuses'', o) = insertChip lowVal lowTo statuses'
                                                (statuses''', o') = insertChip highVal highTo statuses''
                                            in (statuses''', o ++ o')

insertChip :: Int -> Output -> Map Bot BotStatus -> (Map Bot BotStatus, [(Int, Int)])
insertChip val (OutputO o) m = (m, [(o, val)])
insertChip val (BotO b) m = (Map.insertWith (++) b [val] m, [])

withTwo (bot, status) = length status == 2
checkStatus status = sort status == [17, 61]

day code = fst (doit code)
-- SECOND problem
day' code = let outputs = take 3 (sortOn fst (snd (doit code)))
            in product (map snd outputs)

-- tests and data

-- comment out and add tests
test = hspec $ it "works" $ do
  day <$> content `shouldReturn` [Bot 141]
  day' <$> content `shouldReturn` 1209

fileContent = readFile "content/day10"
content = parse parser <$> fileContent

exampleSimple = "value 5 goes to bot 2\n\
\bot 2 gives low to bot 1 and high to bot 0\n\
\value 3 goes to bot 1\n\
\bot 1 gives low to output 1 and high to bot 0\n\
\bot 0 gives low to output 2 and high to output 0\n\
\value 2 goes to bot 2"
