module Day10 where
  
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map
  
parseStates :: [String] -> [(String, Int)]
parseStates ["value", value, "goes", "to", "bot", botId] = [("bot" ++ botId, read value)]
parseStates _ = []

botInstructions :: [String] -> [(String, String, String)]
botInstructions ["bot", bot, "gives", "low", "to", lowType, lowId, "and", "high", "to", highType, highId] = [("bot" ++ bot, lowType ++ lowId, highType ++ highId)]
botInstructions _ = []

mergeMap :: Map.Map String [Int] -> (String, Int) -> Map.Map String [Int]
mergeMap acc (botId, value) = Map.insert botId (value : currentValues) acc
                              where currentValues = Map.findWithDefault [] botId acc

inputPieces :: String -> [[String]]
inputPieces = map words . lines

process :: Map.Map String [Int] -> (String, String, String) -> Map.Map String [Int]
process state (botId, lowId, highId)
  | length item == 2 = Map.insert botId [] $ Map.insert lowId lowState $ Map.insert highId highState state
  | otherwise = state
  where
    item = Map.findWithDefault [] botId state
    lowState = minimum item : Map.findWithDefault [] lowId state
    highState = maximum item : Map.findWithDefault [] highId state

botWithElements :: [Int] -> (String, [Int]) -> Bool
botWithElements target (_, botElements) = sort target == sort botElements

infiniteHistory :: Map.Map String [Int] -> [(String, String, String)] -> [(String, [Int])]
infiniteHistory state instructions = result
  where
    history = scanl' process state instructions
    newState = last history
    result = (history >>= Map.toList) ++ infiniteHistory newState instructions

botComparingElements :: [Int] -> String -> Maybe (String, [Int])
botComparingElements elements input = find (botWithElements elements) $ infiniteHistory state instructions
  where
    state = foldl' mergeMap Map.empty $ inputPieces input >>= parseStates
    instructions = inputPieces input >>= botInstructions

outputValue :: String -> (String, [Int]) -> Bool
outputValue targetKey (key, values)
  | key == targetKey && (not . null) values = True
  | otherwise = False
  
-- A wild blackbird appears https://www.youtube.com/watch?v=seVSlKazsNk
(...) = (.) . (.)

solve1 :: [Int] ->  String -> String
solve1 = (fst . fromJust) ... botComparingElements

solve2 :: String -> Int
solve2 input = output0 * output1 * output2
    where
      state = foldl' mergeMap Map.empty $ inputPieces input >>= parseStates
      instructions = inputPieces input >>= botInstructions
      history = infiniteHistory state instructions
      output0 = head . snd . fromJust . find (outputValue "output0") $ history
      output1 = head . snd . fromJust . find (outputValue "output1") $ history
      output2 = head . snd . fromJust . find (outputValue "output2") $ history