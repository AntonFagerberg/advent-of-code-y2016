module Day25 where

import Data.List
import Data.Maybe
import Control.Arrow
import Data.Map.Strict as Map

toggle :: [String] -> [String]
toggle ["inc", target] = ["dec", target]
toggle [other, target] = ["inc", target]
toggle ("jnz":args) = "cpy" : args
toggle ["nop"] = ["nop"]
toggle (_:args) = "jnz" : args

updateInstructions :: [[String]] -> Int -> Map.Map String Int -> [String] -> [[String]]
updateInstructions instructions index registers ["tgl", target]
  | length instructions <= targetIndex = instructions
  | otherwise = updatedInstructions
  where 
    targetIndex = index + Map.findWithDefault (read target) target registers
    updatedInstructions = before ++ toggle instruction : after
    (before, instruction : after) = splitAt targetIndex instructions

updateInstructions instructions _ _ _ = instructions

updateRegisters :: [String] -> Map.Map String Int -> Map.Map String Int
updateRegisters ["cpy", from, to] registers = Map.insert to (Map.findWithDefault (read from) from registers) registers
updateRegisters ["inc", target] registers = Map.insert target (1 + Map.findWithDefault 0 target registers) registers
updateRegisters ["dec", target] registers = Map.insert target ((-1) + Map.findWithDefault 0 target registers) registers
updateRegisters _ registers = registers

transmit :: [String] -> Map.Map String Int -> Maybe Int
transmit ["out", target] registers
  | target `elem` ["a", "b", "c", "d"] = Map.lookup target registers
  | otherwise = Just $ read target
transmit _ _ = Nothing

nextIndex :: [String] -> Map.Map String Int -> Int
nextIndex ["jnz", "0", _] _ = 1
nextIndex ["jnz", target, offset] registers
  | targetValue == 0 = 1
  | otherwise = offsetValue
  where 
    targetValue = Map.findWithDefault (read target) target registers
    offsetValue = Map.findWithDefault (read offset) offset registers
nextIndex _ _ = 1
  
compute :: Int -> Map.Map String Int -> [[String]] -> (Int, Map.Map String Int, [[String]], Maybe Int)
compute index registers instructions
  | isJust output = (newIndex, updatedRegisters, updatedInstructions, output)
  | length instructions <= index = (index, registers, instructions, output)
  | otherwise = compute newIndex updatedRegisters updatedInstructions
  where 
    currentInstruction = instructions !! index
    output = transmit currentInstruction registers
    updatedInstructions = updateInstructions instructions index registers currentInstruction
    updatedRegisters = updateRegisters currentInstruction registers
    newIndex = index + nextIndex currentInstruction updatedRegisters

output :: Int -> Map.Map String Int -> [[String]] -> Maybe Int -> [Int]
output index registers instructions value
  | isJust signal = fromJust signal : output newIndex updatedRegisters updatedInstructions Nothing
  | otherwise = []
  where
    initializedRegisters
      | isJust value = Map.insert "a" (fromJust value) registers
      | otherwise = registers
    (newIndex, updatedRegisters, updatedInstructions, signal) = compute index initializedRegisters instructions

validate :: Int -> [(Int, [Int])] -> [(Int, [Int])]
validate items = Data.List.filter ((==) (take items $ cycle [0,1]) . take items . snd)

solve :: String -> Int
solve input = fst . head . validate 100 . validate 25 . validate 10 . validate 3 . zip [0..] . fmap (output 0 emptyRegisters instructions . Just) $ [0..]
  where 
    instructions = fmap words . lines $ input
    emptyRegisters = Map.fromList [("a", 0), ("b", 0), ("c", 0), ("d", 0)]