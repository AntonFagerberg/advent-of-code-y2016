module Day12 where

import Data.List
import Data.Maybe
import Data.Map.Strict as Map

updateRegisters :: [String] -> Map.Map String Int -> Map.Map String Int
updateRegisters ["cpy", from, to] registers = Map.insert to (Map.findWithDefault (read from) from registers) registers
updateRegisters ["inc", target] registers = Map.insert target (1 + Map.findWithDefault 0 target registers) registers
updateRegisters ["dec", target] registers = Map.insert target ((-1) + Map.findWithDefault 0 target registers) registers
updateRegisters _ registers = registers

nextIndex :: [String] -> Map.Map String Int -> Int
nextIndex ["jnz", "0", _] _ = 1
nextIndex ["jnz", target, offset] registers
  | value == 0 = 1
  | otherwise = read offset
  where value = Map.findWithDefault (read target) target registers
nextIndex _ _ = 1
  
compute :: Int -> Map.Map String Int -> [[String]] -> Map.Map String Int
compute index registers instructions
  | length instructions <= index = registers
  | otherwise = compute newIndex updatedRegisters instructions
  where 
    currentInstruction = instructions !! index
    updatedRegisters = updateRegisters currentInstruction registers
    newIndex = index + nextIndex currentInstruction updatedRegisters
    
solve1 :: String -> Int
solve1 = fromJust . Map.lookup "a" . compute 0 (Map.fromList [("a", 0), ("b", 0), ("c", 0), ("d", 0)]) . fmap words . lines
    
solve2 :: String -> Int
solve2 = fromJust . Map.lookup "a" . compute 0 (Map.fromList [("a", 0), ("b", 0), ("c", 1), ("d", 0)]) . fmap words . lines