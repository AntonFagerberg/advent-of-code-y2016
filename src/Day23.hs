{- 
  I did not do the multiplication optimization so part 2 takes ~90 min.
-}
module Day23 where

import Data.List
import Data.Maybe
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

nextIndex :: [String] -> Map.Map String Int -> Int
nextIndex ["jnz", "0", _] _ = 1
nextIndex ["jnz", target, offset] registers
  | targetValue == 0 = 1
  | otherwise = offsetValue
  where 
    targetValue = Map.findWithDefault (read target) target registers
    offsetValue = Map.findWithDefault (read offset) offset registers
nextIndex _ _ = 1
  
compute :: Int -> Map.Map String Int -> [[String]] -> Map.Map String Int
compute index registers instructions
  | length instructions <= index = registers
  | otherwise = compute newIndex updatedRegisters updatedInstructions
  where 
    currentInstruction = instructions !! index
    updatedInstructions = updateInstructions instructions index registers currentInstruction
    updatedRegisters = updateRegisters currentInstruction registers
    newIndex = index + nextIndex currentInstruction updatedRegisters

solve :: Int -> String -> Int
solve initValue input = fromJust . Map.lookup "a" $  compute 0 (Map.fromList [("a", initValue), ("b", 0), ("c", 0), ("d", 0)]) . fmap words . lines $ input