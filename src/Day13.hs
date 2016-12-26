module Day13 where

import Numeric
import Data.Char
import Data.List

notWall :: Int -> (Int, Int) -> Bool
notWall key (x, y)
  | x < 0 || y < 0 = False
  | otherwise = mod ones 2 == 0
  where 
    ones = length . filter ('1' == ) $ binary
    binary = showIntAtBase 2 intToDigit number ""
    number = key + x*x + 3*x + 2*x*y + y + y*y
  
solve1 :: Int -> (Int, Int) -> Int -> [(Int, Int)] -> Int
solve1 key target steps history
  | target `elem` history = steps
  | otherwise = solve1 key target (steps + 1) newHistory
  where 
    optionsX = [(xx, y) | (x, y) <- history, xx <- [x - 1 .. x + 1]]
    optionsY = [(x, yy) | (x, y) <- history, yy <- [y - 1 .. y + 1]]
    newHistory = filter (notWall key) . nub $ (optionsX ++ optionsY)
  
solve2 :: Int -> Int -> [(Int, Int)] -> Int
solve2 key steps history
  | steps == 0 = length history
  | otherwise = solve2 key (steps - 1) newHistory
  where 
    optionsX = [(xx, y) | (x, y) <- history, xx <- [x - 1 .. x + 1]]
    optionsY = [(x, yy) | (x, y) <- history, yy <- [y - 1 .. y + 1]]
    newHistory = filter (notWall key) . nub $ (optionsX ++ optionsY)