module Day01 where

import Text.Regex.PCRE
import Data.Tuple

-- Part 1

format :: [String] -> (String, Int)
format [_, direction, num] = (direction, read num :: Int)

split :: String -> [[String]]
split text = text =~ "(L|R)(\\d+)" :: [[String]]

multiplier :: Int -> Int
multiplier number = mod number 2 + (-2) * div number 3

move :: (String, Int) -> ((Int, Int), Int) -> (Int, Int)
move ("R", steps) ((x, y), direction) = (x + steps * multiplier (mod (direction + 1) 4), y + steps * multiplier (mod (direction + 2) 4))
move ("L", steps) (xy, direction) = move ("R", steps) (xy, mod (direction + 2) 4)

turn "R" direction = mod (direction + 1) 4
turn "L" direction = mod (direction - 1) 4

walk :: ((Int, Int), Int) -> (String, Int) -> ((Int, Int), Int)
walk command@(_, direction) position@(side, _) = (move position command, turn side direction)

blocks :: (Int, Int) -> Int
blocks (x, y) = abs x + abs y

solve1 :: String -> Int
solve1 = blocks . fst . foldl walk ((0, 0), 0) . fmap format . split

-- Part 2

slide :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
slide [_] = []
slide (x:xx:xs) = (x, xx) : slide (xx:xs)

steps :: [((Int, Int), (Int, Int))] -> [(Int, Int)]
steps f = let (x:xs) = map expand f
          in x ++ (xs >>= tail)

expand :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
expand s@((x1, y1), (x2, y2))
  | x1 > x2 = reverse . steps $ ((x2, y1), (x1, y2))
  | y1 > y2 = reverse . steps $ ((x1, y2), (x2, y1))
  | otherwise = steps s
  where steps ((x1, y1), (x2, y2)) = [(x, y) | x <- [x1..x2], y <- [y1..y2]]
  
duplicate :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int)
duplicate visited (position:rest) 
  | position `elem` visited = position
  | otherwise = duplicate (position:visited) rest

solve2 :: String -> Int
solve2 = blocks . duplicate [] . steps . slide . map fst . scanl walk ((0, 0), 0) . fmap format . split
