module Day03 where

import Data.List

parse :: String -> [Int]
parse  = fmap read . words

valid :: [Int] -> Bool
valid input = a + b > c where [a, b, c] = sort input

-- Part 1
solve1 :: String -> Int
solve1 = length . filter id . fmap (valid . parse) . lines


-- Part 2

regroup :: [[Int]] -> [[Int]]
regroup [] = []
regroup ([a1, b1, c1] : [a2, b2, c2] : [a3, b3, c3] : rest) = [a1, a2, a3] : [b1, b2, b3] : [c1, c2, c3] : regroup rest

solve2 :: String -> Int
solve2 = length . filter id . fmap valid . regroup . fmap parse . lines
