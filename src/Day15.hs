module Day15 where

import Text.Regex.PCRE
import Debug.Trace

parse :: String -> [(Int, Int)]
parse input = (\[positions, position] -> (positions, position)) . fmap read . tail . tail <$> matches
  where matches = input =~ "Disc #(\\d+) has (\\d+) positions; at time=0, it is at position (\\d+)." :: [[String]]

rotate :: [(Int, Int)] -> Int
rotate discs
  | any (( /= 0) . snd) capsuleFall = 1 + rotate rotatedDiscs
  | otherwise = 0
  where 
    capsuleFall = fmap (\(time, (limit, position)) -> (limit, mod (time + position) limit)) . zip [1..] $ discs
    rotatedDiscs = fmap (\(limit, position) -> (limit, mod (1 + position) limit)) discs

solve1 :: String -> Int
solve1 = rotate . parse

solve2 :: String -> Int
solve2 = rotate . ( ++ [(11, 0)]) . parse