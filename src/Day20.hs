module Day20 where

import Data.List
import Data.List.Split
import Debug.Trace

parse :: String -> [(Integer, Integer)]
parse = sort . fmap ((\[from ,to] -> (read from, read to)) . splitOn "-") . lines

findMin :: Integer -> [(Integer, Integer)] -> Integer
findMin current ranges
  | null matchRanges = current
  | otherwise = findMin (maxInRange + 1) remainingRanges
  where
    maxInRange = maximum . fmap snd $ matchRanges
    (matchRanges, remainingRanges) = partition (\(from, to) -> from <= current && to >= current) ranges

findAll :: Integer -> Integer -> [(Integer, Integer)] -> Integer
findAll highest current []
  | highest > current = highest - current
  | otherwise = 0

findAll highest current ranges
  | current >= highest = 0
  | null matchRanges = min highest nextMin - current + findAll highest nextMin remainingRanges
  | otherwise = findAll highest (maxInRange + 1) filteredRanges
  where
    filteredRanges = filter ((maxInRange <= ) . snd) remainingRanges
    nextMin = fst . head $ remainingRanges
    maxInRange = maximum . fmap snd $ matchRanges
    (matchRanges, remainingRanges) = partition (\(from, to) -> from <= current && to >= current) ranges

solve1 :: String -> Integer
solve1 = findMin 0 . parse

solve2 :: String -> Integer
solve2 = findAll 4294967295 0 . parse