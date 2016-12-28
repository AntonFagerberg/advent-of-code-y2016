module Day24 where

import Data.List as List
import Data.Maybe
import Data.Map.Strict as Map
import Control.Arrow

start :: [String] -> (Int, Int)
start = head . fmap (second fromJust) . List.filter (isJust . snd) . zip [0..] . fmap (elemIndex '0')

targets :: [String] -> [(Int, Int)]
targets room = [(y, x) | (y, xx) <- coordinates, x <- xx]
  where coordinates = List.filter (not . List.null . snd) . zip [0..] . fmap (fmap fst . List.filter (not . (`elem` ['.', '#']) . snd) . zip [0..]) $ room

canWalk :: [String] -> (Int, Int) -> Bool
canWalk room (y, x)
  | y >= 0 && x >= 0 && length room > y && length row > x = row !! x /= '#'
  | otherwise = False
  where row = room !! y

walk :: [String] -> (Int, Int) -> [(Int, Int)] -> Int
walk room target history
  | target `elem` history = 0
  | otherwise = 1 + walk room target newHistory
  where
    optionsX = [(y, xx) | (y, x) <- history, xx <- [x - 1 .. x + 1]]
    optionsY = [(yy, x) | (y, x) <- history, yy <- [y - 1 .. y + 1]]
    newHistory = List.filter (not . (`elem` history)) . List.filter (canWalk room) . nub $ (optionsX ++ optionsY)

sliding :: Int -> [a] -> [[a]]
sliding n xs = List.filter ((n ==) . length) . fmap (take n) $ tails xs

solve1 :: String -> Int
solve1 input = minimum allTravelDistances
  where
    room = lines input
    allTargets = targets room
    allPointCombinations = [(from, to) | from <- allTargets, to <- List.filter (from < ) allTargets]
    travelDistances = Map.fromList $ fmap (\(from, to) -> ([from, to], walk room to [from])) allPointCombinations
    startPosition = start room
    allTravels = fmap (startPosition :) . permutations . List.filter (startPosition /=) $ allTargets
    allTravelDistances = fmap (sum . fmap (fromJust . flip Map.lookup travelDistances . sort) . sliding 2) allTravels

solve2 :: String -> Int
solve2 input = minimum allTravelDistances
  where
    room = lines input
    allTargets = targets room
    allPointCombinations = [(from, to) | from <- allTargets, to <- List.filter (from < ) allTargets]
    travelDistances = Map.fromList $ fmap (\(from, to) -> ([from, to], walk room to [from])) allPointCombinations
    startPosition = start room
    allTravels = fmap ((++ [startPosition]) . (startPosition :)) . permutations . List.filter (startPosition /=) $ allTargets
    allTravelDistances = fmap (sum . fmap (fromJust . flip Map.lookup travelDistances . sort) . sliding 2) allTravels