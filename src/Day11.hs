module Day11 where

import Text.Regex.PCRE ((=~))
import Data.List ((++), (\\), null, partition, sort, tails)
import Data.Set (Set, member, fromList, union, empty, toList)
import Control.Monad (replicateM)

type Floors = [[[String]]]

parse :: String -> Floors
parse = fmap (fmap (filter (not . null) . tail) . (=~ "([a-z]+)-compatible (microchip)|([a-z]+)\\s(generator)")) . lines

valid :: Floors -> Bool
valid [] = True
valid (current:rest)
  | null generators = valid rest
  | null microchips = valid rest
  | null $ fmap head microchips \\ fmap head generators = valid rest
  | otherwise = False
  where 
    (microchips, generators) = partition (("microchip" == ) . last) current

brute :: Int -> Set (Int, Floors) -> Set (Int, Floors) -> Int
brute maxHeight history state
  | any (all null) . fmap (init . snd) . toList $ state = 0
  | otherwise = 1 + brute newMaxHeight newHistory newState
  where
    newState = fromList $ singleStateChanges ++ dualStateChanges
    newMaxHeight = maximum . fmap (length . dropWhile null . reverse . snd) . toList $ newState
    newHistory = history `union` newState
    singleStateChanges = [ (targetFloor, finalState) | (position, floors) <- toList state,
                                                       firstItem <- floors !! position,
                                                       targetFloor <- [position - 1, position + 1], 
                                                       targetFloor >= 0,
                                                       targetFloor < length floors,
                                                       let (rmBefore, rmTarget : rmAfter) = splitAt position floors,
                                                       let cleanFloors = rmBefore ++ filter (firstItem /=) rmTarget : rmAfter,
                                                       let (inBefore, inTarget: inAfter) = splitAt targetFloor cleanFloors,
                                                       let finalState = inBefore ++ sort (firstItem : inTarget) : inAfter,
                                                       (maxHeight <=) . length . dropWhile null . reverse $ finalState,
                                                       not ((targetFloor, finalState) `member` history),
                                                       valid finalState ]
    dualStateChanges = [ (targetFloor, finalState) | (position, floors) <- toList state,
                                                     (firstItem:restItems) <- tails (floors !! position),
                                                     secondItem <- restItems,
                                                     targetFloor <- [position - 1, position + 1], 
                                                     targetFloor >= 0,
                                                     targetFloor < length floors,
                                                     let (rmBefore, rmTarget : rmAfter) = splitAt position floors,
                                                     let cleanFloors = rmBefore ++ filter (not . (`elem` [firstItem, secondItem])) rmTarget : rmAfter,
                                                     let (inBefore, inTarget: inAfter) = splitAt targetFloor cleanFloors,
                                                     let finalState = inBefore ++ sort (firstItem : secondItem : inTarget) : inAfter,
                                                     (maxHeight <=) . length . dropWhile null . reverse $ finalState,
                                                     not ((targetFloor, finalState) `member` history),
                                                     valid finalState ]

solve1 :: String -> Int
solve1  input = brute 0 (fromList [(0, state)]) (fromList [(0, state)])
  where state = fmap sort . parse $ input
    
solve2 :: String -> Int
solve2 input = brute 0 (fromList [(0, state)]) (fromList [(0, state)])
  where 
    (firstFloor: rest) = parse input
    state = fmap sort $ (["elerium","generator"] : ["elerium", "microchip"] : ["dilithium","generator"] : ["dilithium", "microchip"] : firstFloor) : rest