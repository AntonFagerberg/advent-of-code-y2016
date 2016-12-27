module Day19 where
  
import Data.List
import Debug.Trace
  
dropFirstIfOdd :: [a] -> [a]
dropFirstIfOdd list
  | (odd . length) list = tail list
  | otherwise = list

solve1 :: Int -> Int
solve1 count = head . head . filter ((==) 1 . length) . iterate (snd . unzip . filter (even . fst) . dropFirstIfOdd . zip [0..]) $ [1..count]

solve2 :: Int -> Int
solve2 size = circleElves [1 .. size]

removeElves :: Int -> Int -> Int -> Int -> Int -> [Int] -> [Int]
removeElves elfCount removed minIndex maxIndex index elves
  | removed + 1 == maxIndex && shouldBeRemoved = tail elves
  | shouldBeRemoved = removeElves elfCount (removed + 1) minIndex maxIndex (index + 1) (tail elves)
  | otherwise  = head elves : removeElves elfCount removed minIndex maxIndex (index + 1) (tail elves)
  where
    shouldBeRemoved = index >= minIndex && div (elfCount - removed) 2 + 2 * removed == index

circleElves :: [Int] -> Int
circleElves elves
  | elfCount == 1 || elfCount == 2 || elfCount == 4 = head elves
  | elfCount == 3 = elves !! 2
  | otherwise = circleElves (partition2 ++ partition1)
  where
    elfCount = length elves
    middleIndex = div elfCount 2
    removed = ceiling $ fromIntegral elfCount / 3.0
    remainingElves = removeElves elfCount 0 middleIndex removed 0 elves
    (partition1, partition2) = splitAt removed remainingElves