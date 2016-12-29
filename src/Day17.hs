module Day17 where

import Data.List
import Data.Maybe
import Crypto.Hash
import Crypto.Hash.Algorithms
import qualified Data.ByteString.Char8 as B

md5 :: B.ByteString -> Digest MD5
md5 = hash

md5String :: String -> String
md5String = show . md5 . B.pack

validOptions :: String -> String -> (Int, Int) -> [((Int, Int), Char)]
validOptions passcode path (x, y) = up ++ down ++ left ++ right
  where
    options = md5String (passcode ++ path) 
    open = "bcdef"
    up
      | y > 0 && (options !! 0) `elem` open = [((0,-1), 'U')]
      | otherwise = []
    down
      | y < 3 && (options !! 1) `elem` open = [((0,1), 'D')]
      | otherwise = []
    left
      | x > 0 && (options !! 2) `elem` open = [((-1,0), 'L')]
      | otherwise = []
    right
      | x < 3 && (options !! 3) `elem` open = [((1,0), 'R')]
      | otherwise = []

walk1 :: [((Int, Int), String)] -> String -> String
walk1 movements passcode
  | isJust match = snd . fromJust $ match
  | otherwise = walk1 nextMovements passcode
  where
    match = find (((3, 3) ==) . fst) movements
    nextMovements = [((x + xx, y + yy), path ++ [move]) | ((x, y), path) <- movements, ((xx, yy), move) <- validOptions passcode path (x, y)]

solve1 :: String -> String
solve1 = walk1 [((0, 0), "")]

walk2 :: [((Int, Int), String)] -> String -> Int
walk [] _ = []
walk2 movements passcode
  | null movements = 0
  | null matches = walk2 nextMovements passcode
  | otherwise = max (maximum . fmap (length . snd) $ matches) (walk2 nextMovements passcode)
  where
    (matches, nextMovements) = partition (((3, 3) ==) . fst) [((x + xx, y + yy), path ++ [move]) | ((x, y), path) <- movements, ((xx, yy), move) <- validOptions passcode path (x, y)]

solve2 :: String -> Int
solve2 = walk2 [((0, 0), "")]