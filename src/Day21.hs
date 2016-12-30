module Day21 where

import Data.List
import Data.Maybe
import Text.Regex.PCRE

swapLetter :: Char -> Char -> Char -> Char
swapLetter x y char
  | char == x = y
  | char == y = x
  | otherwise = char

command :: String -> [String]
command text = (unwords . take 2 . words $ command) : parts
  where
    (command : parts) = head . fmap head . filter (not . null) . fmap (text =~) $ matchcommands
    matchcommands = [
        "swap position (\\d+) with position (\\d+)",
        "swap letter (\\w+) with letter (\\w+)",
        "rotate (left|right) (\\d+) steps?",
        "rotate based on position of letter (\\w+)",
        "reverse positions (\\d+) through (\\d+)",
        "move position (\\d+) to position (\\d+)"
      ]

mapping :: Int -> Int -> Int
mapping size index
  | index >= 4 = mod (2 + index) size
  | otherwise = mod (1 + index) size

process :: String -> [String] -> String
process string ["swap position", x, y] = change
  where
    letterX = string !! read x
    letterY = string !! read y
    (beforeX, _ : afterX) = splitAt (read x) string
    partialChange = beforeX ++ letterY : afterX
    (beforeY, _ : afterY) = splitAt (read y) partialChange
    change = beforeY ++ letterX : afterY

process string ["swap letter", [x], [y]] = fmap (swapLetter x y) string

process string ["rotate left", _, steps] = before ++ after
      where (after, before) = splitAt (read steps) string

process string ["rotate right", _, steps] = before ++ after
      where (after, before) = splitAt (length string - read steps) string
      
process string ["reverse positions", from, to] = before ++ reversed ++ after
  where
    (temp, after) = splitAt (read to + 1) string
    (before, middle) = splitAt (read from) temp
    reversed = reverse middle
    
process string ["move position", from, to] = change
  where
    (before1, target : after1) = splitAt (read from) string
    (before2, after2)
      | read to < length before1 = splitAt (read to) (before1 ++ after1)
      | otherwise = splitAt (read to) (before1 ++ after1)
    change = before2 ++ target : after2
    
process string ["rotate based", [letter]] = process string ["rotate right", "", show $ mapping (length string) index]
  where 
    index = fromJust $ letter `elemIndex` string
  
process string ["rotate inversed", [letter]]
  | index == target = string
  | index < target = process string ["rotate right", "", show $ target - index]
  | index > target = process string ["rotate left", "", show $ index - target]
  where
    size = length string
    index = fromJust $ letter `elemIndex` string
    [target] = filter ((index ==) . flip mod size . (\option -> option + mapping size option)) [0 .. size - 1]

parse :: String -> [[String]]
parse = fmap command . lines

solve1 :: String -> String -> String
solve1 string commands = foldl' process string (parse commands)

inverse :: [String] -> [String]
inverse ("rotate left":rest) = "rotate right" : rest
inverse ("rotate right":rest) = "rotate left" : rest
inverse ["move position", from, to] = ["move position", to, from]
inverse ("rotate based":rest) = "rotate inversed" : rest
inverse other = other

solve2 :: String -> String -> String
solve2 string commands = foldl' process string (fmap inverse . reverse . parse $ commands)