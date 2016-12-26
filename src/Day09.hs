module Day09 where

import Text.Regex.PCRE
import Data.Char

-- Part 1

decompress :: String -> String
decompress "" = ""
decompress ('(':rest) = decompressed ++ next
  where 
    (size, times, remain) = parse rest
    decompressed = concat . replicate times . take size $ remain
    next = decompress . drop size $ remain
decompress (s:ss) = s : decompress ss

parse :: String -> (Int, Int, String)
parse text = (read size, read times, rest) where [[_, size, times, rest]] = text =~ "(\\d+)x(\\d+)\\)(.*)" :: [[String]]

solve1 :: String -> Int
solve1 =  length . decompress

-- Part 2

nums :: String -> (Int, Int, Int)
nums text = (length match, read size, read times) 
  where [[match, size, times]] = text =~ "^(\\d+)x(\\d+)\\)" :: [[String]]

solve2 :: String -> Int
solve2 "" = 0
solve2 ('(':rest) = times * solve2 sub_expression + solve2 untouched
  where 
    (skip, size, times) = nums rest
    remain = drop skip rest
    sub_expression = take size remain
    untouched = drop size remain
solve2 (_:ss) = 1 + solve2 ss