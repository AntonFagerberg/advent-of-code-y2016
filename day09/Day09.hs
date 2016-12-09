module Day09 where

import Text.Regex.PCRE
import Data.Char

-- Part 1
decompress :: String -> String
decompress "" = ""
decompress ('(':rest) = decompressed ++ next
  where 
    (size, times, remain) = parse rest
    decompressed = concat . take times . repeat . take size $ remain
    next = decompress . drop size $ remain
decompress (s:ss) = s : decompress ss

parse :: String -> (Int, Int, String)
parse text = (read size, read times, rest) where [[_, size, times, rest]] = (text =~ "(\\d+)x(\\d+)\\)(.*)" :: [[String]])

result1 :: FilePath -> IO ()
result1 filepath = readFile filepath >>= putStrLn . show . length . decompress

-- Part 2
nums :: String -> (Int, Int, Int)
nums text = (length match, read size, read times) 
  where [[match, size, times]] = (text =~ "^(\\d+)x(\\d+)\\)" :: [[String]])

decompress_length :: String -> Int
decompress_length "" = 0
decompress_length ('(':rest) = times * decompress_length sub_expression + decompress_length untouched
  where 
    (skip, size, times) = nums rest
    remain = drop skip rest
    sub_expression = take size remain
    untouched = drop size remain
decompress_length (_:ss) = 1 + decompress_length ss
  
result2 :: FilePath -> IO ()
result2 filepath = readFile filepath >>= putStrLn . show . decompress_length

-- Tests
test1 = "ADVENT" == decompress "ADVENT"
test2 = "ABBBBBC" == decompress "A(1x5)BC"
test3 = "XYZXYZXYZ" == decompress "(3x3)XYZ"
test4 = "ABCBCDEFEFG" == decompress "A(2x2)BCD(2x2)EFG"
test5 = "(1x3)A" == decompress "(6x1)(1x3)A"
test6 = "X(3x3)ABC(3x3)ABCY" == decompress "X(8x2)(3x3)ABCY"

-- test7 = "XYZXYZXYZ" == decompress_length "(3x3)XYZ"
-- test8 = "XABCABCABCABCABCABCY" == decompress_length "X(8x2)(3x3)ABCY"
-- test9 = (take 241920 . repeat $ 'A') == decompress_length "(27x12)(20x12)(13x14)(7x10)(1x12)A"
-- test10 = "XYZXYZXYZ" == decompress_length "(3x3)XYZ"
-- test11 = 445 == (length . decompress_length $ "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN")