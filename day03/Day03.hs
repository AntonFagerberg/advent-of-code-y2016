module Day01 where

import Data.List

parse :: String -> [Int]
parse  = fmap read . words

valid :: [Int] -> Bool
valid input = a + b > c where [a, b, c] = sort input

-- Part 1
solve1 :: String -> Int
solve1 = length . filter id . fmap (valid . parse) . lines

result1 :: FilePath -> IO ()
result1 filepath = readFile filepath >>= putStrLn . show . solve1

test_input1 :: String
test_input1 = "5 10 25"

test1 = valid . parse $ test_input1

-- Part 2

test_input2 :: String
test_input2 = "101 301 501\n102 302 502\n103 303 503\n201 401 601\n202 402 602\n203 403 603"

regroup :: [[Int]] -> [[Int]]
regroup [] = []
regroup ([a1, b1, c1] : [a2, b2, c2] : [a3, b3, c3] : rest) = [a1, a2, a3] : [b1, b2, b3] : [c1, c2, c3] : regroup rest

solve2 :: String -> Int
solve2 = length . filter id . fmap valid . regroup . fmap parse . lines

result2 :: FilePath -> IO ()
result2 filepath = readFile filepath >>= putStrLn . show . solve2