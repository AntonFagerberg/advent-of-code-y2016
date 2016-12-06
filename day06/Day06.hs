module Day06 where

import Data.List
import Data.Function

pivot :: [String] -> [String]
pivot ([]:_) = []
pivot x = (h:t)
  where 
    h = fmap head x
    t = pivot . fmap tail $ x


solve :: ([String] -> String) -> String -> String
solve select = fmap (head . select . group . sort) . pivot . lines

-- Part 1
solve1 :: String -> String
solve1 = solve $ maximumBy (compare `on` length)

result1 :: FilePath -> IO ()
result1 filepath = readFile filepath >>= putStrLn . show . solve1

-- Part 2
solve2 :: String -> String
solve2 = solve $ minimumBy (compare `on` length)

result2 :: FilePath -> IO ()
result2 filepath = readFile filepath >>= putStrLn . show . solve2

-- Tests
test_input1 = "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar"

test1 = "easter" == solve1 test_input1
test2 = "advent" == solve2 test_input1