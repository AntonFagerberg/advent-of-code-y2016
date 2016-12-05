module Day01 where

import Data.Char
import Crypto.Hash
import Crypto.Hash.Algorithms
import qualified Data.ByteString.Char8 as B

md5 :: B.ByteString -> Digest MD5
md5 = hash

md5_string :: String -> String
md5_string = show . md5 . B.pack

crack :: String -> [String]
crack door_id = filter ((==) "00000" . take 5) . fmap (md5_string . (++) door_id . show) $ [0..]

-- Part 1
solve1 :: String -> String
solve1 = foldl (++) "" . take 8 . fmap (take 1 . drop 5) . crack

result1 :: String
result1 =  solve1 "wtnhxymk"

-- Part 2
solve2 :: String -> String
solve2 = build "________" . fmap (take 2 . drop 5) . crack

validate :: String -> [String] -> String
validate solution suggestions
  | any ((==) '_') solution = build solution suggestions
  | otherwise = solution

build :: String -> [String] -> String
build solution  ((i:c:_):suggestions)
  | isDigit i && nr < 8 && (solution !! nr) == '_' = validate ((take nr solution) ++ [c] ++ (drop (nr + 1) solution)) suggestions
  | otherwise = build solution suggestions
  where nr = digitToInt i

result2 :: String
result2 = solve2 "wtnhxymk"

-- Tests
test1 :: Bool
test1 = "18f47a30" == solve1 "abc"

test2 :: Bool
test2 = "05ace8e3" == solve2 "abc"