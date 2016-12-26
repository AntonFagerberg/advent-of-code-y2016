module Day05 where

import Data.Char
import Crypto.Hash
import Crypto.Hash.Algorithms
import qualified Data.ByteString.Char8 as B

md5 :: B.ByteString -> Digest MD5
md5 = hash

md5String :: String -> String
md5String = show . md5 . B.pack

crack :: String -> [String]
crack door_id = filter ((==) "00000" . take 5) . fmap (md5String . (++) door_id . show) $ [0..]

-- Part 1

solve1 :: String -> String
solve1 = concat . take 8 . fmap (take 1 . drop 5) . crack

-- Part 2

solve2 :: String -> String
solve2 = build "________" . fmap (take 2 . drop 5) . crack

validate :: String -> [String] -> String
validate solution suggestions
  | '_' `elem` solution = build solution suggestions
  | otherwise = solution

build :: String -> [String] -> String
build solution  ((i:c:_):suggestions)
  | isDigit i && nr < 8 && (solution !! nr) == '_' = validate (take nr solution ++ [c] ++ drop (nr + 1) solution) suggestions
  | otherwise = build solution suggestions
  where nr = digitToInt i