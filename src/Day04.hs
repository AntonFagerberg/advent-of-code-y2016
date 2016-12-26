module Day04 where
  
import Text.Regex.PCRE
import qualified Data.Map.Strict as Map
import Data.List
import Data.Char
import Data.Maybe
import Data.Tuple.Select

split :: String -> [[String]]
split text = text =~ "([a-z-]+)([0-9]+)\\[([a-z]+)\\]"

parse :: [[String]] -> (String, Int, String)
parse [[_, room, sector, checksum]] = (room, read sector, checksum)

charSort :: String -> String -> Ordering
charSort a b
  | len_a > len_b = LT
  | len_a < len_b = GT
  | otherwise = compare a b
  where (len_a, len_b) = (length a, length b)

checksum :: String -> String
checksum = take 5 . fmap head . sortBy charSort . group . sort . filter ('-' /=)

validate :: (String, Int, String) -> Int
validate (room, sector, validation_checksum)
  | checksum room == validation_checksum = sector
  | otherwise = 0

-- Part 1

solve1 :: String -> Int
solve1 = sum . fmap (validate . parse . split) . lines

-- Part 2 

decrypt :: Int -> Char -> Char
decrypt _ '-' = ' '
decrypt key char = chr shifted
                    where 
                      min_int = ord 'a'
                      max_int = ord 'z' + 1 - min_int
                      char_int = ord char
                      key_shift = char_int - min_int + key
                      shifted = min_int + mod key_shift max_int

target :: String -> (String, Int, String) -> Bool
target text (room, sector, validation_checksum) = checksum room == validation_checksum && fmap (decrypt sector) room == text

solve2 :: String -> Int
solve2 = sel2 . fromJust . find (target "northpole object storage ") . fmap (parse . split) . lines
