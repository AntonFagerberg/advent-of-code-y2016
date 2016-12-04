module Day01 where
  
import Text.Regex.PCRE
import qualified Data.Map.Strict as Map
import Data.List
import Data.Char

split :: String -> [[String]]
split text = text =~ "([a-z-]+)([0-9]+)\\[([a-z]+)\\]"

parse :: [[String]] -> ([Char], Int, [Char])
parse [[_, room, sector, checksum]] = (room, read sector, checksum)

char_sort :: String -> String -> Ordering
char_sort a b
  | len_a > len_b = LT
  | len_a < len_b = GT
  | otherwise = compare a b
  where (len_a, len_b) = (length a, length b)

checksum :: String -> String
checksum = take 5 . fmap head . sortBy char_sort . group . sort . filter ('-' /=)

validate :: ([Char], Int, [Char]) -> Int
validate (room, sector, validation_checksum)
  | checksum room == validation_checksum = sector
  | otherwise = 0

-- Part 1
result1 :: FilePath -> IO ()
result1 filepath = readFile filepath >>= putStrLn . show . solve1

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

target :: String -> ([Char], Int, [Char]) -> Bool
target text (room, sector, validation_checksum) = checksum room == validation_checksum && fmap (decrypt sector) room == text

solve2 :: String -> Maybe ([Char], Int, [Char])
solve2 = find (target "northpole object storage ") . fmap (parse . split) . lines

result2 :: FilePath -> IO ()
result2 filepath = readFile filepath >>= putStrLn . show . solve2

-- Tests
test_input1 = "aaaaa-bbb-z-y-x-123[abxyz]\na-b-c-d-e-f-g-h-987[abcde]\nnot-a-real-room-404[oarel]\ntotally-real-room-200[decoy]"

test1 :: Bool
test1 = 1514 == solve1 test_input1