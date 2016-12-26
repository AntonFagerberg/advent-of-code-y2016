module Day08 where
  
import Text.Regex.PCRE
import Data.List
  
merge :: [String] -> [String] -> [String]
merge [a] b = take (length a) $ b ++ [a]
merge a [] = a
merge (a:as) (b:bs) = new_line : merge as bs where new_line = b ++ drop (length b) a
  
create :: Char -> Int -> Int -> [String]
create char cols rows = (replicate rows . replicate cols) char

monitor :: Int -> Int -> [String]
monitor = create ' '

rect :: Int -> Int -> [String] -> [String]
rect cols rows matrix = merge matrix $ create '#' cols rows

rotateRow :: Int -> Int -> [String] -> [String]
rotateRow index offset matrix = before ++ (rotated : after)
  where
    before = take index matrix
    rotated = reverse . take (length row) . drop offset . concat . repeat . reverse $ row
    row = matrix !! index
    after = drop (index + 1) matrix
  
rotateCol :: Int -> Int -> [String] -> [String]
rotateCol index offset = transpose . rotateRow index offset . transpose
  
parse :: String -> (String, String, Int, Int)
parse text = (command, sub_command, read x, read y)
  where [[_, command, sub_command, x, y]] = text =~ "^([a-z]+) ([a-z]*)[a-z= ]*(\\d+)[a-z= ]*(\\d+)" :: [[String]]

command :: [String] -> (String, String, Int, Int) -> [String]
command matrix ("rect", _, cols, rows) = rect cols rows matrix
command matrix ("rotate", "row", index, offset) = rotateRow index offset matrix
command matrix ("rotate", "column", index, offset) = rotateCol index offset matrix

process :: [String] -> String -> [String]
process matrix = foldl command matrix . fmap parse . lines

-- Part 1

solve1 :: String -> Int
solve1 = length . filter ('#' ==) . concat . process (monitor 50 6)

result1 :: FilePath -> IO ()
result1 filepath = readFile filepath >>= print . solve1

-- Part 2

solve2 :: String -> String
solve2 = unlines . process (create ' ' 50 6)

result2 :: FilePath -> IO ()
result2 filepath = readFile filepath >>= putStrLn . solve2