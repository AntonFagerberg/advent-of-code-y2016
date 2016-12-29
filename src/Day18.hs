module Day18 where

check :: String -> Int -> Char
check input index
  | left && center && not right = '^'
  | not left && center && right = '^'
  | left && not center && not right = '^'
  | not left && not center && right = '^'
  | otherwise = '.'
  where
    left
      | index > 0 = input !! (index - 1) == '^'
      | otherwise = False
    center
      | index >= 0 && index < length input = input !! index == '^'
      | otherwise = False
    right
      | index + 1 < length input = input !! (index + 1)  == '^'
      | otherwise = False

generate :: String -> String
generate input = fmap (check input) [0 .. length input - 1]

solve1 :: Int -> String -> Int
solve1 size = sum . fmap (length . filter ('.' ==)) . take size . iterate generate

solve2 :: Int -> String -> Int
solve2 size = sum . fmap (length . filter ('.' ==)) . take size . iterate generate