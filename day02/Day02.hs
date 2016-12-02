module Day01 where

move :: Char -> (Int, Int)
move 'U' = (-1,  0)
move 'D' = ( 1,  0)
move 'L' = ( 0, -1)
move 'R' = ( 0,  1)

step :: [[Char]] -> (Int, Int) -> [(Int, Int)] -> (Int, Int)
step _ xy [] = xy
step pad (x, y) ((dx, dy):rest)
  | pad !! ny !! nx == '-' = step pad (x, y) rest
  |              otherwise = step pad(nx, ny) rest
  where (nx, ny) = (x + dx, y + dy)

key :: [[Char]] -> (Int, Int) -> Char
key pad (x, y) = pad !! x !! y

parse :: String -> [[(Int, Int)]]
parse = fmap (fmap move) . lines

solve :: [[Char]] -> (Int, Int) -> String -> [Char]
solve pad xy = fmap (key pad) . tail . scanl (step pad) xy . parse

-- Part 1

pad1 :: [[Char]]
pad1 = 
  [['-','-','-','-','-'],
   ['-','1','2','3','-'],
   ['-','4','5','6','-'],
   ['-','7','8','9','-'],
   ['-','-','-','-','-']]

solve1 :: String -> [Char]
solve1 = solve pad1 (2, 2)

result1 :: FilePath -> IO ()
result1 filepath = readFile filepath >>= putStrLn . solve1

-- Part 2

pad2 :: [[Char]]
pad2 = 
  [['-','-','-','-','-','-','-'],
   ['-','-','-','1','-','-','-'],
   ['-','-','2','3','4','-','-'],
   ['-','5','6','7','8','9','-'],
   ['-','-','A','B','C','-','-'],
   ['-','-','-','D','-','-','-'],
   ['-','-','-','-','-','-','-']]

solve2 :: String -> [Char]
solve2 = solve pad2 (3, 1)

result2 :: FilePath -> IO ()
result2 filepath = readFile filepath >>= putStrLn . solve2

-- Test
test_input = "ULL\nRRDDD\nLURDL\nUUUUD"
test1 = "1985" == solve1 test_input
test2 = "5DB3" == solve2 test_input