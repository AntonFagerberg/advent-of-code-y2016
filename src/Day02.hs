module Day02 where

move :: Char -> (Int, Int)
move 'U' = (-1,  0)
move 'D' = ( 1,  0)
move 'L' = ( 0, -1)
move 'R' = ( 0,  1)

step :: [String] -> (Int, Int) -> [(Int, Int)] -> (Int, Int)
step _ xy [] = xy
step pad (x, y) ((dx, dy):rest)
  | pad !! ny !! nx == '-' = step pad (x, y) rest
  |              otherwise = step pad(nx, ny) rest
  where (nx, ny) = (x + dx, y + dy)

key :: [String] -> (Int, Int) -> Char
key pad (x, y) = pad !! x !! y

parse :: String -> [[(Int, Int)]]
parse = fmap (fmap move) . lines

solve :: [String] -> (Int, Int) -> String -> String
solve pad xy = fmap (key pad) . tail . scanl (step pad) xy . parse

-- Part 1

pad1 :: [String]
pad1 = 
  [['-','-','-','-','-'],
   ['-','1','2','3','-'],
   ['-','4','5','6','-'],
   ['-','7','8','9','-'],
   ['-','-','-','-','-']]

solve1 :: String -> String
solve1 = solve pad1 (2, 2)

-- Part 2

pad2 :: [String]
pad2 = 
  [['-','-','-','-','-','-','-'],
   ['-','-','-','1','-','-','-'],
   ['-','-','2','3','4','-','-'],
   ['-','5','6','7','8','9','-'],
   ['-','-','A','B','C','-','-'],
   ['-','-','-','D','-','-','-'],
   ['-','-','-','-','-','-','-']]

solve2 :: String -> String
solve2 = solve pad2 (3, 1)
