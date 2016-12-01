module Day01 where

import Text.Regex.PCRE

format :: [String] -> (String, Int)
format [_, dir, num] = (dir, read num :: Int)

split :: String -> [[String]]
split text = text =~ "(L|R)(\\d+)" :: [[String]]

move :: (String, Int) -> ((Int, Int), Int) -> (Int, Int)
move ("R", nr) ((x, y), 0) = (x + nr, y     )
move ("R", nr) ((x, y), 1) = (x     , y - nr)
move ("R", nr) ((x, y), 2) = (x - nr, y     )
move ("R", nr) ((x, y), 3) = (x     , y + nr)
move ("L", nr) ((x, y), i) = move ("R", nr) ((x, y), (mod (i + 2) 4))

direction ("R", _) dir = mod (dir + 1) 4
direction ("L", _) dir = mod (dir - 1) 4

walk :: ((Int, Int), Int) -> (String, Int) -> ((Int, Int), Int)
walk command@(_, dir) pos = ((move pos command), direction pos dir)

blocks (x, y) = abs x + abs y

solve1 :: String -> Int
solve1 s = blocks . fst . foldl walk ((0, 0), 0) $ fmap format $ split s

result1 file = do input <- readFile file
                  putStrLn . show . solve1 $ input

tests = test1 && test2 && test3
test1 = 5 == solve1 "R2, L3"
test2 = 2 == solve1 "R2, R2, R2"
test3 = 12 == solve1 "R5, L5, R5, R3"

slide :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
slide [_] = []
slide (x:xx:xs) = (x, xx) : (slide (xx:xs))

step ((x1, y1), (x2, y2)) = [(x, y) | x <- [x1..x2], y <- [y1..y2]]

fixStep s@((x1, y1), (x2, y2))
  | x1 > x2 = reverse $ step ((x2, y1), (x1, y2))
  | y1 > y2 = reverse $ step ((x1, y2), (x2, y1))
  | otherwise = step s
  

duplicate :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int)
duplicate visited (xy:rest) 
  | elem xy visited = xy
  | otherwise = duplicate (xy:visited) rest

hack (x:xs) = x : map tail xs

solve2 s = hack . map fixStep . slide . map fst . scanl walk ((0, 0), 0) $ fmap format $ split s

solve3 s = map fst . scanl walk ((0, 0), 0) $ fmap format $ split s

result2 file = do input <- readFile file
                  putStrLn . show . blocks . duplicate [] $ solve2 input >>= id