module Day07 where

import Data.List  
  
parse :: [String] -> Char -> [String]
parse [] char = [[char]]
parse acc '[' = "" : acc
parse acc ']' = "" : acc
parse (current:rest) char = (char : current) : rest

mod2 :: (Int, String) -> Bool
mod2 (i, _) = mod i 2 == 0

unpack :: ([(Int, String)], [(Int, String)]) -> ([String], [String])
unpack (a, b) = (fmap snd a, fmap snd b)

divide :: String -> ([String], [String])
divide = unpack . partition mod2 . zip [0..] . foldl parse []

-- Part 1

abba :: String -> Bool
abba (_:_:_:"") = False
abba (a:b:c:d:rest)
  | a == d && b == c && a /= b = True
  | otherwise = abba $ b:c:d:rest

validate1 :: ([String], [String]) -> Bool
validate1 (ok, no) = any abba ok && not (any abba no)

solve1 :: String -> Int
solve1 = length . filter id . fmap (validate1 . divide) . lines

-- Part 2

aba :: String -> [String]
aba (_:_:"") = []
aba (a:b:c:rest)
  | a == c && a /= b = [b, a, b] : next
  | otherwise = next
  where next = aba (b:c:rest)

bab :: [String] -> String -> Bool
bab _ (_:_:"") = False
bab valid (a:b:c:rest) = elem [a, b, c] valid || bab valid (b:c:rest)

validate2 :: ([String], [String]) -> Bool
validate2 (supernet, hypernet) = any (bab (supernet >>= aba)) hypernet

solve2 :: String -> Int
solve2 = length . filter id . fmap (validate2 . divide) . lines