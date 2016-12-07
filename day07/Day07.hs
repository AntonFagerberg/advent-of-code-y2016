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

solve1 :: String -> Bool
solve1 = validate1 . divide

result1 :: FilePath -> IO ()
result1 filepath = readFile filepath >>= putStrLn . show . length . filter id . fmap solve1 . lines

-- Part 2
aba :: String -> [String]
aba (_:_:"") = []
aba (a:b:c:rest)
  | a == c && a /= b = [b, a, b] : next
  | otherwise = next
  where next = aba (b:c:rest)

bab :: [String] -> String -> Bool
bab _ (_:_:"") = False
bab valid (a:b:c:rest) = any ((==) [a, b, c]) valid || bab valid (b:c:rest)

validate2 :: ([String], [String]) -> Bool
validate2 (supernet, hypernet) = any (bab (supernet >>= aba)) hypernet

solve2 :: String -> Bool
solve2 = validate2 . divide

result2 :: FilePath -> IO ()
result2 filepath = readFile filepath >>= putStrLn . show . length . filter id . fmap solve2 . lines

-- Tests
test1 = True == solve1 "abba[mnop]qrst"
test2 = False == solve1 "abcd[bddb]xyyx"
test3 = False == solve1 "aaaa[qwer]tyui"
test4 = True == solve1 "ioxxoj[asdfgh]zxcvbn"

test5 = True == solve2 "aba[bab]xyz"
test6 = False == solve2 "xyx[xyx]xyx"
test7 = True == solve2 "aaa[kek]eke"
test8 = True == solve2 "zazbz[bzb]cdb"