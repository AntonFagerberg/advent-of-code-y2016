module Day14 where

import Text.Regex.PCRE
import Crypto.Hash
import Crypto.Hash.Algorithms
import qualified Data.ByteString.Char8 as B

md5 :: B.ByteString -> Digest MD5
md5 = hash

md5String :: String -> String
md5String = show . md5 . B.pack

md5HashKeyIndex :: (Int, String) -> String
md5HashKeyIndex (index, key) = md5String $ key ++ show index

hashes :: String -> [String]
hashes key = fmap md5HashKeyIndex . zip [0..] $ repeat key 

stretchedHashes :: String -> [String]
stretchedHashes key = fmap (!! 2016) hashList
  where hashList = iterate md5String <$> hashes key

findRepeating :: Int -> String -> [[String]]
findRepeating len text = text =~ ("(\\w)\\1{" ++ show (len - 1) ++ "}") :: [[String]]

hasRepeatingKey :: String -> String -> Bool
hasRepeatingKey target md5String  = target `elem` results
  where results = fmap head . findRepeating (length target) $ md5String

findKeys :: Int -> [(Int, String)] -> Int
findKeys count ((index, current):rest)
  | count == 1 && valid = index
  | valid = findKeys (count - 1) rest
  | otherwise = findKeys count rest
  where 
    matches = findRepeating 3 current    
    target = take 5 . concat . repeat . head . head $ matches
    searchSpace = fmap snd . take 1000 $ rest
    valid = (not . null) matches && any (hasRepeatingKey target) searchSpace

solve1 :: String -> Int
solve1 = findKeys 64 . zip [0..] . hashes

solve2 :: String -> Int
solve2 = findKeys 64 . zip [0..] . stretchedHashes