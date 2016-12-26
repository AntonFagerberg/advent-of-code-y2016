module Day16 where

switch :: Char -> Char
switch '0' = '1'
switch '1' = '0'

generate :: String -> String
generate original = original ++ '0' : flipped
  where flipped = fmap switch . reverse $ original
  
partialChecksum :: String -> String
partialChecksum [] = []
partialChecksum (a:b:rest)
  | a == b = '1' : partialChecksum rest
  | otherwise = '0' : partialChecksum rest

checksum :: String -> String
checksum = head . filter (odd . length) . iterate partialChecksum

solve :: Int -> String -> String
solve size = checksum . take size . head . filter ((<) size . length) . iterate generate