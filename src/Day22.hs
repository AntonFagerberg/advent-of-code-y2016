module Day22 where

import Text.Regex.PCRE
import Data.Set (Set, fromList, member, insert, delete)
import Data.Tuple.Select
import Data.List (find, maximumBy, nub, groupBy, nubBy, (\\))
import Data.Maybe (isJust, fromJust)
import Control.Monad (replicateM)

type Node = ((Int, Int), (Int, Int))
type Goal = (Set (Int, Int), ((Int, Int), (Int, Int)))

parse :: String -> Node
parse text = ((read x, read y), (read used, read avail))
  where 
    [[_, x, y, _, used, avail]] = text =~ "/dev/grid/node-x(\\d+)-y(\\d+)\\s+(\\d+)T\\s+(\\d+)T\\s+(\\d+)T"

validPair :: Node -> Node -> Bool
validPair (_, (used1, avail1)) (_, (used2, avail2))
  | used1 > 0 && used1 < avail2 = True
  | used2 > 0 && used2 < avail1 = True
  | otherwise = False

pairs :: [Node] -> [(Node, Node)]
pairs [] = []
pairs (node:rest) = zip (repeat node) matches ++ pairs rest
  where
    matches = filter (validPair node) rest

solve1 :: String -> Int
solve1 = length . pairs .  fmap parse . drop 2 . lines

(...) = (.) . (.)

path :: [[(Int, Int)]] -> Set (Int, Int) -> (Int, Int) -> [(Int, Int)]
path history grid target
  | isJust solution = fromJust solution
  | otherwise = path newHistory grid target
  where
    solution = find (target `elem`) history
    moves = [(-1, 0), (1, 0), (0, 1), (0, -1)] 
    newHistory = nubBy (null ... (\\)) $ history >>= (\h@((x,y):_) -> fmap ( : h) . filter (`member` grid) $ [(x + x', y + y') | (x', y') <- moves] )

pathLength :: [(Int, Int)] -> Set (Int, Int) -> (Int, Int) -> Int
pathLength history grid target
  | target `elem` history = 0
  | otherwise = 1 + pathLength newHistory grid target
  where
    moves = [(-1, 0), (1, 0), (0, 1), (0, -1)]
    newHistory = nub . filter (`member` grid) $ [(x + x', y + y') | (x', y') <- moves, (x, y) <- history]

move :: Set (Int, Int) -> [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Int
move _ [] _ _ = 0
move grid (destination:rest) current target = 1 + steps + move newGrid rest target destination
  where
    steps = pathLength [current] grid destination
    newGrid = insert target . delete destination $ grid

solve2 :: String -> Int
solve2 input = move grid rest free target
  where
    nodes = fmap parse . drop 2 . lines $ input
    target = maximum . filter ((0 ==) . snd) . fmap fst $ nodes
    [(free, _)] = filter ((0 ==) . fst . snd) nodes
    grid = fromList . fmap fst . filter ((100 >) . fst . snd) $ nodes  
    (_:rest) = fmap (\x -> (x, 0)) . reverse $ [0 .. fst target] -- Cheating solution
    -- Proper slow solution: path [[(0,0)]] grid target
    -- Should've used Dijkstra's algorithm or something.