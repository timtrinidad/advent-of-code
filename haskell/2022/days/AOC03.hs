module Days.AOC03 (day03) where

import Data.List.Split (chunksOf)
import Data.List (intersect)
import Data.Char (ord)

day03 = (part1, part2)

priority x
  | x >= 97 = x - 96
  | otherwise = x - 38

part1 input = do
  let parsed = parseInput input
  print $ sum $ map(
    \[x, y] ->
      priority -- convert to priority number
      $ ord -- get ascii number
      $ intersect x y !! 0 -- get first character from intersection
    ) parsed

part2 input = do
  print "part2 not defined for day 03"

-- Split each line into two equa lists
parseInput = map(\x -> chunksOf (div (length x) 2) x) . lines
