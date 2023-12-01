module Days.AOC01 (day01) where

import Data.Char

day01 = (part1, part2)

part1 input = do
  let parsed = parseInput input
  print $ sum $ map(\x -> parseInt [head x, last x]) parsed

part2 input = do
  print "part2 not defined for day 01"

parseInt = read::String->Int
isInt x = ord x >= 48 && ord x <= 57
parseInput = map(filter isInt) . lines
