module Days.AOC04 (day04) where

import Data.List.Split (splitOneOf)

day04 = (part1, part2)

part1 input = do
  let parsed = parseInput input
  print $ length
    -- Filter by overlaps where either the second set is surrounded by the first set or vice versa
    $ filter (\[a, b, x, y] -> (a <= x && b >= y) || (x <= a && y >= b)) parsed

part2 input = do
  let parsed = parseInput input
  -- Filter where both first set are before lower of second set or after higher of second set
  let numNoOverlap = length $ filter(\[a, b, x, y] -> (a < x && b < x) || (a > y && b > y)) parsed
  print $ (length parsed) - numNoOverlap

parseInt x = read x :: Int
parseInput = map(map(parseInt) . splitOneOf ",-") . lines
