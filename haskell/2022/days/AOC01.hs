module Days.AOC01 (day01) where

import Data.List
import Data.List.Split
import Control.Concurrent



day01 = (part1, part2)

part1 input = do
  let parsed = parse_input input
  print $ maximum $ map sum parsed

part2 input = do
  print "not implemented"

parse_int = read::String->Int
parse_input = map (map parse_int . lines) . splitOn "\n\n"
