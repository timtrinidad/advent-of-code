module Days.AOC04 (day04) where

import Data.List.Split (splitOneOf, chunksOf, splitOn)
import Data.List (intersect)
import Common (parseInt)

day04 = (part1, part2)

part1 input = do
  let parsed = parseInput input
  print
    $ sum
    -- Convert to points based
    $ map (\x -> if x > 0 then 2 ^ (x-1) else 0)
    -- For each line map to # of intersections between left and right
    $ map (\[x, y] -> (length $ intersect x y)) parsed

part2 input = do
  print "part2 not defined for day 04"

-- Split each line into two lists of numbers
parseInput =
  map (
      map (
          map parseInt . chunksOf 3
        )
      . splitOn " |"
      -- Drop the "game" part
      . drop 1
      . dropWhile (/= ':')
    )
  . lines