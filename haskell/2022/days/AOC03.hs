module Days.AOC03 (day03) where

import Data.List.Split (chunksOf)
import Data.List (intersect)
import Data.Char (ord)

day03 = (part1, part2)

part1 input = do
  -- Split each lint into two equal lines
  let parsed = map(\x -> chunksOf (div (length x) 2) x) $ lines input -- Split into each line into halves
  print $ intersectionPrioritySum $ parsed

part2 input = do
  -- Split into chunks of 3 lines
  let parsed = chunksOf 3 $ lines input
  print $ intersectionPrioritySum $ parsed

-- Given a list of lists, find intersection, convert to priority, and sum priorities
intersectionPrioritySum = sum . map(intersectionToPriority . foldr1 intersect)

-- Converts a list like "ZZZ" to 52
intersectionToPriority =
  priority -- convert to priority
  . ord -- get ASCII code
  . head -- only first item in list

priority x
  | x >= 97 = x - 96
  | otherwise = x - 38

