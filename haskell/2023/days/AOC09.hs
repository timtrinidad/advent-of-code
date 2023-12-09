module Days.AOC09 (day09) where

import Data.List (zip, nub)
import Data.List.Split (splitOn)
import Common (parseInt)
import Debug.Trace

day09 = (part1, part2)

part1 input = show $ sum $ map findNext $ parseInput input

part2 input = do
  show "part2 not defined for day 09"

findNext list
  | (length $ nub list) == 1 = head list
  | otherwise = last list + nextNum
  where
    diffs (a, b) = b - a
    nextNum = findNext $ map diffs $ zip list $ tail list

parseInput = map processLine . lines
  where processLine = map parseInt . splitOn " "