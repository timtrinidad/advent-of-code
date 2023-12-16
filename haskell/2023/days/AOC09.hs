module AOC09 (day09) where

import Data.List (zip, nub)
import Data.List.Split (splitOn)
import Common (parseInt)
import Debug.Trace

day09 :: (String -> String, String -> String)
day09 = (part1, part2)

-- Find the sum of the next number of each series
part1 :: String -> String
part1 input = show $ sum $ findNext <$> parseInput input

-- Reverse each series, find next, and sum
part2 :: String -> String
part2 input = show $ sum $ findNext . reverse <$> parseInput input

-- Find the next number in a given series
findNext :: [Int] -> Int
findNext list
  | length (nub list) == 1 = head list -- base case - all numbers in the list are the same
  | otherwise = last list + nextDiff -- the next number in the series will be the last number in the list + the diff
  where
    -- recurse - zip each item with its previous item, calculate diff, and find next number
    nextDiff = findNext $ zipWith diffs list (tail list)
    diffs prev curr = curr - prev -- Diff to previous number

-- Split input into a list of list of ints
parseInput :: String -> [[Int]]
parseInput = map processLine . lines
  where processLine = map parseInt . splitOn " "