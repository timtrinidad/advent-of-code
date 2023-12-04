module Days.AOC04 (day04) where

import Data.List.Split (splitOneOf, chunksOf, splitOn)
import Data.List (intersect)
import Common (parseInt)
import Data.Map (Map)
import qualified Data.Map as Map

day04 = (part1, part2)

part1 input = do
  let parsed = parseInput input
  print
    $ sum
    $ map (numMatchesToPoints . getNumMatches) parsed

part2 input = do
  let parsed = parseInput input
  -- Number of matches for each card
  let numMatches = zip [1..] $ map getNumMatches parsed
  -- Initialize a map with key cardNum and val 1 (starting with 1 of every card)
  let numCards = Map.fromList(map (\x -> (x, 1)) [1..(length parsed)])
  -- Recursively count cards and sum the total values
  print $ sum $  Map.elems $ countCards numMatches numCards

-- Process each card and update the card counter
countCards [] acc = acc -- base case
countCards ((idx, numMatches):cardsToProcess) acc = do
  -- Get the number of the current card
  let numCurrCard = Map.findWithDefault 1 idx acc
  -- Recurse with remaining cards
  countCards cardsToProcess
    -- Based on the number of matches, update the counter of the subsequent cards by the number of current cards
    $ foldr (\idx' acc2 -> Map.adjust (numCurrCard+) idx' acc2) acc [idx+1..idx+numMatches]

-- Convert to points based
numMatchesToPoints x = if x > 0 then 2 ^ (x-1) else 0
-- For ea®ch line map to # of intersections between left and right
getNumMatches [left, right] = length $ intersect left right

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