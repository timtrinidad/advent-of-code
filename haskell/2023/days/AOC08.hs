module Days.AOC08 (day08) where

import Data.List.Split (splitOn, splitOneOf)
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map

day08 = (part1, part2)

part1 input = show $ traverseMap mapping dir "AAA" 0
  where
    (dir, mapping) = parseInput input

-- Traverse the map iterating through each direction until we get to "ZZZ"
traverseMap _ _ "ZZZ" numMoves = numMoves -- base case
traverseMap mapping (currDir:restDir) currLoc numMoves = traverseMap mapping dirs nextLoc (numMoves+1)
  where
    dirs = (restDir ++ [currDir]) -- append current dir back to the end of the list
    nextLoc = (if currDir == 'R' then snd else fst) currToNextLoc -- pick second direction if 'R' otherwise first direction
    currToNextLoc = fromJust $ Map.lookup currLoc mapping -- mapping lookup - throw error if not found - shouldn't happen


part2 input = do
  show "part2 not defined for day 08"

-- Parse the dir and mapping separately
parseInput input = (dir, parseMapping mapping)
  where
    [dir, mapping] = splitOn "\n\n" input

-- Convert each line into a map entry (first is the key, second and third is the value as a tuple
parseMapping input = Map.fromList $ processLine <$> splitOn "\n" input
  where
     -- Split on any symbol and filter out empty strings
     processLine = toTuple . filter (not . null) . splitOneOf "=(), "
     -- Convert results to src as key and l/r as value
     toTuple [src, left, right] = (src, (left, right))