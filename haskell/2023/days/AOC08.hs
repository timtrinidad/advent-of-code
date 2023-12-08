module Days.AOC08 (day08) where

import Data.List (foldl1)
import Data.List.Split (splitOn, splitOneOf)
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

day08 = (part1, part2)

-- Traverse the map recursively starting with "AAA"
part1 input = show $ traverseMap mapping dir "AAA" 0
  where
    (dir, mapping) = parseInput input

-- Find the least common multiple of the number of steps it takes
-- to get from each staring point (anything ending with 'A') to
-- their corresponding ending point (ending with 'Z')
part2 input = show $ foldl1 lcm indivNumsToEnd
  where
    indivNumsToEnd = map (\x -> traverseMap mapping dir x 0) startingLocs -- each starting piont's # steps
    startingLocs = filter (endsIn 'A') $ Map.keys mapping -- Find points ending with 'A'
    (dir, mapping) = parseInput input

-- Traverse the map iterating through each direction until we get to "ZZZ"
traverseMap mapping (currDir:restDir) currLoc numMoves
  | endsIn 'Z' currLoc = numMoves -- base case
  | otherwise = traverseMap mapping dirs nextLoc (numMoves+1) -- recurse
  where
    dirs = (restDir ++ [currDir]) -- append current dir back to the end of the list
    nextLoc = (if currDir == 'R' then snd else fst) currToNextLoc -- pick second direction if 'R' otherwise first direction
    currToNextLoc = fromJust $ Map.lookup currLoc mapping -- mapping lookup - throw error if not found - shouldn't happen

-- Return T/F if the string ends in the given letter
endsIn letter str = last str == letter

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