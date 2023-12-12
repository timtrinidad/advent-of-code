module AOC08 (day08) where

import Data.List (foldl1, isSuffixOf, scanl)
import Data.List.Split (splitOn, splitOneOf)
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

day08 = (part1, part2)

-- Traverse the map recursively starting with "AAA"
part1 input = show $ traverseMap mapping dir "AAA"
  where (dir, mapping) = parseInput input

-- Find the least common multiple of the number of steps it takes
-- to get from each staring point (anything ending with 'A') to
-- their corresponding ending point (ending with 'Z')
part2 input = show
    $ foldl1 lcm
    $ map (traverseMap mapping dir) -- each starting point's # steps to '..Z'
    $ filter (isSuffixOf "A") $ Map.keys mapping -- Find points ending with 'A'
  where (dir, mapping) = parseInput input

-- Traverse the map iterating through each direction until we get to something that ends with 'Z'
traverseMap mapping dirs currLoc = length $ takeWhile (not . isSuffixOf "Z") $ scanl getNextLoc currLoc dirs
  -- pick second direction if 'R' otherwise first direction
  where getNextLoc currLoc dir = (if dir == 'R' then snd else fst) $ mapping Map.! currLoc

-- Parse the dir and mapping separately
parseInput input = (cycle dir, parseMapping mapping)
  where [dir, mapping] = splitOn "\n\n" input

-- Convert each line into a map entry (first is the key, second and third is the value as a tuple
parseMapping input = Map.fromList $ processLine <$> splitOn "\n" input
  where
     -- Split on any symbol and filter out empty strings
     processLine = toTuple . filter (not . null) . splitOneOf "=(), "
     -- Convert results to src as key and l/r as value
     toTuple [src, left, right] = (src, (left, right))