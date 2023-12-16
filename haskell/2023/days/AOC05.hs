module AOC05 (day05) where

import Data.List (find, foldr1, foldl, sortOn, union)
import Data.List.Split (splitOn, chunksOf)
import Common (parseInt)
import Text.Regex.TDFA (getAllTextMatches, (=~))
import Data.Map.Ordered (OMap)
import qualified Data.Map.Ordered as Map
import Data.Maybe (fromJust)
import Debug.Trace (traceShow, trace)
import Data.Range

day05 :: (String -> String, String -> String)
day05 = (part1, part2)

-- For each seed number, convert to location number, and find min of all results
part1 :: String -> String
part1 input = show $ minimum $ map findNextNumber' seeds
  where findNextNumber' = findNextNumber sections
        (seeds, sections) = parseInput input

-- Find the first seed that's in the allSeeds range, using binary search
part2 :: String -> String
part2 input = show $ searchLocations allSeeds reverseSections 0 maxLocation initialStep
  where
    initialStep = 10000
    maxLocation = maximum $ map (!!1) $ last sections
    -- Reverse the order of the sections and switch the location of the to/from in each list
    reverseSections = map ( map (\[to, from, len] -> [from, to, len]) ) $ reverse sections
    -- Create a range of all seed numbers
    allSeeds = mergeRanges $ map expandSeedRange $ chunksOf 2 seeds
    expandSeedRange [x, xlen] = x +=* (x+xlen)
    (seeds, sections) = parseInput input

-- Binary search using a min max and step
searchLocations :: [Range Int] -> [[[Int]]] -> Int -> Int -> Int -> Int
searchLocations allSeeds sections minNum maxNum step = do
  -- Look through range and the given step
  let res = find (inRanges allSeeds . findNextNumber sections) [minNum,minNum+step..maxNum]
  case (step, res) of
    -- No results found at all
    (1, Nothing) -> 0
    -- Down to single step - this is the answer
    (1, Just res') -> res'
    -- Found a result - reduce the step and limit search to this last chunk
    (_, Just res') -> searchLocations allSeeds sections (res' - step) res' $ step `div` 2
    -- No results found - reduce step but keep min max
    (_, Nothing) -> searchLocations allSeeds sections minNum maxNum $ step `div` 2

-- Recursively convert from one type to another
findNextNumber :: [[[Int]]] -> Int -> Int
findNextNumber [] num = num -- base case - return current number
findNextNumber (rangeSnippets:otherSections) num = findNextNumber otherSections nextNum -- recurse with the next section
  where
        -- Look for the applicable range snippet (num within range start and end)
        rangeSnippet = find (\[_, rangeStart, rangeLength] -> num >= rangeStart && num < rangeStart + rangeLength) rangeSnippets
        nextNum = case rangeSnippet of
          Just [nextRangeStart, rangeStart, _] -> num - rangeStart + nextRangeStart -- convert to next num
          Nothing -> num -- No applicable range found - just return number


-- Parse the input by parsing the first and remaining sections separately
parseInput :: String -> ([Int], [[[Int]]])
parseInput input = (
      parseSeeds $ head $ head sections, -- Out of all the sections, send the first line to parseSeeds
      map parseSection $ tail sections -- Send each of the remaining sections through parseSection and create a Map
    )
  where sections = map lines $ splitOn "\n\n" input -- Split into Sections based on double newlines

-- Parse all the numbers in the "Seeds" section into ints
parseSeeds :: String -> [Int]
parseSeeds seeds = map parseInt $ splitOn " " $ drop 7 seeds -- ignore the "Seeds: " part

-- Parse out the ranges as a 2d list of ints
parseSection :: [String] -> [[Int]]
parseSection section = rangeSnippets
  where
        rangeSnippets = map parseRange $ tail section -- remove the first line and parse into ints
        parseRange = map parseInt . splitOn " " -- Split by space and convert to ints