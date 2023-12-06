module Days.AOC05 (day05) where

import Data.List (find, foldr1, foldl, sortOn, union)
import Data.List.Split (splitOn, chunksOf)
import Common (parseInt)
import Text.Regex.TDFA (getAllTextMatches, (=~))
import Data.Map.Ordered (OMap)
import qualified Data.Map.Ordered as Map
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Data.Range

day05 = (part1, part2)

-- For each seed number, convert to location number, and find min of all results
part1 input = show $ minimum $ map findNextNumber' seeds
  where findNextNumber' = findNextNumber sections
        (seeds, sections) = parseInput input

-- Find the first seed that's in the allSeeds range, working backwards from locations
part2 input = show $ find (\x -> inRanges allSeeds $ findNextNumber' x )  [0..]
  where
    -- Find the next number, printing every 1000
    findNextNumber' x
          | x `mod` 1000 == 0 = trace ("Testing: " ++ show x) findNextNumber reverseSections x
          | otherwise = findNextNumber reverseSections x
    -- Reverse the order of the sections and switch the location of the to/from in each list
    reverseSections = map ( map (\[to, from, len] -> [from, to, len]) ) $ reverse sections
    -- Create a range of all seed numbers
    allSeeds = mergeRanges $ map expandSeedRange $ chunksOf 2 seeds
    expandSeedRange = \[x, xlen] -> (x +=* (x+xlen))
    (seeds, sections) = parseInput input


-- Recursively convert from one type to another
findNextNumber [] num = num -- base case - return current number
findNextNumber (rangeSnippets:otherSections) num = findNextNumber otherSections nextNum -- recurse with the next section
  where
        -- Look for the applicable range snippet (num within range start and end)
        rangeSnippet = find (\[_, rangeStart, rangeLength] -> num >= rangeStart && num < rangeStart + rangeLength) rangeSnippets
        nextNum = case rangeSnippet of
          Just [nextRangeStart, rangeStart, rangeLength] -> num - rangeStart + nextRangeStart -- convert to next num
          Nothing -> num -- No applicable range found - just return number


-- Parse the input by parsing the first and remaining sections separately
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