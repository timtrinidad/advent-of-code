module Days.AOC05 (day05) where

import Data.List (find)
import Data.List.Split (splitOn)
import Common (parseInt)
import Text.Regex.TDFA (getAllTextMatches, (=~))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Debug.Trace (trace)

day05 = (part1, part2)

-- For each seed number, convert to location number, and find min of all results
part1 input = print $ minimum $ map (findLocation sectionsMap "seed") seeds
  where (seeds, sectionsMap) = parseInput input

part2 input = do
  print "part2 not defined for day 05"

-- Recursively convert from one type to another
findLocation _ "location" num = num -- base case - return current number
findLocation sectionMap srcName srcNum = findLocation sectionMap dstName dstNum -- recurst with new destination info
  where
        (dstName, rangeSnippets) = fromJust $ Map.lookup srcName sectionMap -- Lookup the section info based on the src name
        -- Look for the applicable range snippet (src num within src start and src end)
        rangeSnippet = find (\[_, srcStart, rangeLength] -> srcNum >= srcStart && srcNum < srcStart + rangeLength) rangeSnippets
        dstNum = case rangeSnippet of
          Just [dstStart, srcStart, rangeLength] -> srcNum - srcStart + dstStart -- Convert from src to dest
          Nothing -> srcNum -- No applicable range found - just return number


-- Parse the input by parsing the first and remaining sections separately
parseInput input = (
      parseSeeds $ head $ head sections, -- Out of all the sections, send the first line to parseSeeds
      Map.fromList $ map parseSection $ tail sections -- Send each of the remaining sections through parseSection and create a Map
    )
  where sections = map lines $ splitOn "\n\n" input -- Split into Sections based on double newlines

-- Parse all the numbers in the "Seeds" section into ints
parseSeeds :: String -> [Int]
parseSeeds seeds = map parseInt $ splitOn " " $ drop 7 seeds -- ignore the "Seeds: " part

-- For a given section, create a mapping from the source name to its dest name along with the integer range snippets
-- For example, from
--   water-to-light map:
--   88 18 7
--   18 25 70
-- To
--   ("water", ("light", [[88, 18, 7], [18, 25, 70]]))
parseSection :: [String] -> (String, (String, [[Int]]))
parseSection section = (srcName, (dstName, rangeSnippets))
  where
        rangeSnippets = map parseRange $ tail section -- remove the first line and parse into ints
        parseRange = map parseInt . splitOn " " -- Split by space and convert to ints
        srcName = sectionTitle !! 0 -- first word
        dstName = sectionTitle !! 2 -- third word
        sectionTitle = getAllTextMatches (head section =~ "[[:lower:]]+") :: [String] -- capture all the words in the first line