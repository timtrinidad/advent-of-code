module AOC12 (day12) where
import Data.List.Split (splitOn, wordsBy)
import Common (parseInt)
import Data.List (group, intercalate)
import Data.MemoTrie (memo2)
import Debug.Trace

day12 :: (String -> String, String -> String)
day12 = (part1, part2)

-- For each record, get the number of possible arragements, and sum
part1 :: String -> String
part1 input = show $ sum $ map numArrangements $ parseInput 1 input


part2 :: String -> String
part2 input = show $ sum $ map numArrangements $ parseInput 5 input

-- For a given diagram and list of working groups, count the total number of valid arrangements
numArrangements :: (String, [Int]) -> Int
numArrangements (diagram, workingGroups) = allPossibilities
  where
    allPossibilities = expandPossibilitiesMemo diagram workingGroups -- generatate all possible combinations

-- Memoized version of expandPossibilities
expandPossibilitiesMemo :: String -> [Int] -> Int
expandPossibilitiesMemo = memo2 expandPossibilities

-- Recursively generate possibilties, converting each ? into two possible strings (# or .)
expandPossibilities :: String -> [Int] -> Int
expandPossibilities [] [] = 1 -- base case - no more characters to process and no more working groups
expandPossibilities [] _ = 0 -- no more characters to process but still have some working groups left - invalid
expandPossibilities (currSpring:restSprings) workingGroups = 
  notWorking + working
  where
    -- Figure out what options there are to set this next character to '.'
    notWorking = if currSpring `elem` "?." then expandPossibilitiesMemo restSprings workingGroups else 0
    -- Figure out what options there are to set the next few characters to '#' based on the next working group number
    working = if currSpring `elem` "?#" then expandWorkingPossibilities (currSpring:restSprings) workingGroups else 0

expandWorkingPossibilities :: String -> [Int] -> Int
expandWorkingPossibilities _ [] = 0
expandWorkingPossibilities diagram (currWorkingGroup:restWorkingGroups)
  -- Is valid - Recurse, skipping the next n chars based on what we added just now
  | workingGroupIsValid workingGroupString diagram = expandPossibilitiesMemo (drop (length workingGroupString) diagram) restWorkingGroups
  -- Proposed working group is not valid - short circuit recursion
  | otherwise = 0
  where
    workingGroupString = replicate currWorkingGroup '#'
      ++ (['.' | length diagram > currWorkingGroup]) -- If we're not at the end, append a spacer ('.')

-- Determine if the proposed string for this working group is valid based
-- on the given diagram
workingGroupIsValid :: String -> String -> Bool
workingGroupIsValid workingGroupString diagram = 
  (length workingGroupString <= length diagram) -- diagram is long enough
  && and (zipWith (\a b -> b `elem` [a, '?']) workingGroupString diagram) -- diagram matches each charachter or has '?'

parseInput :: Int -> String -> [(String, [Int])]
parseInput multiplier = map processLine . lines
  where
    processLine = parseNums . splitOn " " -- split by space
    parseNums [slots, nums] = (intercalate "?" $ replicate multiplier slots, concat $ replicate multiplier $ map parseInt $ splitOn "," nums) -- convert right side to list of ints
