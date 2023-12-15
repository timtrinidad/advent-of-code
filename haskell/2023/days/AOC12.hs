module AOC12 (day12) where
import Data.List.Split (splitOn, wordsBy)
import Common (parseInt)
import Data.List (group, intercalate)
import Debug.Trace

day12 :: (String -> String, String -> String)
day12 = (part1, part2)

-- For each record, get the number of possible arragements, and sum
part1 :: String -> String
part1 input = show $ sum $ map numArrangements $ parseInput 1 input


part2 :: String -> String
part2 input = show $ sum $ zipWith handleRecord [1..] $ parseInput 4 input
  where
    handleRecord :: Int -> (String, [Int]) -> Int
    handleRecord idx record = traceShow idx $ numArrangements record


-- For a given diagram and list of working groups, count the total number of valid arrangements
numArrangements (diagram, workingGroups) =
      length
      -- $ traceShow filteredOut
      $ filter (==workingGroups) -- filter to ones that match the nums for this record
      $ map convertToNums allPossibilities -- convert to a number record
  where
    allPossibilities = expandPossibilities diagram workingGroups -- generatate all possible combinations
    convertToNums = map length . wordsBy (=='.') -- remove periods and count each group of '#'

-- Recursively generate possibilties, converting each ? into two possible strings (# or .)
expandPossibilities :: String -> [Int] -> [String]
expandPossibilities [] _ = [""] -- base case - no more characters to process
expandPossibilities (currSpring:restSprings) workingGroups = do notWorking ++ working
  where
    -- Figure out what options there are to set this next character to '.'
    notWorking = if currSpring == '#' then [] else map ('.':) $ expandPossibilities restSprings workingGroups
    -- Figure out what options there are to set the next few characters to '#' based on the next working group number
    working = if currSpring == '.' then [] else expandWorkingPossibilities (currSpring:restSprings) workingGroups

expandWorkingPossibilities _ [] = []
expandWorkingPossibilities diagram (currWorkingGroup:restWorkingGroups)
  -- Is valid - prepend the proposed string to each of the generated downsteam options
  | workingGroupIsValid workingGroupString diagram = map (workingGroupString ++) substring
  -- Proposed working group is not valid - short circuit recursion
  | otherwise = []
  where
    -- Recurse, skipping the next n chars based on what we added just now
    substring = expandPossibilities (drop (length workingGroupString) diagram) restWorkingGroups
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
