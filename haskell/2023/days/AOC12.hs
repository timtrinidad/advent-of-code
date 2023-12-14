module AOC12 (day12) where
import Data.List.Split (splitOn, wordsBy)
import Common (parseInt)
import Data.List (group, intercalate)
import Debug.Trace
import Turtle.Format (x)

day12 :: (String -> String, String -> String)
day12 = (part1, part2)

-- For each record, get the number of possible arragements, and sum
part1 :: String -> String
part1 input = show $ sum $ map numArrangements $ parseInput 1 input
    

part2 :: String -> String
part2 input = show $ sum $ zipWith (curry handleRecord) [1..] $ parseInput 5 input
  where
    handleRecord (idx, record) = traceShow idx $ numArrangements record


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
expandPossibilities [] _ = [""]
expandPossibilities (currSpring:restSprings) workingGroups = do notWorking ++ working
  where
    notWorking = if currSpring == '#' then [] else map ('.':) $ expandPossibilities restSprings workingGroups
    working = expandWorkingPossibilities (currSpring:restSprings) workingGroups

expandWorkingPossibilities _ [] = []
expandWorkingPossibilities diagram (currWorkingGroup:restWorkingGroups)
  | workingGroupIsValid currWorkingGroup diagram = map (workingGroupString ++) substring
  |  otherwise = []
  where
    workingGroupString = replicate currWorkingGroup '#'
    workingGroupIsValid numWorking diagram' =  nextCharsAreAvalid numWorking diagram' && hasEnoughCharsLeft numWorking diagram'
    nextCharsAreAvalid numWorking diagram' = all (`elem` ['#', '?']) $ take numWorking diagram'
    hasEnoughCharsLeft numWorking diagram' = numWorking <= length diagram'
    substring = expandPossibilities (drop currWorkingGroup diagram) restWorkingGroups

parseInput :: Int -> String -> [(String, [Int])]
parseInput multiplier = map processLine . lines
  where
    processLine = parseNums . splitOn " " -- split by space
    parseNums [slots, nums] = (intercalate "?" $ replicate multiplier slots, concat $ replicate multiplier $ map parseInt $ splitOn "," nums) -- convert right side to list of ints
