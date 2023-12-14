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
<<<<<<< Updated upstream
part1 input = show $ sum $ map numArrangements records 
=======
part1 input = show $ sum $ map numArrangements $ parseInput 1 input
>>>>>>> Stashed changes
  where
    

part2 :: String -> String
<<<<<<< Updated upstream
part2 input = show $ sum $ map handleRecord $ zip [1..] records
  where 
    handleRecord (idx, record) = traceShow idx $ numArrangements record
    records = parseInput 5 input
    -- 4 - 4.7s
=======
part2 input = show $ sum $ zipWith (curry handleRecord) [1..] $ parseInput 5 input
  where
    handleRecord (idx, record) = traceShow idx $ numArrangements record
>>>>>>> Stashed changes


-- For a given diagram and list of working groups, count the total number of valid arrangements
numArrangements (diagram, workingGroups) =
      length
<<<<<<< Updated upstream
      $ filter (==(workingGroups record)) -- filter to ones that match the nums for this record
=======
      -- $ traceShow filteredOut
      $ filter (==workingGroups) -- filter to ones that match the nums for this record
>>>>>>> Stashed changes
      $ map convertToNums allPossibilities -- convert to a number record
  where
    allPossibilities = expandPossibilities diagram workingGroups -- generatate all possible combinations
    convertToNums = map length . wordsBy (=='.') -- remove periods and count each group of '#'
<<<<<<< Updated upstream
=======

>>>>>>> Stashed changes

-- Recursively generate possibilties, converting each ? into two possible strings (# or .)
expandPossibilities :: String -> [Int] -> [String]
expandPossibilities [] _ = [""]
expandPossibilities (currSpring:restSprings) workingGroups = do notWorking ++ working
  where
<<<<<<< Updated upstream
    expansionBroken = if isValid record $ reverse optionBroken then expandPossibilities record xs optionBroken else []
    optionBroken = '.':partialDiagram
    expansionWorking = if isValid record $ reverse optionWorking then expandPossibilities record xs optionWorking else []
    optionWorking = '#':partialDiagram
expandPossibilities record (x:xs) partialDiagram = expandPossibilities record xs (x:partialDiagram)

isValid record partialDiagram = -- traceShow (diagram record) $ traceShow (partialDiagram)
    and [
        notTooManyBroken, 
        notTooManyWorking, 
        matchesWorkingGroups, 
        notTooManyWorkingGroups,
        notTooManyWorkingGroupsLeft,
        lastGroupNotTooLarge
      ]
  where
    notTooManyWorkingGroupsLeft = length partialDiagram + toBeAllocated <= numTotal record
    toBeAllocated = sum workingGroupsLeft + (length workingGroupsLeft - 1)
    workingGroupsLeft = drop (length partialWorkingGroups + 1) workingGroups'
    notTooManyWorkingGroups = length partialWorkingGroups <= numWorkingGroups record
    notTooManyBroken = (numBroken record) >= (length $ filter (=='.') partialDiagram)
    notTooManyWorking = (numWorking record) >= (length $ filter (=='#') partialDiagram)
    lastGroupNotTooLarge = if last partialDiagram == '.' then True else (length (takeWhile (=='#') $ reverse partialDiagram)) <= (head $ drop (length partialWorkingGroups) workingGroups')
    matchesWorkingGroups = and $ zipWith (==) workingGroups' partialWorkingGroups
    workingGroups' = workingGroups record
    partialWorkingGroups = (if last partialDiagram == '.' then id else init) $ map length $ wordsBy (=='.') partialDiagram
=======
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
>>>>>>> Stashed changes

parseInput :: Int -> String -> [(String, [Int])]
parseInput multiplier = map processLine . lines
<<<<<<< Updated upstream
  where processLine = toRecord multiplier . splitOn " " -- split by space

toRecord :: Int -> [String] -> Record
toRecord multiplier [diagramOriginal, nums] = Record {
  diagram = diagram',
  workingGroups = workingGroups',
  numWorkingGroups = length workingGroups',
  numWorking = numWorking',
  numTotal = length diagram',
  numBroken = length diagram' - numWorking',
  numKnownBroken = length $ filter (=='.') diagram'
} where 
    diagram' = intercalate "?" $ replicate multiplier diagramOriginal
    workingGroups' = concat $ replicate multiplier $ map parseInt $ splitOn "," nums
    numWorking' = sum workingGroups'
=======
  where
    processLine = parseNums . splitOn " " -- split by space
    parseNums [slots, nums] = (intercalate "?" $ replicate multiplier slots, concat $ replicate multiplier $ map parseInt $ splitOn "," nums) -- convert right side to list of ints
>>>>>>> Stashed changes
