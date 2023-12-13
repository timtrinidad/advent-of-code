module AOC12 (day12) where
import Common (parseInt)
import Data.List (group, intercalate)
import Data.List.Split (splitOn, wordsBy)
import Debug.Trace

day12 :: (String -> String, String -> String)
day12 = (part1, part2)

data Record = Record {
  diagram :: String,
  workingGroups :: [Int],
  numWorkingGroups :: Int,
  numTotal :: Int,
  numWorking :: Int,
  numKnownBroken :: Int,
  numBroken :: Int
} deriving (Show)

-- For each record, get the number of possible arragements, and sum
part1 :: String -> String
part1 input = show $ sum $ map numArrangements records 
  where
    records = parseInput 1 input

part2 :: String -> String
part2 input = show $ sum $ map handleRecord $ zip [1..] records
  where 
    handleRecord (idx, record) = traceShow idx $ numArrangements record
    records = parseInput 5 input
    -- 4 - 4.7s


-- For a given diagram and list of working groups, count the total number of valid arrangements
numArrangements :: Record -> Int
numArrangements record =
      length
      $ filter (==(workingGroups record)) -- filter to ones that match the nums for this record
      $ map convertToNums allPossibilities -- convert to a number record
  where
    allPossibilities = expandPossibilities record (diagram record) "" -- generatate all possible combinations
    convertToNums = map length . wordsBy (=='.') -- remove periods and count each group of '#'

-- Recursively generate possibilties, converting each ? into two possible strings (# or .)
expandPossibilities :: Record -> String -> String -> [String]
expandPossibilities _ [] partialDiagram = [reverse partialDiagram]
expandPossibilities record ('?':xs) partialDiagram = expansionBroken ++ expansionWorking
  where
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

parseInput :: Int -> String -> [Record]
parseInput multiplier = map processLine . lines
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