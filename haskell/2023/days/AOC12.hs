module AOC12 (day12) where
import Data.List.Split (splitOn)
import Common (parseInt)
import Data.List (intercalate)
import Debug.Trace

day12 :: (String -> String, String -> String)
day12 = (part1, part2)

part1 :: String -> String
part1 input = show $ sum $ map countPossibilities records
  where
    records = parseInput input

part2 :: String -> String
part2 input = "todo"

-- Generate all valid diagrams and count them
countPossibilities :: (String, [Int]) -> Int
countPossibilities (slots, workingGroups) = length $ filter isValid diagrams 
  where
    isValid = and . zipWith (\a b -> a == '?' || a == b) slots -- Check to make sure that this diagram matches with what we know
    diagrams = map (generateDiagram workingGroups) $ intComposition (numWorkingGroups + 1) numUnknownBroken  -- generate all possible slots given the order of working slot groups
    numUnknownBroken = numTotal - numWorking - numKnownBroken -- remaining slots
    numKnownBroken = numWorkingGroups - 1 -- must be at least one known broken between each group
    numWorkingGroups = length workingGroups -- total number of working groups
    numTotal = length slots -- total nunmber of slots
    numWorking = sum workingGroups -- total number of known working slots

-- Given a list of working slot groups and broken slot groups, generate a list of possible strings
generateDiagram :: [Int] -> [Int] -> String
generateDiagram workingSlots brokenSlots = concat $ zipWith combine brokenSlots' (workingSlots ++ [0])
  where
    combine numBroken numWorking = replicate numBroken '.' ++ replicate numWorking '#' -- generate the string
    -- Inject a broken spring between all working springs (on all except the first and last)
    brokenSlots' = [fstBrokenSlot] ++ midBrokenSlots ++ [lastBrokenSlot]
    midBrokenSlots = map (+1) $ init rstBrokenSlots
    lastBrokenSlot = last rstBrokenSlots
    (fstBrokenSlot:rstBrokenSlots) = brokenSlots


-- Find all ways to have numInts integers to add up to n
-- https://stackoverflow.com/questions/39074828/composition-of-integer-in-c-and-haskell
intComposition :: Int -> Int -> [[Int]]
intComposition 1 rest = [[rest]]
intComposition numInts n = [x:rest | x <- [0..n], rest <- intComposition (numInts-1) (n-x)]

parseInput :: String -> [(String, [Int])]
parseInput = map processLine . lines
  where
    processLine = parseNums . splitOn " " -- split by space
    parseNums [slots, nums] = (slots, map parseInt $ splitOn "," nums) -- convert right side to list of ints