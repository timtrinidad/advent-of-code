module AOC12 (day12) where
import Common (parseInt)
import Data.List (group)
import Debug.Trace

day12 :: (String -> String, String -> String)
day12 = (part1, part2)

part1 :: String -> String
part1 input = show $ sum $ map numArrangements records -- For each record, get the number of possible arragements, and sum
  where
    numArrangements (slots, nums) =
      length -- total number found
      $ filter (==nums) -- filter to ones that match the nums for this record
      $ map convertToNums -- convert to a number record
      $ expandPossibilities slots -- generatate all possible combinations
    convertToNums = map length . wordsBy (=='.') -- remove periods and count each group of '#'
    records = parseInput input


part2 :: String -> String
part2 input = do
  show "part2 not defined for day 12"

-- Recursively generate possibilties, converting each ? into two possible strings (# or .)
expandPossibilities :: String -> [String]
expandPossibilities [] = [""]
expandPossibilities ('?':xs) = map ('.' :) substring ++ map ('#' :) substring
  where substring = expandPossibilities xs
expandPossibilities (x:xs) = map (x :) $ expandPossibilities xs

parseInput :: String -> [(String, [Int])]
parseInput = map processLine . lines
  where
    processLine = parseNums . splitOn " " -- split by space
    parseNums [slots, nums] = (slots, map parseInt $ splitOn "," nums) -- convert right side to list of ints