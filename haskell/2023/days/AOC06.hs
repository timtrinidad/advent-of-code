module Days.AOC06 (day06) where

import Text.Regex.TDFA (getAllTextMatches, (=~))
import Data.List.Split (chunksOf)
import Data.List (transpose)
import Common (parseInt)
import Debug.Trace

day06 = (part1, part2)

-- Product of the number of won races
part1 :: String -> String
part1 input = show $ product $ numWonRaces
  where
    -- For each race, calculate how many races are won
    numWonRaces = map countWinningOutcomes raceInfos
    -- Split into two lines, extract 2 numbers, and transpose into pairs based on vertical position
    raceInfos = transpose $ map parseLine $ lines input
    parseLine line =  map parseInt $ getAllTextMatches (line =~ "[0-9]+")

-- Parse input ignoring spaces. Determine number of winning outcomes for the single race
part2 :: String -> String
part2 input = show $ countWinningOutcomes raceInfo
  where
    raceInfo = map parseInt $ getAllTextMatches (line =~ "[0-9]+")
    line = filter (/= ' ') input


-- Calculete all possible outcomes and determine which ones are past current record
countWinningOutcomes [raceLength, currRecord] = length $ filter (>currRecord) $ possibleOutcomes raceLength
possibleOutcomes raceLength = [(raceLength - holdMs) * holdMs | holdMs <- [0..raceLength]]