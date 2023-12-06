module Days.AOC06 (day06) where

import Text.Regex.TDFA (getAllTextMatches, (=~))
import Data.List.Split (chunksOf)
import Data.List (transpose)
import Common (parseInt)
import Debug.Trace

day06 = (part1, part2)

part1 :: String -> String
-- Product of the number of won races
part1 input = show $ product $ numWonRaces
  where
    -- For each race, calculate how many races are won
    numWonRaces = map (length . wonRaces) parsed
    -- Calculete all possible outcomes and determine which ones are past current record
    wonRaces [raceLength, currRecord] = filter (>currRecord) $ possibleOutcomes raceLength
    possibleOutcomes raceLength = [(raceLength - holdMs) * holdMs | holdMs <- [0..raceLength]]
    parsed = parseInput input

part2 input = do
  show "part2 not defined for day 06"

-- Split into two lines, extract 2 numbers, and transpose into pairs based on vertical position
parseInput input = transpose $ map parseLine $ lines input
  where parseLine line =  map parseInt $ getAllTextMatches (line =~ "[0-9]+")