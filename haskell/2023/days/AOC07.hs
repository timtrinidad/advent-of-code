module Days.AOC07 (day07) where

import Data.List (group, sort, sortBy)
import Data.List.Split (splitOn)
import Common (parseInt)
import Debug.Trace

day07 = (part1, part2)

data Hand = Hand { cards :: String, sortVal :: String, bid :: Int }  deriving (Show)

-- Sort the cards by the sortval, multiply rank by bid, and sum
part1 input = show $ sum $ map calcWinnings $ zip [1..] $ sortBy sortValSorter parsed
  where
    parsed = parseInput input
    -- Compare by 'sortVal' key
    sortValSorter a b = compare (sortVal a) (sortVal b)
    -- Mutiply bid by rank
    calcWinnings (rank, hand) = bid hand * rank

part2 input = do
  show "part2 not defined for day 07"

-- Create a Hand object which contains the raw card string, the bid, and a sortable value
parseInput = map processLine . lines
  where
    processLine = createHand . splitOn " "
    createHand [cards, bidStr] = Hand { cards = cards, sortVal = sortVal, bid = parseInt bidStr }
      where
        sortVal = (determineType cards):(map sortableLetters cards)

-- Based on the number of each card in a hand, determine the type of hand
determineType cards
  | cardNums == [5] = '7' -- five of a kind
  | cardNums == [1, 4] = '6' -- four of a kind
  | cardNums == [2, 3] = '5' -- full house
  | cardNums == [1, 1, 3] = '4' -- three of a kind
  | cardNums == [1, 2, 2] = '3' -- two pair
  | cardNums == [1, 1, 1, 2] = '2' -- one pair
  | otherwise = '1' -- high card
  where
    -- count each group and sort
    cardNums = sort $ map length groups
    -- Group each card by letter
    groups = group $ sort cards

-- Normalize card to letters that are in order of strength (higher corresponding letter = higher strength)
sortableLetters 'T' = 'D'
sortableLetters 'J' = 'E'
sortableLetters 'Q' = 'F'
sortableLetters 'K' = 'G'
sortableLetters 'A' = 'H'
sortableLetters char = char