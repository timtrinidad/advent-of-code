module AOC07 (day07) where

import Data.List (group, sort, sortBy)
import Data.List.Split (splitOn)
import Common (parseInt)
import Debug.Trace

day07 = (part1, part2)

data Hand = Hand { cards :: String, sortVal :: String, bid :: Int }  deriving (Show)

part1 input = show $ totalWinnings allowJokers input
  where allowJokers = False
part2 input = show $ totalWinnings allowJokers input
  where allowJokers = True

-- Sort the cards by the sortval, multiply rank by bid, and sum
totalWinnings allowJokers input = sum $ map calcWinnings  $  zip [1..] $ sortBy sortValSorter parsed
   where
     parsed = parseInput allowJokers input
     -- Compare by 'sortVal' key
     sortValSorter a b = compare (sortVal a) (sortVal b)
     -- Multiply bid by rank
     calcWinnings (rank, hand) = bid hand * rank

-- Create a Hand object which contains the raw card string, the bid, and a sortable value
parseInput allowJokers input = map processLine $ lines input
  where
    processLine = createHand' . splitOn " "
    createHand' = createHand allowJokers


-- Create a Hand object which includes the original card string, the sortable card string, and the bid
createHand allowJokers [cards, bidStr] = Hand {
  cards = cards,
  bid = parseInt bidStr,
  -- prepend the hand type to the sortable card string
  sortVal = handType:sortableCardsString
}
  where
    -- Determine the hand type
    handType = determineType allowJokers cards
    -- Generate a sortable string where the letters are in sortable order
    -- For a hand like '9K9KK', generates '59G9GG'
    -- where K is replaced with sortable letter 'G'
    sortableCardsString = map sortableLetters' cards
    sortableLetters' = sortableLetters allowJokers

-- Based on the number of each card in a hand, determine the type of hand
-- First of each case is for no jokers, others are for varying number of jokers (1-5)
determineType allowJokers cards
  | cardNums `elem` [[5], [4], [3], [2], [1], []] = '7' -- five of a kind
  | cardNums `elem` [[1, 4], [1, 3], [1, 2], [1, 1]] = '6' -- four of a kind
  | cardNums `elem` [[2, 3], [2, 2]] = '5' -- full house
  | cardNums `elem` [[1, 1, 3], [1, 1, 2], [1, 1, 1]] = '4' -- three of a kind
  | cardNums `elem` [[1, 2, 2]] = '3' -- two pair - even one joker will allow it to be promoted it to 3 of a kind
  | cardNums `elem` [[1, 1, 1, 2], [1, 1, 1, 1]] = '2' -- one pair
  | otherwise = '1' -- high card
  where
    -- count each group and sort
    cardNums = sort $ map length groups
    -- Group each card by letter
    groups = group
      $ sort
      -- If jokers are allowed, remove them for pattern matching
      $ if allowJokers then filter (/= 'J') cards else cards


-- Normalize card to letters that are in order of strength (higher corresponding letter = higher strength)
sortableLetters _ 'T' = 'D'
sortableLetters False 'J' = 'E'
sortableLetters True 'J' = '0' -- If jokers are allowed, needs to have lowest value (lower than '2')
sortableLetters _ 'Q' = 'F'
sortableLetters _ 'K' = 'G'
sortableLetters _ 'A' = 'H'
sortableLetters _ char = char