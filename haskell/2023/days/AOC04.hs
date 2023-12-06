module Days.AOC04 (day04) where

import Data.List.Split (splitOneOf, chunksOf, splitOn)
import Data.List (intersect)
import Common (parseInt)
import Data.Map (Map)
import qualified Data.Map as Map

day04 = (part1, part2)

-- For each card, calculate the number of points it's worth and sum up the points
part1 input = show $ sum $ map calculatePoints parsed
  where parsed = parseInput input
        calculatePoints = numMatchesToPoints . getNumMatches

-- Recursively count cards and sum the total values
part2 input = show $ sum $  Map.elems $ countCards numMatches numCardsMap
  where numMatches = zip [1..] $ map getNumMatches parsed -- Number of matches for each card
        -- Initialize a map with key cardNum and val 1 (starting with 1 of every card)
        numCardsMap = Map.fromList(map (\x -> (x, 1)) [1..(length parsed)])
        parsed = parseInput input





-- Process each card and update the card counter
countCards [] numCardsMap = numCardsMap -- base case
countCards ((cardNum, numMatches):remainingCards) numCardsMap =
  -- recurse with remaining cards
  countCards remainingCards numCardsMap'
  where numCardsMap' = foldr incrementCardCount numCardsMap cardsToUpdate -- go through all updates to the countMap
        incrementCardCount = Map.adjust (numCurrCard+) -- updates the countMap for an index by the number of current cards we have
        numCurrCard = Map.findWithDefault 1 cardNum numCardsMap -- the number of the current card index we have
        cardsToUpdate = [cardNum+1..cardNum+numMatches] -- if 3 wins on card 2, this will be [3,4,5]


-- Convert to points based on number of matches
numMatchesToPoints x
  | x == 0 = 0
  | otherwise = 2 ^ (x-1)

-- For each line map to # of intersections between left and right
getNumMatches [left, right] = length $ intersect left right

-- Split each line into two lists of numbers
parseInput = map processLine . lines
  where processLine = map extractNumbers
          . splitOn " |"
          -- Remove the "Card #:"
          . drop 1 . dropWhile (/= ':')
        extractNumbers = map parseInt . chunksOf 3