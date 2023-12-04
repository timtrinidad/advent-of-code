module Days.AOC03 (day03) where

import Common (parseInt)
import Text.Regex.TDFA
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (foldr, foldr1, union, sort, elemIndex, find)
import Data.List.Split (chunksOf)
import Data.Char (isDigit)
import Data.Maybe (fromJust, isJust)

day03 = (part1, part2)

part1 input = do
  let (partNums, symbols) = parseInput input
  print
    $ sum
    -- Extract second val of tuple
    $ map (\(_, num, _) -> num)
    -- Filter to only numbers which have a neighboring symbol
    $ filter (\((x, y), num, len) ->
        -- for every surrounding coordinate (+/- 1 vertically, -1 to len horizontally), find if any coordinate
        -- exists in the "symbols" set
        isJust $ find (\(x', y') -> Set.member((x + x', y + y')) symbols) [(x', y') | x' <- [-1..len], y' <- [-1..1]]
      ) partNums

part2 input = do
  print "part2 not defined for day 03"

-- Split input by line and add row numbers.
-- Parse out numbers and symbols.
parseInput input = do
  (parsePartNums input, parseSymbols input)

-- Parse out the location and length of all part numbers
parsePartNums input = do
  -- For simplicity, parse out of a single string rather than a list of strings.
  -- Keep track of the "row length"
  let rowLength = 1 + (fromJust $ elemIndex '\n' input)
  let matchIdxs = getAllMatches (input =~ "[0-9]+") :: [(Int, Int)]
  let matchNums = map parseInt $ getAllTextMatches (input =~ "[0-9]+")
  -- Convert the single-line index of each found match and convert it to (x, y) coordinates
  -- return a tuplie with (xyCoors, num, len)
  map (\(idx, (x, len)) -> ((x `mod` rowLength, x `div` rowLength), matchNums !! idx, len)) $ zip [0..] matchIdxs

-- Collect a set of (x, y) coordinates for all symbols
-- for each row
parseSymbols =
    Set.fromList
    . foldr1 union
    . map (\(y, row)  ->
        -- generate a list of (x, y) coordinates for remaining points
        map (\(x, _) -> (x, y))
          -- Filter out all periods and digits
          $ filter(\(_, char) -> (not $ isDigit char) && char /= '.')
          -- Add indices (x coordinate)
          $ zip [0..] row
      )
    . zip [0..]
    . lines


