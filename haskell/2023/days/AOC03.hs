{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move map inside list comprehension" #-}
module AOC03 (day03) where

import Text.Regex.TDFA
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (foldr, foldr1, union, sort, elemIndex, find)
import Data.List.Split (chunksOf)
import Data.Char (isDigit)
import Data.Maybe (fromJust, isJust)
import Common (parseInt)

day03 :: (String -> String, String -> String)
day03 = (part1, part2)

part1 :: String -> String
part1 input = do
  let (partNums, symbols) = parseInput input
  show
    $ sum
    -- Extract second val of tuple
    $ map (\(_, num, _) -> num)
    -- Filter to only numbers which have a neighboring symbol
    $ filter (\((x, y), _, len) ->
        -- for every surrounding coordinate (+/- 1 vertically, -1 to len horizontally), find if any coordinate
        -- exists in the "symbols" set
        isJust $ find (\(x', y') -> Map.member (x + x', y + y') symbols) [(x', y') | x' <- [-1..len], y' <- [-1..1]]
      ) partNums

part2 :: String -> String
part2 input = do
  let (partNums, symbols) = parseInput input
  -- In part 1 we iterated on numbers and looked up symbols but now we have to do the opposite
  -- Create a mapping of coodrinates to numbers, ensuring that a 3 digit number gets 3 coordinates in the map
  let partNumsMap =
        Map.fromList
        $ foldr (\((x, y), num, len) acc ->
            foldr (\x' acc2 -> ((x + x', y), num) : acc2) acc [0..len-1]
          ) [] partNums
  show
    $ sum
    -- Product of each set
    $ map (product . Set.toList)
    -- Only sets of 2
    $ filter (\x -> Set.size x == 2)
    $ map (\(x, y) ->
        -- Find all neighboring numbers, defaulting to 0 if a number isn't found (which is then filtered out)
        Set.fromList $ filter (/= 0) $ map (\(x', y') ->
            Map.findWithDefault 0 (x + x', y + y') partNumsMap
          ) [(x', y') | x' <- [-1..1], y' <- [-1..1]]
      )
    $ Map.keys
    -- Only consider 'gears' / '*'
    $ Map.filter (== '*') symbols

-- Split input by line and add row numbers.
-- Parse out numbers and symbols.
parseInput :: String -> ([((Int, Int), Int, Int)], Map (Int, Int) Char)
parseInput input = do
  (parsePartNums input, parseSymbols input)

-- Parse out the location and length of all part numbers
parsePartNums :: String -> [((Int, Int), Int, Int)]
parsePartNums input = do
  -- For simplicity, parse out of a single string rather than a list of strings.
  -- Keep track of the "row length"
  let rowLength = 1 + fromJust (elemIndex '\n' input)
  let matchIdxs = getAllMatches (input =~ "[0-9]+") :: [(Int, Int)]
  let matchNums = map parseInt $ getAllTextMatches (input =~ "[0-9]+")
  -- Convert the single-line index of each found match and convert it to (x, y) coordinates
  -- return a tuplie with (xyCoors, num, len)
  zipWith (\idx (x, len) -> ((x `mod` rowLength, x `div` rowLength), matchNums !! idx, len)) [0..] matchIdxs

-- Collect a set of (x, y) coordinates for all symbols
-- for each row
parseSymbols :: String -> Map (Int, Int) Char
parseSymbols =
    Map.fromList
    . foldr1 union
    . zipWith (\y row  ->
        -- generate a list of (x, y) coordinates for remaining points
        map (\(x, char) -> ((x, y), char))
          -- Filter out all periods and digits
          $ filter (\(_, char) -> not (isDigit char) && char /= '.')
          -- Add indices (x coordinate)
          $ zip [0..] row
      ) [0..] . lines


