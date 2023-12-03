module Days.AOC05 (day05) where

import Data.List (foldr, transpose)
import Data.List.Split (chunksOf, splitOn)
import Data.Text (strip)
import Data.Char (isSpace)
import Common (parseInt)
import Text.Regex.TDFA
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

day05 = (part1, part2)

part1 input = do
  let (stacks, instructions) = parseInput input
  print $ topOfStacks stacks instructions True

part2 input = do
  let (stacks, instructions) = parseInput input
  print $ topOfStacks stacks instructions False

-- Get the top of the resulting stacks
topOfStacks stacks instructions = map(head . snd) . Map.toList . execInst stacks instructions

-- Execute instructions recursively
execInst :: Map Int String -> [[Int]] -> Bool -> Map Int String
-- Base case - no more instructions, return current stack
execInst stacks [] _  = stacks
execInst stacks ([num,src,dst]:xs) shouldReverse = do
  -- Pull out relevant src/dst stacks
  let srcStack = Map.findWithDefault "" src stacks
  let dstStack = Map.findWithDefault "" dst stacks
  -- Split out what's being moved
  let (toMove, srcStack') = splitAt num srcStack
  -- Add it back to destination stack
  let dstStack' = (if shouldReverse then reverse toMove else toMove) ++ dstStack
  -- Update stack map
  let stacks' = Map.insert dst dstStack' $ Map.insert src srcStack' stacks
  -- Recurse
  execInst stacks' xs shouldReverse

-- Parse stacks and instructions separately
parseInput input = (parseStacks stacks, parseInstructions instructions)
  where [stacks, instructions] = splitOn [""] $ lines input

-- Parse the stack visualization
parseStacks =
  Map.fromList
  -- Parse as ints and return a tuple of (stackNum, stack)
  . map (\(x:xs) -> (parseInt [x], reverse xs))
  -- Remove spaces
  . map (filter (not . isSpace))
  . transpose
  . reverse
  -- Each column is 4 chars - pull out index 1
  . map(map(\x -> x !! 1) . chunksOf 4)

-- Parse out numbers from text instructions
parseInstructions = map (\x -> map parseInt $ getAllTextMatches (x =~ "[0-9]+"))

