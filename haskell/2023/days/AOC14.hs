module AOC14 (day14) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (find)
import Debug.Trace

day14 = (part1, part2)

-- Get the sum of all row numbers for each rolling rock
part1 :: String -> String
part1 input = show $ sum rollingRockRows
  where
    -- Find all rolling rocks and map to their inverted height (highest one has highest row index)
    rollingRockRows = map (\(_, y) -> height - y) $ Map.keys $ Map.filter (=='O') finalMapping
    -- Map containing final location of rocks
    finalMapping = foldl moveRock mapping $ [(x, y) | x <- [0..width-1], y <- [0..height-1]]
    (mapping, (width, height)) = parseInput input

part2 :: String -> String
part2 input = do
  show "part2 not defined for day 14"

-- Move a rock as high as possible, returning a new Map after the move.
moveRock :: Map (Int, Int) Char -> (Int, Int) -> Map (Int, Int) Char
moveRock mapping (currX, currY)
  | Map.findWithDefault '#' (currX, currY) mapping == '#' = mapping -- do nothing
  | otherwise = Map.insert newPoint 'O' $ Map.delete (currX, currY) mapping -- replace with new 
  where
    newPoint = (currX, obstacleY + 1) -- New point is one aftet the obstacle
    -- Move up one by one until we hit an obstacle
    obstacleY = fromMaybe (-1) $ find (\y -> Map.member (currX, y) mapping) [currY-1,currY-2..(-2)]

-- Parse the input string into a Map and dimensions
parseInput :: String -> (Map (Int, Int) Char, (Int, Int))
parseInput input = (mapping, (width, height))
  where
    width = length $ head allLines
    height = length allLines
    -- create Map and filter out empty spaces (periods)
    mapping = Map.filter (/= '.') $ Map.fromList $ concat $ zipWith processLine [0..] $ allLines
    allLines = lines input
    -- Parse into a grid
    processLine y = zipWith (processChar y) [0..]
    processChar y x char = ((x, y), char)
