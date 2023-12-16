module AOC14 (day14) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust)
import Data.List (find, group, sort)
import Debug.Trace
import Data.Text.Unsafe (iter)

data Direction = North | West | South | East
  deriving (Eq, Ord, Show)

type Dimensions = (Int, Int)
type Mapping = Map Dimensions Char

day14 :: (String -> String, String -> String)
day14 = (part1, part2)

-- Get the sum of all row numbers for each rolling rock
part1 :: String -> String
part1 input = show $ calculateLoad height finalMapping
  where
    finalMapping = moveRocks North (width, height) mapping -- final location of all rocks
    (mapping, (width, height)) = parseInput input

-- Get the load after 1b cycles
part2 :: String -> String
part2 input = show $ calculateLoad height mappingEquivalent
  where
    -- Get the mapping for this equivalent index
    mappingEquivalent = fst $ fromJust $ find (\(_, idx) -> idx == idxEquivalent) $  Map.assocs seenMappings
    -- The equivalent index is the equivalent position within the loop (accounting for where the loop starts)
    idxEquivalent = loopStart + (1000000000 - loopStart - 1) `mod` (loopEnd - loopStart)
    -- Get loop info
    (seenMappings, loopStart, loopEnd) = findLoop mapping Map.empty (width, height) 0 $ cycle [North, West, South, East]
    (mapping, (width, height)) = parseInput input

-- Find all rolling rocks and map to their inverted height (highest one has highest row index)
calculateLoad :: Int -> Mapping -> Int
calculateLoad height mapping = sum $ map (\(_, y) -> height - y) $ Map.keys $ Map.filter (=='O') mapping

-- Generate all mappings, the start idx, and end idx of a loop.
findLoop :: Mapping -> Map Mapping Int -> Dimensions -> Int -> [Direction] -> (Map Mapping Int, Int, Int)
findLoop currMapping seenMappings (width, height)  iterNum (currDir:restDirs)
  -- base case - return loop info
  | Map.member newMapping seenMappings = (seenMappings, seenMappings Map.! newMapping, iterNum)
  -- recurse
  | otherwise = findLoop newMapping seenMappings' (width, height) iterNum' restDirs
  where
    -- If "East" (end of a full cycle), increment the cycle counter and add the current state to the "Seen" map
    iterNum' = iterNum + if currDir == East then 1 else 0
    seenMappings' = if currDir == East then Map.insert newMapping iterNum seenMappings else seenMappings
    newMapping = moveRocks currDir (width, height) currMapping

-- Update a mapping by shifting all rolling rocks in a given direction
moveRocks :: Direction -> Dimensions -> Mapping -> Mapping
moveRocks direction (width, height) mapping = foldl moveRock' mapping allPoints'
  where
     -- if tilting South or East start from the opposite direction
    allPoints' = (if direction `elem` [North, West] then id else reverse) allPoints
    allPoints = [(x, y) | x <- [0..width-1], y <- [0..height-1]] -- iterate through all points
    moveRock' = moveRock direction (width, height)

-- Move a rock as high as possible, returning a new Map after the move.
moveRock :: Direction -> Dimensions -> Mapping -> Dimensions -> Mapping
moveRock dir (width, height) mapping (currX, currY)
  | Map.findWithDefault '#' (currX, currY) mapping == '#' = mapping -- do nothing - can't move a '#'
  | otherwise = Map.insert newPoint 'O' $ Map.delete (currX, currY) mapping -- replace with new 
  where
    -- New point is one aftet the obstacle
    newPoint = case dir of
      North -> (currX, obstacleY + 1)
      West  -> (obstacleX + 1, currY)
      South -> (currX, obstacleY - 1)
      East  -> (obstacleX - 1, currY)
    -- Move up one by one until we hit an obstacle
    obstacleY = case dir of
      North -> fromMaybe (-1) $ find (\y -> Map.member (currX, y) mapping) [currY-1,currY-2..(-2)]
      South -> fromMaybe height $ find (\y -> Map.member (currX, y) mapping) [currY+1,currY+2..(height+1)]
    obstacleX = case dir of
      West -> fromMaybe (-1) $ find (\x -> Map.member (x, currY) mapping) [currX-1,currX-2..(-2)]
      East -> fromMaybe width $ find (\x -> Map.member (x, currY) mapping) [currX+1,currX+2..(width+1)]

-- Parse the input string into a Map and dimensions
parseInput :: String -> (Mapping, Dimensions)
parseInput input = (mapping, (width, height))
  where
    width = length $ head allLines
    height = length allLines
    -- create Map and filter out empty spaces (periods)
    mapping = Map.filter (/= '.') $ Map.fromList $ concat $ zipWith processLine [0..] allLines
    allLines = lines input
    -- Parse into a grid
    processLine y = zipWith (processChar y) [0..]
    processChar y x char = ((x, y), char)
