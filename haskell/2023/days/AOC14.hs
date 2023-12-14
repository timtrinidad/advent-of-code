module AOC14 (day14) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust)
import Data.List (find, group, sort)
import Debug.Trace

data Direction = North | West | South | East
  deriving (Eq, Ord, Show)


day14 = (part1, part2)

-- Get the sum of all row numbers for each rolling rock
part1 :: String -> String
part1 input = show $ calculateLoad height finalMapping
  where
    finalMapping = moveRocks North (width, height) mapping -- final location of all rocks
    (mapping, (width, height)) = parseInput input

-- Find the number at the given index in the pattern, accounting for the skip
part2 :: String -> String
part2 input = show $ loads !! (skip + patternIdx) 
  where
    -- Apply the pattern to the number of cycles needed
    patternIdx = (1000000000 - skip - 1) `mod` patternLength
    -- Find a patten - iteratively try different skip and pattern lengths until a pattern is found
    -- (skip x, extract the next to y's to see if they match)
    (skip, patternLength) = fromJust $ find (\(x, y) -> take y (drop x loads) == take y (drop (x+y) loads)) [(x, y) | x <- [0..1000], y <- [2..1000]]
    -- Only keep every fourth one (a full cycle)
    loads = map (calculateLoad height . snd) $ filter (\(x, _) -> x `mod` 4 == 3) $ zip [0..] allMappings
    -- process the mapping tracking the result each time
    allMappings = scanl (\acc dir -> moveRocks dir (width, height) acc) mapping directions 
    -- replicate 1000 times so we can find a pattern
    directions = concat $ replicate 1000 [North, West, South, East] 
    (mapping, (width, height)) = parseInput input

-- Find all rolling rocks and map to their inverted height (highest one has highest row index)
calculateLoad :: Int -> Map (Int, Int) Char -> Int
calculateLoad height mapping = sum $ map (\(_, y) -> height - y) $ Map.keys $ Map.filter (=='O') mapping

-- Update a mapping by shifting all rolling rocks in a given direction
moveRocks direction (width, height) mapping = foldl moveRock' mapping allPoints'
  where
    allPoints' = (if direction `elem` [North, West] then id else reverse) allPoints
    allPoints = [(x, y) | x <- [0..width-1], y <- [0..height-1]]
    moveRock' = moveRock direction (width, height)

-- Move a rock as high as possible, returning a new Map after the move.
moveRock :: Direction -> (Int, Int) -> Map (Int, Int) Char -> (Int, Int) -> Map (Int, Int) Char
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
