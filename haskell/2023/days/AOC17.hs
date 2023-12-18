module AOC17 (day17) where

import Data.Map (Map)
import qualified Data.Map as Map
import Common (parseInt)
import Algorithm.Search (dijkstra, aStar)
import Data.Maybe (fromJust)
import Data.List (group)
import Debug.Trace (traceShow, traceShowId, trace)

data Dir = North | East | South | West | None deriving (Show, Eq, Ord)
data Point = Pt Int Int deriving (Show, Eq, Ord)
data PathState = PS {
  psPoint :: Point,
  psLastDir :: Dir,
  psConsecutiveDir :: Int
} deriving (Show, Ord, Eq)

type Mapping = Map Point Int

day17 :: (String -> String, String -> String)
day17 = (part1, part2)

part1 :: String -> String
part1 input = show $ findPathLength mapping validateNextState validateFinalState
  where
    validateNextState currState nextDir =
      psConsecutiveDir currState < 3 -- haven't had 3 consecutive moves yet
      || psLastDir currState /= nextDir -- the next dir is different than the last dir
    -- We're done if we're on the bottom right
    validateFinalState currState = psPoint currState == Pt (width - 1) (height - 1) 
    (mapping, width, height) = parseInput input

part2 :: String -> String
part2 input = show $ findPathLength mapping validateNextState validateFinalState
  where
    validateNextState currState nextDir
      -- No existing moves - any direction is possible
      | psConsecutiveDir currState == 0 = True
      -- Last 10 moves are all equal to the proposed direction - not valid
      | psConsecutiveDir currState == 10 && psLastDir currState == nextDir = False
      -- Less than 4 consecutive moves - needs to be the same as last
      | psConsecutiveDir currState < 4 && psLastDir currState /= nextDir = False
      | otherwise = True
    -- We'de done if we're on the bottom right AND we've had at least 4 consecutive moves
    validateFinalState currState = psPoint currState == Pt (width - 1) (height - 1) && psConsecutiveDir currState >= 4
    (mapping, width, height) = parseInput input

-- Find the length of the path from the top left to the bottom right based on the challenge rules
findPathLength :: Mapping -> (PathState -> Dir -> Bool) -> (PathState -> Bool) -> Int
findPathLength mapping validateNextState found = fst $ fromJust $ dijkstra next cost found initial
    where
        -- All next possible states
        next = getNextPoints mapping validateNextState
        -- The cost for moving to the 
        cost _ nextState = mapping Map.! psPoint nextState
        -- heuristic (PS (Pt x y) _) = (width - x) + (height - y)
        -- Starting point is 0, 0 with no direction history
        initial = PS { psPoint = Pt 0 0, psConsecutiveDir = 0, psLastDir = None}

-- Given a current path state, determine a list of next possible path states
getNextPoints :: Mapping -> (PathState -> Dir -> Bool) -> PathState -> [PathState]
getNextPoints mapping validateNextState currState = filter inMapping $ map toState  $ filter validDir [North, South, East, West]
  where
    -- Make sure generated point is actually within the map
    inMapping point = Map.member (psPoint point) mapping
    toState dir = PS {
      -- The new point given the current point and a direction
      psPoint = transformPoint (psPoint currState) $ dirDelta dir,
      psLastDir = dir,
      psConsecutiveDir = 1 + (if dir == psLastDir currState then psConsecutiveDir currState else 0)
    }
    -- Is valid if we're not going backwards and the given validation passes
    validDir dir = isNotBackwards dir && validateNextState currState dir
    isNotBackwards dir = not (isOpposite dir $ psLastDir currState)

-- Given two points, determine if they are opposites
isOpposite :: Dir -> Dir -> Bool
isOpposite North South = True
isOpposite South North = True
isOpposite West East = True
isOpposite East West = True
isOpposite _ _ = False

-- Convert a direction to an x y delta
dirDelta :: Dir -> (Int, Int)
dirDelta North = (0, -1)
dirDelta East = (1, 0)
dirDelta South = (0, 1)
dirDelta West = (-1, 0)

-- Given a point and a delta, calculate the new point
transformPoint :: Point -> (Int, Int) -> Point
transformPoint (Pt x y) (dx, dy) = Pt (x+dx) (y+dy)

-- Parse an input into
--   a map of characters, keyed by x, y
--   width and height
parseInput :: String -> (Mapping, Int, Int)
parseInput input = (mapping, width, height)
  where
    height = length allLines
    width = length $ head allLines
    mapping = Map.fromList $ concat $ zipWith parseLine [0..] allLines
    allLines = lines input
    parseLine y = zipWith (parseChar y) [0..]
    parseChar y x char = (Pt x y, parseInt [char])


