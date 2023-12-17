module AOC17 (day17) where

import Data.Map (Map)
import qualified Data.Map as Map
import Common (parseInt)
import Algorithm.Search (dijkstra)
import Data.Maybe (fromJust)

data Dir = North | East | South | West deriving (Show, Eq, Ord)
data Point = Pt Int Int deriving (Show, Eq, Ord)
data PathState = PS {
  psPoint :: Point,
  psDirHistory :: [Dir]
} deriving (Show, Ord, Eq)

type Mapping = Map Point Int

day17 :: (String -> String, String -> String)
day17 = (part1, part2)

part1 :: String -> String
part1 input = show $ fst $ fromJust $ dijkstra next cost found initial
    where
        -- All next possible states
        next = getNextPoints mapping
        -- The cost for moving to the 
        cost _ nextState = mapping Map.! psPoint nextState
        -- Starting point is 0, 0 with no direction history
        initial = PS { psPoint = Pt 0 0, psDirHistory = []}
        -- We're done when we're at the bottom right corner
        found currState = psPoint currState == Pt (width - 1) (height - 1)
        (mapping, width, height) = parseInput input

part2 :: String -> String
part2 = show . parseInput

-- Given a current path state, determine a list of next possible path states
getNextPoints :: Mapping -> PathState -> [PathState]
getNextPoints mapping currState = filter inMapping $ map toState $ filter validDir [North, South, East, West]
  where
    -- Make sure generated point is actually within the map
    inMapping point = Map.member (psPoint point) mapping
    toState dir = PS {
      -- The new point given the current point and a direction
      psPoint = transformPoint (psPoint currState) $ dirDelta dir,
      -- Keep track of the last 3 directions
      psDirHistory = take 3 (dir:dirHistory)
    }
    -- Is valid if we're not going backwards and we're not more than 3 consecutive in one direction
    validDir dir = isNotBackwards dir && isNotFourthConsecutive dir
    isNotBackwards dir = null dirHistory || not (isOpposite dir $ head dirHistory)
    isNotFourthConsecutive dir = length dirHistory < 3 || any (/=dir) dirHistory 
    dirHistory = psDirHistory currState -- Last 3 directions for the current state

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


