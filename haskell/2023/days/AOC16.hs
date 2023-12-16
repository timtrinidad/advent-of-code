module AOC16 (day16) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Data.List (nub)

type Mapping = Map Point Char
data Point = Pt Int Int deriving (Show, Eq, Ord)
data Dir = North | South | East | West deriving (Show, Eq, Ord)

day16 :: (String -> String, String -> String)
day16 = (part1, part2)

part1 :: String -> String
part1 input = show $ numTraversed mapping (Pt 0 0, East)
  where (mapping, _) = parseInput input

-- Find all traversed points for each starting point and find max
part2 :: String -> String
part2 input = show $ maximum $ map (numTraversed mapping . traceShowId) startingPoints
  where
    -- Calculate all possible staring points
    startingPoints = 
      [(Pt x 0, South) | x <- [0..width-1]] ++
      [(Pt x (height-1), North) | x <- [0..width-1]] ++
      [(Pt 0 y, East) | y <- [0..height-1]] ++
      [(Pt (width-1) y, West) | y <- [0..height-1]]
    (mapping, (width, height)) = parseInput input

-- Find unique traversed points (ignoring direction) and get length
numTraversed :: Mapping -> (Point, Dir) -> Int
numTraversed mapping (startPoint, startDir) =
  length $ nub -- get uniques now that we're not considering direction
  $ map fst -- ignore direction
  $ Set.toList $ traverseGrid mapping Set.empty [(startPoint, startDir)] -- traverse

-- BFS to traverse the grid, splitting when necessary. Returns a unique set of traversed points.
traverseGrid :: Mapping -> Set (Point, Dir) -> [(Point, Dir)] -> Set (Point, Dir)
traverseGrid _ seen [] = seen
traverseGrid mapping seen ((point, dir):toExplore)
  | Set.member (point, dir) seen = skip -- already seen - move onto next point
  | not $ Map.member point mapping = skip -- point is not within map, move onto next point
  | otherwise = traverseGrid mapping seen' toExplore' -- Add new points to explore and move on
  where
    skip = traverseGrid mapping seen toExplore -- ignore current point and go onto the next one
    -- Append all possible next points/directions to the list to explore
    toExplore' = toExplore ++ map findNextPoint (nextDirs dir charAtPoint)
    -- For a new direction from the current point, calculate the next point 
    findNextPoint dir' = (transformPoint point $ dirDelta dir', dir')
    -- Add the current point/direction to the set (since the same point can be hit from multiple dirs)
    seen' = Set.insert (point, dir) seen
    charAtPoint = mapping Map.! point -- The character at a given xy point

-- Convert a direction to an x y delta
dirDelta :: Dir -> (Int, Int)
dirDelta North = (0, -1)
dirDelta East = (1, 0)
dirDelta South = (0, 1)
dirDelta West = (-1, 0)

-- Given a source direction and character, the next possible directions
nextDirs :: Dir -> Char -> [Dir]
nextDirs East '|' = [North, South]
nextDirs West '|' = [North, South]
nextDirs North '-' = [East, West]
nextDirs South '-' = [East, West]
nextDirs North '/' = [East]
nextDirs East '/' = [North]
nextDirs South '/' = [West]
nextDirs West '/' = [South]
nextDirs North '\\' = [West]
nextDirs East '\\' = [South]
nextDirs South '\\' = [East]
nextDirs West '\\' = [North]
nextDirs dir _ = [dir] -- all else - continue dir

-- Given a point and a delta, calculate the new point
transformPoint :: Point -> (Int, Int) -> Point
transformPoint (Pt x y) (dx, dy) = Pt (x+dx) (y+dy)

-- Create a map of characters keyed by (x,y) coordinates along with width/height
parseInput :: String -> (Map Point Char, (Int, Int))
parseInput input = (mapping, (width, height))
  where
    height = length allLines
    width = length $ head allLines
    mapping = Map.fromList $ concat $ zipWith parseLine [0..] allLines
    allLines = lines input
    parseLine y = zipWith (parseChar y) [0..]
    parseChar y x char = (Pt x y, char)

