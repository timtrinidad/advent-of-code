module Days.AOC10 (day10) where
import Data.Maybe (fromJust)
import Data.List (elemIndex, find)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Debug.Trace

day10 = (part1, part2)

data Pipe = Pipe { label :: Char, connections :: [(Int, Int)] } deriving (Show)

-- Traverse the map to get the full loop distance - the furthest point will be half the loop distance
part1 input = show $ loopDistance `div` 2
  where
    -- traverse pipes to get the full loop distance
    loopDistance = traversePipes pipesMap 1 startCoord nextFromStart
    -- Find the pipes connecting to the start coordinate and arbitrarily select the first one
    nextFromStart = head $ findConnections pipesMap startCoord
    (pipesMap, startCoord) = parseInput input

part2 _ = do
  show "part2 not defined for day 10"

-- Find the connections for a given location by traversing all possible neighbors
-- and filtering for which ones have the current (x,y) coordinates in its connections
findConnections pipesMap (x, y) = filter doesConnect neighbors
  where
    -- Whether or not the current location is in the list of connections for the pipe at a given point
    doesConnect neighborXY = (x, y) `elem` connections (pipesMap ! neighborXY)
    -- All neighbors that have a non-dirt entry in pipesMap
    neighbors = filter (`Map.member` pipesMap) [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1]]

-- Recursively travese the pipesMap until we get to a pipe labelled 'S'
traversePipes pipesMap distance prevXY currXY
  -- base case - return current distance
  | label (pipesMap ! currXY) == 'S' = distance 
  -- recurse - increment distance, shifting currXY to prevXY and finding next as currXY
  | otherwise = traversePipes pipesMap (distance + 1) currXY $ findNext pipesMap prevXY currXY 

-- Find the next coordinate by looking at the two connections for a current pipe and eliminating the coordinate
-- that was passed in as prevXY
findNext pipesMap prevXY (currX, currY) = 
  fromJust $ find (/= prevXY) -- Find the coordinate that's not the prevXY
  $ connections -- The coordinates of its neighbors is in the 'connections' key of the Pipe object
  $ pipesMap ! (currX, currY) -- Get the pipe at the current location
  

-- Convert the input into a tuple containing:
--   - A Map with key (x,y) and value Pipe {label, connections}
--   - An (x,y) coordinate of the start point
parseInput input = (Map.fromList pipes, startCoord)
  where
    startCoord = fst $ fromJust $ find (\(_, pipe) -> label pipe == 'S') pipes
    pipes = map (toPipe rowLength) $ filter isNotDirt $ zip [0..] input
    isNotDirt (_, x) = x /= '.';
    rowLength = 1 + fromJust (elemIndex '\n' input)

-- Convert a single line index and char tuple to an (x,y) coordinate and Pipe object
-- which contains the original character and x,y coordinates of its 2 connections
toPipe rowLength (idx, char) = ((x, y), Pipe {
  label = char,
  connections = map getConnection $ dirsFromPipe char -- A char will give its dx,dy which will be converted to absolute x,y
})
  where
    getConnection (dx, dy) = (x + dx, y + dy)
    -- Convert a single line index to x,y coordinates
    x = idx `mod` rowLength
    y = idx `div` rowLength

-- Convert a pipe to its dx,dy neighbor positions
dirsFromPipe char = case char of
  '|' -> [(0, -1), (0, 1)]
  '-' -> [(-1, 0), (1, 0)]
  'L' -> [(0, -1), (1, 0)]
  'J' -> [(-1, 0), (0, -1)]
  '7' -> [(-1, 0), (0, 1)]
  'F' -> [(1, 0), (0, 1)]
  'S' -> []
  _ -> error "Unknown"