module AOC10 (day10) where
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
    loopDistance = length $ traversePipes pipesMap [] startPoint startPoint secondPoint
    -- Find the pipes connecting to the start coordinate and arbitrarily select the first one
    secondPoint = head $ connections $ pipesMap ! startPoint
    (pipesMap, startPoint, _) = parseInput input

-- Count the number of inside points
part2 input = show $ countInsidePoints path pipesMap allPoints currDy isInside insideCounter
  where
    allPoints = [(x, y) | y <- [0..mapHeight-1], x <- [0..mapWidth-1]]
    -- Initial values
    insideCounter = 0 -- count the number of inside points
    currDy = 0 -- Current running tracker of vertical line source
    isInside = False -- whether or not we're currently inside the path
    -- Like part 1, determine the path
    path = traversePipes pipesMap [] startPoint startPoint secondPoint
    secondPoint = head $ connections $ pipesMap ! startPoint
    (pipesMap, startPoint, (mapWidth, mapHeight)) = parseInput input

-- Recursively look through every point, determining at each point if we cross into the inside or outside of the path
countInsidePoints _ _ [] _ _ insideCounter = insideCounter -- base case - no more points to explore
countInsidePoints path pipesMap (x:xs) currDy isInside insideCounter
  -- If this point is not part of the path, increment counter if we're inside the path
  | x `notElem` path = countInsidePoints path pipesMap xs currDy isInside $ insideCounter + if isInside then 1 else 0
  -- Don't increment counter but determine whether or not we're crossing into or out of the path
  | otherwise = countInsidePoints path pipesMap xs currDy' isInside' insideCounter
  where
    -- running sum of vertical direction. If the running sum goes to 2 or -2, we know the path turned around
    -- and therefore we're not crossing into or out of the path.
    -- If it's 1 or -1 and evens out (e.g. L--7) where L is -1 and 7 is 1, we know it's effectively a vertical
    -- line (across mnultiple x points) and we're crossing a border.
    currDy' = if connectionsSumDy == currDy then 0 else connectionsSumDy + currDy
    -- Flip the 'isInside' flag if the sum was -1 and we hit 1 or vice versa, or if we see a vertical pipe
    isInside' = if connectionsSumDy == -1 * currDy || label currPipe == '|' then not isInside else isInside
    -- For the two points connecting to this pipe, determine if it's coming from up or down.
    -- If it's coming from up to left/right, it will be -1. From down, 1.
    connectionsSumDy = sum $ map snd $ dirsFromPipe $ label currPipe 
    currPipe = pipesMap ! x -- the pipe at the current point. This won't ever error since we know at this point we're part of the path




-- Recursively travese the pipesMap until we get to a pipe labelled 'S'
traversePipes pipesMap path startPoint prevPoint currPoint
  -- base case - return current distance
  | currPoint == startPoint = (currPoint:path)
  -- recurse - increment distance, shifting currPoint to prevPoint and finding next as currPoint
  | otherwise = traversePipes pipesMap (currPoint:path) startPoint currPoint $ findNext pipesMap prevPoint currPoint 
  where 
    -- Find the next coordinate by looking at the two connections for a current pipe and eliminating the coordinate
    -- that was passed in as prevPoint
    findNext pipesMap prevPoint (currX, currY) = 
      fromJust $ find (/= prevPoint) -- Find the coordinate that's not the prevPoint
      $ connections -- The coordinates of its neighbors is in the 'connections' key of the Pipe object
      $ pipesMap ! (currX, currY) -- Get the pipe at the current location

  

-- Convert the input into a tuple containing:
--   - A Map with key (x,y) and value Pipe {label, connections}
--   - An (x,y) coordinate of the start point
parseInput input = (pipesMap', startPoint, mapDimensions)
  where
    -- Update the pipesMap to put in the connections for the starting point
    pipesMap' = Map.insert startPoint Pipe { label = 'S', connections = findConnections pipesMap startPoint } pipesMap
    pipesMap = Map.fromList pipes
    mapDimensions = (mapWidth, mapHeight)
    mapWidth = rowLength - 1 -- rowLength contains newline
    mapHeight = (1 + length input) `div` rowLength -- Every line has newline at the end except for last line
    startPoint = fst $ fromJust $ find (\(_, pipe) -> label pipe == 'S') pipes -- find the point where char is 'S'
    pipes = map (toPipe rowLength) $ filter isNotDirt $ zip [0..] input -- list of pipes
    isNotDirt (_, x) = x /= '.';
    rowLength = 1 + fromJust (elemIndex '\n' input)

-- Convert a single line index and char tuple to an (x,y) coordinate and Pipe object
-- which contains the original character and x,y coordinates of its 2 connections
toPipe rowLength (idx, char) = ((x, y), Pipe {
  label = char,
  -- A char will give its dx,dy which will be converted to absolute x,y
  connections = map getConnection $ dirsFromPipe char 
})
  where
    getConnection (dx, dy) = (x + dx, y + dy)
    -- Convert a single line index to x,y coordinates
    x = idx `mod` rowLength
    y = idx `div` rowLength

-- Convert a pipe to its dx,dy neighbor positions
dirsFromPipe char = case char of
  '|' -> [(0, -1), (0, 1)] -- top/down
  '-' -> [(-1, 0), (1, 0)] -- left/right
  'L' -> [(0, -1), (1, 0)] -- top/right
  'J' -> [(-1, 0), (0, -1)] -- top/left
  '7' -> [(-1, 0), (0, 1)] -- down/left
  'F' -> [(1, 0), (0, 1)] -- down/right
  'S' -> []
  _ -> error "Unknown"

-- Find the connections for a given location by traversing all possible neighbors
-- and filtering for which ones have the current (x,y) coordinates in its connections
findConnections pipesMap (x, y) = filter doesConnect neighbors
  where
    -- Whether or not the current location is in the list of connections for the pipe at a given point
    doesConnect neighborXY = (x, y) `elem` connections (pipesMap ! neighborXY)
    -- All neighbors that have a non-dirt entry in pipesMap
    neighbors = filter (`Map.member` pipesMap) [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1]]
