module AOC23 (day23) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Show.Pretty (ppShow)
import Data.Ord (comparing)

data Dir = North | East | South | West | None deriving (Show, Eq, Ord)
data Point = Pt Int Int deriving (Show, Eq, Ord)
type Mapping = Map Point Char

day23 :: (String -> String, String -> String)
day23 = (part1, part2)

part1 :: String -> String
part1 input = show $ findLongestPath mapping start end
    where
        start = Pt 1 0
        end = Pt (width - 2) (height - 1)
        (mapping, width, height) = parseInput input

part2 :: String -> String
part2 input = show $ findLongestPath mapping' start end
    where
        start = Pt 1 0
        end = Pt (width - 2) (height - 1)
        mapping' = removeSlopes mapping
        (mapping, width, height) = parseInput input

-- Find the longest path between the start and end point
-- by reducing the full map into nodes representing the intersections
-- and having the cost be the distance between each intersection
findLongestPath :: Mapping -> Point -> Point -> Int
findLongestPath mapping start end = maximum $ map sum $ traverseIntersections start end Set.empty [] intersectionDists
    where
        intersectionDists = Map.fromList $ map handleIntersection intersections
        handleIntersection intersection = (intersection, findIntersectionNeighbor mapping intersections 0 intersection)
        intersections = findIntersections mapping ++ [start, end]


-- DFS - Recursively find the distance from a given intersection to all its neighboring intersections
findIntersectionNeighbor :: Mapping -> [Point] -> Int -> Point -> [(Point, Int)]
findIntersectionNeighbor mapping intersections dist currPt 
    -- We've reached another intersection - return the neighboring intersection point and its distance
    | dist > 0 && currPt `elem` intersections = [(currPt, dist)]
    -- Recurse, incrementing distance and exploring all neighboars (should only branch on first call
    -- since it should return once it hits another branch/neighbor
    | otherwise = concatMap (findIntersectionNeighbor mapping' intersections (dist + 1)) $ findNeighbors mapping' currPt
    where
        -- Alternative to "Seen" - mark the previously traversed points as a wall
        mapping' = Map.insert currPt '#' mapping

-- DFS to traverse all intersections to find all possible path distances.
-- Returns an array of paths represented by the distances between intersections in the path.
traverseIntersections :: Point -> Point -> Set Point -> [Int] -> Map Point [(Point, Int)] -> [[Int]]
traverseIntersections curr end seen path intersections
    -- Reached the end - return the current path (distances)
    | curr == end = [path]
    -- Already seen this - quit immediately
    | Set.member curr seen = []
    -- Recurse, exploring each possible intersectino neighbor
    | otherwise = concatMap (\(dest, dist) -> traverseIntersections dest end seen' (path ++ [dist]) intersections) destinations
    where
        -- All possible other intersections from the current intersection
        destinations = intersections Map.! curr
        seen' = Set.insert curr seen

-- Find a list of all points that are intersections (more than 2 neighboring points)
findIntersections :: Mapping -> [Point]
findIntersections mapping = Map.keys $ Map.filterWithKey isIntersection mapping
    where
        -- wall - definitely not an intersection
        isIntersection _ '#' = False
        isIntersection pt _ = length (findNeighbors mapping pt) > 2

-- For part 2 - convert all slopes to regular points
removeSlopes :: Mapping -> Mapping
removeSlopes = Map.map (\x -> if x == '#' then '#' else '.')

-- For debugging - render a map with an O for the given path
renderMap :: Mapping -> [Point] -> Int -> Int -> String
renderMap mapping path width height = unlines $ map renderLine [0..height-1]
    where
        renderLine y = map (renderChar y) [0..width-1]
        renderChar y x
            | Pt x y `elem` path = 'O'
            | otherwise = mapping Map.! Pt x y

-- Given a mapping and a point, find all valid neighbors accounting
-- for slopes (being on one or trying to climb one), map edges, and walls
findNeighbors :: Mapping -> Point -> [Point]
findNeighbors mapping pt = map snd neighbors -- strip out relative direction
    where
        -- All valid neighbors
        neighbors = filter isValid $ map dirToPt $ validDirs currentPointType
        -- Is valid if we can move into the neighboring spot based on canTraverse
        isValid (dir, neighbor) = canTraverse (dir, Map.findWithDefault '#' neighbor mapping)
        currentPointType = mapping Map.! pt
        -- A neighbor and its relative direction to the current point
        dirToPt dir = (dir, transformPoint pt $ dirDelta dir)

-- Given a source direction and a new point, determine whether or not
-- we can traverse that point.
canTraverse :: (Dir, Char) -> Bool
canTraverse (_, '#') = False
canTraverse (North, 'v') = False
canTraverse (South, '^') = False
canTraverse (East, '<') = False
canTraverse (West, '>') = False
canTraverse _ = True

-- If we're at a point, determine valid directions (not accounting for walls)
validDirs :: Char -> [Dir]
validDirs '>' = [East]
validDirs '<' = [West]
validDirs 'v' = [South]
validDirs '^' = [North]
validDirs _ = [North, East, South, West]

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
    parseChar y x char = (Pt x y, char)




