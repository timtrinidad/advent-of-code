module AOC13 (day13) where

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (find)
import Debug.Trace

data VertexType = Vertical | Horizontal deriving (Read, Show, Enum, Eq, Ord)

day13 :: (String -> String, String -> String)
day13 = (part1, part2)

-- Sum each graph's vertex vaule
part1 :: String -> String
part1 input = show $ sum $ map findVertices graphs
  where
    -- Multiply vertices as necessary - only one vertical or horizontal should be found
    -- so one side of + should always be 0
    findVertices graph = maybe 0 (+1) (verticalVtx graph) -- no multiplication
      + maybe 0 (\x -> (x+1)*100) (horizontalVtx graph) -- multiply by 100
    verticalVtx = findVertex Vertical
    horizontalVtx = findVertex Horizontal
    graphs = parseInput input

part2 :: String -> String
part2 input = do
  show "part2 not defined for day 13"


-- Iterate through all possible vertices looking for a reflection
findVertex :: VertexType -> (Map (Int, Int) Char, (Int, Int)) -> Maybe Int
findVertex vertexType (diagramMap, dimensions) = find isValidVertex [0..idx dimensions-2]
  where
    -- The vertex at a given index is valid if all the points across the reflection are equal (or empty)
    isValidVertex vtx =  and $ Map.elems $ Map.mapWithKey (checkReflection vtx) $ Map.filterWithKey (\point _ -> idx point <= vtx) diagramMap
    -- Check of the reflected point is the same charachter
    checkReflection vtx point char = char == Map.findWithDefault char (reflect vtx point) diagramMap
    reflect = reflectAcross vertexType -- function to reflect across the vertex type
    idx = if vertexType == Horizontal then snd else fst -- (x, y) - horizontal line is second, vertical is first

-- Reflect a point across a given vertex
reflectAcross Vertical vtx (x, y) = (2*vtx+1 - x, y)
reflectAcross Horizontal vtx (x, y) = (x, 2*vtx+1 - y)

-- Split into multiple diagrams and process each
parseInput :: String -> [(Map (Int, Int) Char, (Int, Int))]
parseInput input = map processDiagram $ splitOn "\n\n" input


-- Process a single diagram
processDiagram :: [Char] -> (Map (Int, Int) Char, (Int, Int))
processDiagram diagram = (points, dimensions)
  where
    -- Calculate dimentions of this specific diagram
    dimensions = (length $ head diagramLines, length diagramLines)
    -- Generate a Map that is keyed by the coordinates of each character
    points = Map.fromList $ concatMap processLine $ zip [0..] diagramLines
    diagramLines = splitOn "\n" diagram
    processLine (y, line) = zipWith (\x char -> ((x, y), char)) [0..] line