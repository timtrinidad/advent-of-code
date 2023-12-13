module AOC13 (day13) where

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (find, transpose)
import Debug.Trace

day13 :: (String -> String, String -> String)
day13 = (part1, part2)

-- Sum each graph's vertex value
part1 :: String -> String
part1 input = show $ sum $ graphValues 0 graphs
  where graphs = parseInput input

-- Sum each graph's vertext value allowing for 1 incorrect match on reflections
part2 :: String -> String
part2 input = show $ sum $ graphValues 1 graphs
  where graphs = parseInput input

graphValues :: Int -> [(Map (Int, Int) Char, (Int, Int))] -> [Int]
graphValues numIncorrectAllowed = map findVertices
  where
    -- Multiply vertices as necessary - only one vertical or horizontal should be found
    -- so one side of + should always be 0
    findVertices graph = maybe 0 (+1) (findVertex' graph) -- no multiplication
      + maybe 0 (\x -> (x+1)*100) (findVertex' $ transposed graph) -- multiply by 100
    findVertex' = findVertex numIncorrectAllowed
    -- Rotate a graph 90 degrees
    transposed (diagram, (width, height)) = (Map.mapKeys (\(x, y) -> (y, x)) diagram, (height, width))

-- Iterate through all possible vertices looking for a reflection
findVertex :: Int -> (Map (Int, Int) Char, (Int, Int)) -> Maybe Int
findVertex numIncorrectAllowed (diagramMap, (width, _)) = find isValidVertex [0..width-2]
  where
    -- Is valid if no unmatched (part 1) or only 1 unmatched (part 2)
    isValidVertex vtx =  numIncorrectAllowed == numIncorrectMatches vtx
    -- Find the number of points that don't match
    numIncorrectMatches vtx = length $ filter not $ Map.elems $ Map.mapWithKey (checkReflection vtx) $ Map.filterWithKey (\(x, _) _ -> x <= vtx) diagramMap
    -- Check of the reflected point is the same charachter
    checkReflection vtx point char = char == Map.findWithDefault char (reflectAcross vtx point) diagramMap
    reflectAcross vtx (x, y) = (2*vtx+1 - x, y) -- Reflect a point across a given vertex


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