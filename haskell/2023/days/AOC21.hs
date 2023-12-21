{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module AOC21 (day21) where
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (find, nub)
import Data.Maybe (fromJust, fromMaybe)
import Algorithm.Search (dijkstra)
import Debug.Trace (traceShowId)

data Dir = North | East | South | West | None deriving (Show, Eq, Ord)
data Point = Pt Int Int deriving (Show, Eq, Ord)
type Mapping = Map Point Char

day21 :: (String -> String, String -> String)
day21 = (part1, part2)

part1 :: String -> String
part1 input = show $ length $ traceShowId $ filter (isValid numSteps) $ Map.assocs $ traverseMapping mapping numSteps [(0, startPt)] Map.empty
    where
        numSteps = 64
        startPt = findStart mapping
        mapping = parseInput input

part2 :: String -> String
part2 = show . parseInput

isValid goalSteps (_, numSteps)
    | goalSteps == numSteps = True
    | even (goalSteps - numSteps) = True
    | otherwise = False

traverseMapping _ _ [] reachable = reachable
traverseMapping mapping maxSteps ((numSteps, currPt):restPts) reachable = traverseMapping mapping maxSteps restPts' reachable'
    where
        restPts' = nub $ restPts ++ surroundingPts
        reachable' = Map.insert currPt numSteps reachable
        surroundingPts
            | numSteps < maxSteps = map (\x -> (numSteps+1, x)) $ filter (\x -> not (Map.member x reachable) && Map.findWithDefault '#' x mapping == '.') $ map (transformPoint currPt . dirDelta) [North, East, South, West]
            | otherwise = []


-- Convert a direction to an x y delta
dirDelta :: Dir -> (Int, Int)
dirDelta North = (0, -1)
dirDelta East = (1, 0)
dirDelta South = (0, 1)
dirDelta West = (-1, 0)

-- Given a point and a delta, calculate the new point
transformPoint :: Point -> (Int, Int) -> Point
transformPoint (Pt x y) (dx, dy) = Pt (x+dx) (y+dy)

findStart mapping = fst $ fromJust $ find (\(_, char) -> char == 'S') $ Map.assocs mapping

parseInput :: String -> Mapping
parseInput = Map.fromList . concat . zipWith processLine [0..] . lines
    where
        processLine y = zipWith (processChar y) [0..]
        processChar y x char = (Pt x y, char)


