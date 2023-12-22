{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module AOC21 (day21) where
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (find, nub)
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (traceShowId)
import Data.Bifunctor (second)
import Text.Show.Pretty (ppShow)

data Dir = North | East | South | West | None deriving (Show, Eq, Ord)
data Point = Pt Int Int deriving (Show, Eq, Ord)
type Mapping = Map Point Char

day21 :: (String -> String, String -> String)
day21 = (part1, part2)

part1 :: String -> String
part1 input = show $ length $ traceShowId
    $ filter (isValid numSteps) $ Map.assocs
    $ traverseMapping mapping Nothing numSteps [(0, startPt)] Map.empty
    where
        numSteps = 64
        startPt = findStart mapping
        (mapping, _, _) = parseInput input

-- Calculate up to 2.5 expansion for pattern finding
-- Returns the number of walkable points for each repetition of the map
part2 :: String -> String
part2 input = ppShow $ map (second length) $ Map.assocs
    $ groupByGrid (width, height) $ map fst
    $ filter (isValid numSteps) $ Map.assocs
    $ traverseMapping mapping (Just (width, height)) numSteps [(0, startPt)] Map.empty
    where
        numSteps = 327
        startPt = findStart mapping
        (mapping, width, height) = parseInput input

-- For each point, determine which repetition of the map it belongs to and group by it
groupByGrid :: Foldable t => (Int, Int) -> t Point -> Map Point [Point]
groupByGrid (width, height) = foldl processPt Map.empty
    where
        processPt acc pt = Map.insertWith (++) (gridPos pt) [pt] acc
        gridPos (Pt x y) =  Pt (x `div` width) (y `div` height)

-- Determine if the given point is actually valid for the number of goal steps
isValid :: Int -> (a, Int) -> Bool
isValid goalSteps (_, numSteps)
    | goalSteps == numSteps = True
    | even $ abs (goalSteps - numSteps) = True
    | otherwise = False

-- BFS to find all points and their distance within a max number of steps
-- Returns all the points that are reachable within the max and the number of steps it took to get there
traverseMapping :: Mapping -> Maybe (Int, Int) -> Int -> [(Int, Point)] -> Map Point Int -> Map Point Int
traverseMapping _ _ _ [] reachable = reachable
traverseMapping mapping dimensions maxSteps ((numSteps, currPt):restPts) reachable
    | Map.member currPt reachable = traverseMapping mapping dimensions maxSteps restPts reachable
    | otherwise = traverseMapping mapping dimensions maxSteps restPts' reachable'
    where
        restPts' = restPts ++ surroundingPts
        reachable' = Map.insert currPt numSteps reachable
        surroundingPts
            -- Within the max - out of the four surrounding points
            -- filter for ones we haven't already seen
            -- and associate the number of steps to get to it
            | numSteps < maxSteps = 
                map (\x -> (numSteps+1, x)) 
                $ filter (\x -> not (Map.member x reachable)) 
                $ findSurroundingPoints mapping dimensions currPt
            -- At the max, don't explore more points
            | otherwise = []

-- Given a point, find the walkable surrounding points
findSurroundingPoints :: Mapping -> Maybe (Int, Int) -> Point -> [Point]
findSurroundingPoints mapping dimensions currPt = filter isGardenPlot $ map (transformPoint currPt . dirDelta) [North, East, South, West]
    where
        isGardenPlot pt = Map.findWithDefault '#' (equivPt dimensions pt) mapping /= '#'
        -- If height/width are given, assume infinite map and "wrap" automatically
        equivPt (Just (width, height)) (Pt x y) = Pt (x `wrapNum` width) (y `wrapNum` height)
        -- Otherwise, don't wrap
        equivPt Nothing pt = pt

-- A version of `mod` that wraps negative numbers as well
wrapNum :: Integral a => a -> a -> a
wrapNum a b = if numMod < 0 then numMod + b else numMod
    where
        numMod = a `mod` b

-- Convert a direction to an x y delta
dirDelta :: Dir -> (Int, Int)
dirDelta North = (0, -1)
dirDelta East = (1, 0)
dirDelta South = (0, 1)
dirDelta West = (-1, 0)

-- Given a point and a delta, calculate the new point
transformPoint :: Point -> (Int, Int) -> Point
transformPoint (Pt x y) (dx, dy) = Pt (x+dx) (y+dy)

findStart :: Map Point Char -> Point
findStart mapping = fst $ fromJust $ find (\(_, char) -> char == 'S') $ Map.assocs mapping

parseInput :: String -> (Mapping, Int, Int)
parseInput input = (mapping, width, height)
    where
        width = length $ head allLines
        height = length allLines
        mapping = Map.fromList $ concat $ zipWith processLine [0..] allLines
        processLine y = zipWith (processChar y) [0..]
        processChar y x char = (Pt x y, char)
        allLines = lines input