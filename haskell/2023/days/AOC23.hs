{-# LANGUAGE RecordWildCards #-}
module AOC23 (day23) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Common (parseInt)
import Data.List (sort, maximumBy)
import Debug.Trace
import Text.Show.Pretty (ppShow)
import Data.Ord (comparing)

data Dir = North | East | South | West | None deriving (Show, Eq, Ord)
data Point = Pt Int Int deriving (Show, Eq, Ord)
type Mapping = Map Point Char

day23 :: (String -> String, String -> String)
day23 = (part1, part2)

part1 :: String -> String
part1 input = show $ length path
    -- renderMap mapping path width height
    where
        path = traverseMapping mapping end start
        start = Pt 1 1
        end = Pt (width - 2) (height - 1)
        (mapping, width, height) = parseInput input

renderMap mapping path width height = unlines $ map renderLine [0..height-1]
    where
        renderLine y = map (renderChar y) [0..width-1]
        renderChar y x
            | Pt x y `elem` path = 'O'
            | otherwise = mapping Map.! Pt x y

traverseMapping mapping end currPt
    | currPt == end = [currPt]
    | null neighbors = []
    | null pathOptions = []
    | otherwise = currPt : maximumBy (comparing length) pathOptions
    where
        pathOptions = filter (not . null) $ map traverseNeighbor neighbors
        traverseNeighbor = traverseMapping mapping' end
        neighbors = findNeighbors mapping' currPt
        mapping' = Map.insert currPt '#' mapping


findNeighbors mapping pt = map snd neighbors
    where
        neighbors = filter isValid $ map dirToPt $ validDirs char
        isValid (dir, neighbor) = canTraverse (dir, Map.findWithDefault '#' neighbor mapping)  
        char = mapping Map.! pt
        dirToPt dir = (dir, transformPoint pt $ dirDelta dir)

-- getTopologicalOrder _ [] _ order = order
-- getTopologicalOrder mapping (currPt:restPts) num order
--     | Map.member currPt order = getTopologicalOrder mapping restPts num order
--     | otherwise = getTopologicalOrder mapping restPts' num' order'
--     where
--         num' = num + 1
--         restPts' = restPts ++ findNeighbors mapping currPt
--         order' = traceShow (currPt, num) $ Map.insert currPt num order

canTraverse (_, '#') = False
canTraverse (North, 'v') = False
canTraverse (South, '^') = False
canTraverse (East, '<') = False
canTraverse (West, '>') = False
canTraverse _ = True

validDirs '>' = [East]
validDirs '<' = [West]
validDirs 'v' = [South]
validDirs '^' = [North]
validDirs _ = [North, East, South, West]

part2 :: String -> String
part2 = show . parseInput

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




