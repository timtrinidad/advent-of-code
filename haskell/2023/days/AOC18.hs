module AOC18 (day18) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split (wordsBy)
import Data.List ( nub, foldl' )
import Common (parseInt)
import Debug.Trace (traceShowId, traceShow)

type Color = String
type Mapping = Map Point Color
data Dir = North | East | South | West | None deriving (Show, Eq, Ord)
data Point = Pt Int Int deriving (Show, Eq, Ord)
data Instruction = Inst Dir Int Color deriving (Show)

day18 :: (String -> String, String -> String)
day18 = (part1, part2)

part1 :: String -> String
part1 input = show $ Map.size $ floodFill mapping [Pt 1 1]
    where
        mapping = snd $ generateBorder $ parseInput input

part2 :: String -> String
part2 = show . parseInput

render :: Mapping -> String
render mapping = unlines $ map renderRow [yMin..yMax]
    where
        renderRow y = traceShowId $ map (renderCol y) [xMin..xMax]
        renderCol 0 0 = '+'
        renderCol y x = if Map.member (Pt x y) mapping then '#' else '.'
        (xMin, xMax) = (minimum xRange, maximum xRange)
        (yMin, yMax) = (minimum yRange, maximum yRange)
        xRange = map (\(Pt x _) -> x) $ Map.keys mapping
        yRange = map (\(Pt _ y) -> y) $ Map.keys mapping

generateBorder :: [Instruction] -> (Point, Mapping)
generateBorder = foldl' processInstruction (Pt 0 0, Map.empty)
    where
        processInstruction acc (Inst dir dist color) = foldl' (dig dir color) acc [1..dist]


dig :: Dir -> Color -> (Point, Mapping) -> Int -> (Point, Mapping)
dig dir color (currPt, mapping) _ = (nextPt, Map.insert nextPt color mapping)
    where
        nextPt = transformPoint currPt (dirDelta dir)

floodFill :: Mapping -> [Point] -> Mapping
floodFill mapping [] = mapping
floodFill mapping (currPt:nextPts) = floodFill mapping' (nub $ nextPts ++ addlPts)
    where
        mapping' = Map.insert currPt "000000" mapping
        addlPts = filter (\x -> not $ Map.member x mapping) $ map (transformPoint currPt . dirDelta) [North, South, East, West]

dirDelta :: Dir -> (Int, Int)
dirDelta North = (0, -1)
dirDelta East = (1, 0)
dirDelta South = (0, 1)
dirDelta West = (-1, 0)

-- Given a point and a delta, calculate the new point
transformPoint :: Point -> (Int, Int) -> Point
transformPoint (Pt x y) (dx, dy) = Pt (x+dx) (y+dy)

parseInput :: String -> [Instruction]
parseInput = map (processLine . wordsBy (`elem` " (#)")) . lines
    where
        processLine [letter, dist, color] = Inst (dir letter) (parseInt dist) color
        dir x = case x of
            "U" -> North
            "D" -> South
            "L" -> West
            "R" -> East

