module AOC18 (day18) where

import Data.List.Split (wordsBy)
import Data.List (scanl')
import Common (parseInt)
import Numeric (readHex)

data Dir = DirUp | DirDown | DirLeft | DirRight deriving (Show, Eq, Ord)
data Point = Pt Int Int deriving (Show, Eq, Ord)
data Instruction = Inst Dir Int deriving (Show)

day18 :: (String -> String, String -> String)
day18 = (part1, part2)

part1 :: String -> String
part1 input = show $ totalArea $ parseInput1 input

part2 :: String -> String
part2 input = show $ totalArea $ parseInput2 input

-- Use Pick's theorem to calculate the total area including the perimeter
totalArea :: [Instruction] -> Int
totalArea instructions = shoelaceArea vertices + perimeter instructions `div` 2 + 1
    where
        vertices = init $ scanl' processInst (Pt 0 0) instructions
        processInst currPt (Inst dir dist) = transformPoint currPt dir dist

-- Calculate the perimeter of the polygon - sum of all dists
perimeter :: [Instruction] -> Int
perimeter = sum . map (\(Inst _ dist) -> dist)

-- Calculate the shoelace area of the polygon (doesn't include the perimeter)
shoelaceArea :: [Point] -> Int
shoelaceArea vertices = sums `div` 2
    where
        sums = sum $ zipWith processVtx vertices $ tail vertices
        processVtx (Pt x y) (Pt x' y') = x * y' - y * x'

-- Given a point, direction, and distance, calculate the new point
transformPoint :: Point -> Dir -> Int -> Point
transformPoint (Pt x y) DirUp dist = Pt x (y-dist)
transformPoint (Pt x y) DirDown dist = Pt x (y+dist)
transformPoint (Pt x y) DirLeft dist = Pt (x-dist) y
transformPoint (Pt x y) DirRight dist = Pt (x+dist) y

-- Parse the first two columns into a set of instructions
parseInput1 :: String -> [Instruction]
parseInput1 = map (processLine . wordsBy (`elem` " (#)")) . lines
    where
        processLine [letter, dist, _] = Inst (dir letter) (parseInt dist)
        dir x = case x of
            "U" -> DirUp
            "D" -> DirDown
            "L" -> DirLeft
            "R" -> DirRight

-- Parse the hexidecimal "Color" into a set of instructions
parseInput2 :: String -> [Instruction]
parseInput2 = map (processLine . wordsBy (`elem` " (#)")) . lines
    where
        processLine [_, _, hex] = Inst(dir $ last hex) (calcDist $ take 5 hex)
        calcDist hexDist = fst $ head $ readHex hexDist
        dir x = case x of
            '3' -> DirUp
            '1' -> DirDown
            '2' -> DirLeft
            '0' -> DirRight