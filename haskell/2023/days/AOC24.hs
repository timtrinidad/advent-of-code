module AOC24 (day24) where
import Data.List.Split (wordsBy)
import Common (parseInt)
import Debug.Trace
import Text.Show.Pretty (ppShow)
import Data.Maybe (mapMaybe)
import Data.Either (fromRight)
import Data.List (intercalate)

data Point = Pt Int Int Int deriving (Show, Eq, Ord)
data Velocity = Vel Int Int Int deriving (Show, Eq, Ord)
type Stone = (Point, Velocity)

day24 :: (String -> String, String -> String)
day24 = (part1, part2)

-- Find the number of valid stone pairings
part1 :: String -> String
part1 input = ppShow $ length $ filter isValid $ mapMaybe findPathIntersectionsWithTime hailstonePairs
    where
        -- This pairing is valid only if both times are in the future and the point is within range
        isValid (x, y, time1, time2) =
            time1 > 0 &&
            time2 > 0 &&
            x >= rangeLow &&
            x <= rangeHigh &&
            y >= rangeLow &&
            y <= rangeHigh
        -- Defined by challenge
        rangeLow = if length hailstones < 10 then 7 else 200000000000000
        rangeHigh = if length hailstones < 10 then 27 else 400000000000000
        -- All possible pairs of hailstones (ignoring order)
        hailstonePairs = [(i, j) | i <- hailstones, j <- hailstones, i < j]
        hailstones = parseInput input

-- Take the first three rocks to generate 9 equations for 9 variables:
--   - the x y z starting position of the magic rock
--   - the a b c velocity of the magic rock
--   - the h i j times of impact with the first three hailstones
-- mfsolve wasn't properly solving this equation, so this outputs a python script to solve the system.
part2 :: String -> String
part2 input = intercalate "\n" cmd
    where
        cmd = [
                "",
                "python3 -c '",
                "from sympy import *",
                "a, b, c, h, i, j, x, y, z = symbols(\"a b c h i j x y z\")",
                "res = nonlinsolve([",
                "  x+a*h - " ++ show px1 ++ "-" ++ show vx1 ++ "*h,",
                "  y+b*h - " ++ show py1 ++ "-" ++ show vy1 ++ "*h,",
                "  z+c*h - " ++ show pz1 ++ "-" ++ show vz1 ++ "*h,",
                "  x+a*i - " ++ show px2 ++ "-" ++ show vx2 ++ "*i,",
                "  y+b*i - " ++ show py2 ++ "-" ++ show vy2 ++ "*i,",
                "  z+c*i - " ++ show pz2 ++ "-" ++ show vz2 ++ "*i,",
                "  x+a*j - " ++ show px3 ++ "-" ++ show vx3 ++ "*j,",
                "  y+b*j - " ++ show py3 ++ "-" ++ show vy3 ++ "*j,",
                "  z+c*j - " ++ show pz3 ++ "-" ++ show vz3 ++ "*j",
                "], (a, b, c, h, i, j, x, y, z)).args[0]",
                "print(res[6] + res[7] + res[8])",
                "';"
            ]

        [
            (Pt px1 py1 pz1, Vel vx1 vy1 vz1),
            (Pt px2 py2 pz2, Vel vx2 vy2 vz2),
            (Pt px3 py3 pz3, Vel vx3 vy3 vz3)
            ] = take 3 $ parseInput input

-- Given two input lines (two stones), determine at what point their paths
-- will cross and at what time for each stone. Returns Nothing if the two stones
-- never cross (parallel)
findPathIntersectionsWithTime :: (Stone, Stone) -> Maybe (Float, Float, Float, Float)
findPathIntersectionsWithTime (stone1, stone2)
    | slope1 == slope2 = Nothing
    | otherwise = Just (x, y, time1, time2)
    where
        (int1, slope1) = findInterceptAndSlope stone1
        (int2, slope2) = findInterceptAndSlope stone2
        x = (int2 - int1)/(slope1 - slope2)
        y = slope1 * x + int1
        time1 = findTimeForPoint stone1 (x, y)
        time2 = findTimeForPoint stone2 (x, y)

-- Find the 2D intercept and slope (i.e. y = mx + b form) for an input line
findInterceptAndSlope :: Stone -> (Float, Float)
findInterceptAndSlope (Pt x y _, Vel dx dy _) = (intercept, slope)
    where
        intercept = fromIntegral y - (slope * fromIntegral x)
        slope = fromIntegral dy/fromIntegral dx

-- Determine the time it took to get between the initial point and the given point
findTimeForPoint :: Stone -> (Float, Float) -> Float
findTimeForPoint (Pt x _ _, Vel dx _ _) (intX, _) = (intX - fromIntegral x) / fromIntegral dx

-- Parse the input into a list of points and their velocities
parseInput :: String -> [Stone]
parseInput = map processLine . lines
    where
        processLine = toData . map parseInt . wordsBy (`elem` ", @")
        toData [x, y, z, dx, dy, dz] = (Pt x y z, Vel dx dy dz)

