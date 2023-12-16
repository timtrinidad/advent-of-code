module AOC11 (day11) where

import Data.List (foldr, transpose)
import Debug.Trace

day11 :: (String -> String, String -> String)
day11 = (part1, part2)


-- calculate sum of distances with each space being doubled
part1 :: String -> String
part1 input = show $ galaxyDistances input 2
-- calculate sum of distances with each space being multiplied by 1m
part2 :: String -> String
part2 input = show $ galaxyDistances input 1000000

-- Sum the manhattan distances between all pairs of galaxies
galaxyDistances :: String -> Int -> Int
galaxyDistances input multiplier = sum distances
  where
    distances = map (\(i, j) -> calcDistance emptyRows emptyCols (multiplier - 1) (galaxies !! i) (galaxies !! j)) pairs -- List of distances of each pair
    pairs = [(i, j) | i <- [0..numGalaxies-1], j <- [0..numGalaxies-1], i < j] -- all index pairs ignoring order
    numGalaxies = length galaxies
    (galaxies, emptyRows, emptyCols) = parseInput input

-- Calculate the manhattan distance between two points, adding padding based on the empty columns/rows
calcDistance :: [Int] -> [Int] -> Int -> (Int, Int) -> (Int, Int) -> Int
calcDistance emptyRows emptyCols multiplier (x1, y1) (x2, y2) =  dx + (multiplier * colPadding) + dy + (multiplier * rowPadding)
  where
    dx = abs (x2 - x1) -- diff of xs
    dy = abs (y2 - y1) -- diff of ys
    colPadding = length $ filter (\col -> col > min y1 y2 && col < max y1 y2) emptyCols -- num cols between y points
    rowPadding = length $ filter (\row -> row > min x1 x2 && row < max x1 x2) emptyRows -- num rows between x points

-- Parse the input into 
--   - a list of points of galaxies in the unexpanded map
--   - a list of empty column indices
--   - a list of empty row indices.
parseInput :: String -> ([(Int, Int)], [Int], [Int])
parseInput input = (galaxyPoints, emptyCols, emptyRows)
  where
    emptyCols = emptyIdxs $ transpose grid -- list of empty column indices
    emptyRows = emptyIdxs grid -- List of empty row indices
    emptyIdxs = map fst . filter isEmpty . zip [0..] -- Indices of empty lines
    isEmpty = all (== '.') . snd -- Entire line is periods
    galaxyPoints = concatMap processRow $ zip [0..] grid -- list of points of galaxies in unexpanded map
    processRow (y, row) = map (processCol y) $ filter (\(_, char) -> char /= '.') $ zip [0..] row -- filter out periods and extract coordinates
    processCol y (x, _) = (x, y) -- convert the rol/col nums into an x,y point, ignoring the actual character
    grid = lines input -- split into lines