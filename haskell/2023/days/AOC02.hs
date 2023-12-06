module Days.AOC02 (day02) where

import Data.List.Split (splitOneOf, chunksOf)

day02 = (part1, part2)

part1 input = do
  let parsed = parseInput input
  -- Find all games that are invalid - for each game, filter to only invalid draws and then filter
  -- to only games with any invalid draws
  let invalidGames = filter (\(gameNum, draws) -> (length(filter(not . isValid) draws) > 0)) parsed
  let sumInvalidGameIds = sum $ map(fst) invalidGames
  -- Find the inverse - sum of valid games
  show $ sum [1..length parsed] - sumInvalidGameIds

part2 input = do
  let parsed = parseInput input
  --
  show $ sum $ map(product . findMaxPerColor) parsed

isValid [num, "red"] = parseInt num <= 12
isValid [num, "green"] = parseInt num <= 13
isValid [num, "blue"] = parseInt num <= 14

-- Returns a list of the max number of colors for a given game
findMaxPerColor :: (Int, [[String]]) -> [Int]
findMaxPerColor (_, draws) =
  -- For each color
  map(\color ->
    -- Reduce to the max
    maximum
    -- Get the number of that color
    $ map(\[num, _] -> parseInt num)
    -- Filter all num/color pairs to the selected color
    $ filter(\x -> x !! 1 == color)
    draws
  )
  ["red", "green", "blue"]

-- Results in an input like [(1, [["12", "red"], ["2", "green"], ["5", "green"]]), (2, [...])]
parseInput :: String -> [(Int, [[String]])]
parseInput = map(\x -> do
    -- Separate out the game number
    let [gameNum, sets] = splitOneOf ":" x
    -- Difference between colors and draws don't matter - get a list of number/color pairs in the whole game
    let cubes = chunksOf 2 $ filter(not . null) $ splitOneOf ";, " $ sets
    -- Tuple with game number as int and array of num/color pairs
    (parseInt $ drop 5 gameNum, cubes)
  ) . lines


parseInt = read :: String->Int

