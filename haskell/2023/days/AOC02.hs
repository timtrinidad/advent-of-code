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
  print $ sum [1..length parsed] - sumInvalidGameIds

part2 input = do
  print "part2 not defined for day 02"

isValid [num, "red"] = parseInt num <= 12
isValid [num, "green"] = parseInt num <= 13
isValid [num, "blue"] = parseInt num <= 14

-- Results in an input like [(1, [["12", "red"], ["2", "green"], ["5", "green"]]), (2, [...])]
parseInput = map(\x -> do
    let [gameNum, sets] = splitOneOf ":" x
    let cubes = chunksOf 2 $ filter(not . null) $ splitOneOf ";, " $ sets
    (parseInt $ drop 5 gameNum, cubes)
  ) . lines


parseInt = read :: String->Int

