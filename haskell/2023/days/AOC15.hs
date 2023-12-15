module AOC15 (day15) where
import Data.List.Split (splitOn)
import Data.Char (ord)

day15 = (part1, part2)

part1 input = show $ sum $ map hash $ parseInput input

part2 input = do
  show "part2 not defined for day 15"

hash seq = foldl processChar 0 seq
  where processChar acc char = (acc + ord char) * 17 `mod` 256

parseInput input = splitOn "," input