module AOC18 (day18) where

day18 :: (String -> String, String -> String)
day18 = (part1, part2)

part1 :: String -> String
part1 = show . parseInput 

part2 :: String -> String
part2 = show . parseInput

parseInput :: String -> [String]
parseInput = lines

