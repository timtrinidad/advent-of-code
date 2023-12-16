module AOC19 (day19) where

day19 :: (String -> String, String -> String)
day19 = (part1, part2)

part1 :: String -> String
part1 = show . parseInput 

part2 :: String -> String
part2 = show . parseInput

parseInput :: String -> [String]
parseInput = lines

