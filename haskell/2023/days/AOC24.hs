module AOC24 (day24) where

day24 :: (String -> String, String -> String)
day24 = (part1, part2)

part1 :: String -> String
part1 = show . parseInput 

part2 :: String -> String
part2 = show . parseInput

parseInput :: String -> [String]
parseInput = lines

