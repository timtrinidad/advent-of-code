module AOC23 (day23) where

day23 :: (String -> String, String -> String)
day23 = (part1, part2)

part1 :: String -> String
part1 = show . parseInput 

part2 :: String -> String
part2 = show . parseInput

parseInput :: String -> [String]
parseInput = lines

