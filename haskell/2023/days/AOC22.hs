module AOC22 (day22) where

day22 :: (String -> String, String -> String)
day22 = (part1, part2)

part1 :: String -> String
part1 = show . parseInput 

part2 :: String -> String
part2 = show . parseInput

parseInput :: String -> [String]
parseInput = lines

