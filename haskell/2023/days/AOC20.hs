module AOC20 (day20) where

day20 :: (String -> String, String -> String)
day20 = (part1, part2)

part1 :: String -> String
part1 = show . parseInput 

part2 :: String -> String
part2 = show . parseInput

parseInput :: String -> [String]
parseInput = lines

