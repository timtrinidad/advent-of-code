module AOC21 (day21) where

day21 :: (String -> String, String -> String)
day21 = (part1, part2)

part1 :: String -> String
part1 = show . parseInput 

part2 :: String -> String
part2 = show . parseInput

parseInput :: String -> [String]
parseInput = lines

