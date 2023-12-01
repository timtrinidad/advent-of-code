module Days.AOC02 (day02) where

import Data.List.Split

day02 = (part1, part2)

part1 input = do
  let parsed = parse_input input
  print $ sum $ map p1_score parsed
    where p1_score [hand1, hand2] = hand_score (p1_hand hand2) + outcome_score (p1_hand hand1) (p1_hand hand2)

part2 input = do
  print "part2 not defined for day 02"


data Hand = Rock | Paper | Scissors deriving (Enum)

p1_hand :: String -> Hand
p1_hand "A" = Rock
p1_hand "X" = Rock
p1_hand "B" = Paper
p1_hand "Y" = Paper
p1_hand "C" = Scissors
p1_hand "Z" = Scissors

hand_score :: Hand -> Int
hand_score Rock = 1
hand_score Paper = 2
hand_score Scissors = 3

outcome_score :: Hand -> Hand -> Int
outcome_score Rock Rock = 3
outcome_score Rock Paper = 6
outcome_score Rock Scissors = 0
outcome_score Paper Rock = 0
outcome_score Paper Paper = 3
outcome_score Paper Scissors = 6
outcome_score Scissors Rock = 6
outcome_score Scissors Paper = 0
outcome_score Scissors Scissors = 3

parse_input = map(splitOn " ") . lines
