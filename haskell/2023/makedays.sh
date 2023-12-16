#!/usr/bin/env bash

solutions="days"

mkdir "$solutions"

for i in $(printf "%02i " {1..24})
do
  cat << EOF > "$solutions/AOC$i.hs"
module AOC$i (day$i) where

day$i :: (String -> String, String -> String)
day$i = (part1, part2)

part1 :: String -> String
part1 = show . parseInput 

part2 :: String -> String
part2 = show . parseInput

parseInput :: String -> [String]
parseInput = lines

EOF
done
