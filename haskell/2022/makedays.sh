#!/usr/bin/env bash

solutions="days"

mkdir "$solutions"

for i in $(printf "%02i " {1..24})
do
  cat << EOF > "$solutions/AOC$i.hs"
module Days.AOC$i (day$i) where

day$i = (part1, part2)

part1 input = do
  print "part1 not defined for day $i"

part2 input = do
  print "part2 not defined for day $i"
EOF
done
