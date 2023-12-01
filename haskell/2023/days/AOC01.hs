module Days.AOC01 (day01) where

import Data.Char
import Text.Regex.TDFA
import Data.List.Utils
import qualified Data.Text as T
import Data.Text.Internal.Search


day01 = (part1, part2)

part1 input = do
  print $ sumNums $ lines input

part2 input = do
  print $ sumNums $ map replaceNums $ lines input

-- Gets first and last int chars, converts them to an integer, and sums them
sumNums = sum . map(\x -> read [head x, last x]) . map(filter isDigit)

-- Recursively replace the first spelled out number and replace it with its digit
replaceNums string = do
  let match = string =~ "one|two|three|four|five|six|seven|eight|nine" :: String
  if length match == 0
    -- base case
    then do
      string
    -- Find the index of the match and replace its chars with its digit
    else do
      let idx = head $ indices (T.pack match) (T.pack string)
      let left = take (idx) string
      -- Replace only its first two characters with its digit
      -- not the whole word in case its letters are shared with another number
      let right = drop (idx + 1) string
      -- Recurse
      replaceNums $ left ++ (replaceNum match) ++ right

replaceNum x
  | x == "one" = "1"
  | x == "two" = "2"
  | x == "three" = "3"
  | x == "four" = "4"
  | x == "five" = "5"
  | x == "six" = "6"
  | x == "seven" = "7"
  | x == "eight" = "8"
  | x == "nine" = "9"



