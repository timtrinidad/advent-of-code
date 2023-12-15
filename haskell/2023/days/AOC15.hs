{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module AOC15 (day15) where
import Data.List.Split (splitOn, splitOneOf)
import Data.Char (ord)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Ordered (OMap)
import qualified Data.Map.Ordered as OMap
import Common (parseInt)
import Debug.Trace

day15 :: (String -> String, String -> String)
day15 = (part1, part2)

-- Sum of hashes of each string
part1 :: String -> String
part1 input = show $ sum $ map hash $ parseInput input

-- Calculate total focusing power of all lenses
part2 :: String -> String
part2 input = show $ totalFocusingPower $ foldl processInstruction Map.empty instructions
  where
    -- Results in an list of (label, focalLength) tuples
    instructions = map (toTuple . processInst) $ parseInput input
    -- Ignoring "-" or "=" - if it has a number at the end, it's an "add" otherwise "delete"
    toTuple [label, focalLength] = (label, if null focalLength then 0 else  parseInt focalLength)
    processInst = splitOneOf "-="

-- Add the instruction into a single ordered list
processInstruction :: Map Int (OMap String Int) -> (String, Int) -> Map Int (OMap String Int)
processInstruction boxes (label, focalLength)
  | focalLength == 0 = Map.insert lensHash (OMap.delete label box) boxes
  | otherwise = Map.insert lensHash ((label, focalLength) OMap.<| box) boxes
  where
    box = Map.findWithDefault OMap.empty lensHash boxes
    lensHash = hash label

-- Calculate total focusing power of all lenses
totalFocusingPower :: Map Int (OMap String Int) -> Int
totalFocusingPower boxes = sum $ map processBox $ Map.assocs boxes
  where
    -- Add index (lens number) to each lens and calculate power
    processBox (boxNum, box) = sum $ zipWith (processLens boxNum) [1..] $ reverse $ OMap.assocs box
    processLens boxNum lensNum (_, focalLength) = (boxNum + 1) * lensNum * focalLength -- defined by challenge

-- Calculate the hash for a given string
hash :: String -> Int
hash = foldl processChar 0
  where processChar acc char = (acc + ord char) * 17 `mod` 256

parseInput :: String -> [String]
parseInput = splitOn ","