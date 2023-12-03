#!/usr/bin/env stack
{- stack script
   --resolver lts-21.13
   --package turtle
   --package base
   --package split
   --package mr-env
   --package text
   --package containers
   --package timeit
   --package MissingH
   --package regex-tdfa
-}
{-# LANGUAGE OverloadedStrings #-}
import System.Environment ( getArgs )
import System.Environment.MrEnv ( envAsString )
import Data.IntMap.Strict qualified as M
import Days (getDay)
import Data.Typeable
import System.TimeIt
import Text.Printf
import Data.List

main :: IO()

loadFile dayNum [_, "sample"] = do
  let fileName = "inputs/" ++ dayNum ++ ".sample.txt"
  print $ "Loading input from " ++ fileName
  readFile fileName
loadFile dayNum [_] = do
  let fileName = "inputs/" ++ dayNum ++ ".txt"
  print $ "Loading input from " ++ fileName
  readFile fileName

execPart input day "1"  = (fst day) input
execPart input day "2" = (snd day) input

main = do
  args <- getArgs
  dayString <- envAsString "DAY" "01"
  input <- loadFile dayString args
  printf "Day %s Part %s solution: " dayString (intercalate " " args)
  (time, _) <- timeItT $ execPart input (getDay dayString) (head args)
  printf ("CPU Time: %6.3fms\n") $ time * 1000
