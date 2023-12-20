module AOC19 (day19) where
import Data.List.Split (splitOn, wordsBy, chunksOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Common (parseInt)
import Data.List (find, mapAccumL, groupBy)
import Data.Maybe (fromJust)
import Text.Show.Pretty (ppShow)
import Debug.Trace (traceShowId)
import qualified Data.Range as Range
import Data.Bifunctor (second)

day19 :: (String -> String, String -> String)
day19 = (part1, part2)

type Workflows = Map String [WorkflowCondDest]
type WorkflowCondition = (Char, [Range.Range Int])
type WorkflowCondDest = (WorkflowCondition, String)
type Part = Map Char Int

part1 :: String -> String
-- Find the sum of all part values of accepted parts
part1 input = show
    $ sum
    $ map (sum . Map.elems . snd)
    -- Filter for those who ended up at "A"
    $ filter (\(dst, _) -> dst == "A")
    -- Find the last destination for each part starting with the "in" workflow
    $ map (findDestination workflows "in") parts
    where
        (workflows, parts) = parseInput input

-- Count all combinations given the list of conditions collected from the paths from "A" to "in"
part2 :: String -> String
part2 input = ppShow $ sum $ map countCombinations allRanges
    where
        -- All conditions of all part types from "A" to "in"
        allRanges = findReversePaths reversedWorkflows "A" []
        -- Data structure of all reversed workflows
        reversedWorkflows = reverseWorkflows workflows
        (workflows, _) = parseInput input

-- Recursively evaluate conditions and workflows until we reach either 'A' or 'R'
findDestination :: Workflows -> String -> Part -> (String, Part)
-- Base cases - return the part and whether it was rejected/accepted
findDestination _ "A" part = ("A", part)
findDestination _ "R" part = ("R", part)
-- Recursion - run new workflow at workflowID
findDestination workflows workflowId part = findDestination workflows nextWorkflowId part
    where
        -- Go through all conditions of this workflow until one is true
        -- at which point second element of tuple is the new workflow ID
        nextWorkflowId = snd $ fromJust $ find conditionMet conditions
        -- Evaluate the condition against the value pulled from the part based on the key
        conditionMet ((condKey, ranges), _) = Range.inRanges ranges (part Map.! condKey)
        conditions = workflows Map.! workflowId

-- Given a set of workflows keyed by workflow ID, generate a reversed
-- data set keyed by destination workflow ID and all possible source conditions/workflows.
-- reverseWorkflows :: Workflows -> [([WorkflowCondition], (String, [WorkflowCondition]), String)]
reverseWorkflows :: Workflows -> [(String, [WorkflowCondition], String)]
reverseWorkflows workflows = concatMap processWorkflow $ Map.assocs workflows
    where
        -- For each workflow, process each condition accumulating preceding conditions
        -- as we go (e.g. we know we get the third condition only if the inverse of the
        -- first two conditions are true)
        processWorkflow (workflowId, conditions) = snd $ mapAccumL (reverseCondition workflowId) [] conditions

-- Reverse the data structure of a specific condition. 
-- Accumulator previousConds' is all previous conditions, inverted.
reverseCondition :: String -> [WorkflowCondition] -> WorkflowCondDest -> ([WorkflowCondition], (String, [WorkflowCondition], String))
reverseCondition workflowId previousConds (currCond, dest) = (previousConds', (dest, combinedConds, workflowId))
    where
        combinedConds = currCond:previousConds
        -- Update the accumulator to contain all previous conditions (inverted)
        previousConds' = previousConds ++ [(currCondKey, Range.invert currCondRange)]
        (currCondKey, currCondRange) = currCond

-- Given the structure of reversed workflows, find all paths that lead to the given workflow ID
-- and their applicable (cumulative) conditions
findReversePaths :: [(String, [WorkflowCondition], String)] -> String -> [WorkflowCondition] -> [[(Char, [Range.Range Int])]]
-- Base case - we've reached the beginning - return all conditions we've collected
findReversePaths _ "in" conditions = [conditions]
findReversePaths reversedWorkflows workflowId conditions = 
    -- For all possible workflows that result in the given workflowId,
    -- recursively explore each workflow and collect conditions all the way
    -- until "in"
    concatMap explore
    $ filter (\(x, _, _) -> x == workflowId) reversedWorkflows
    where
        -- Recurse with the new workflow ID and the cumulative collections thus far
        explore (_, conditions', srcId) = findReversePaths reversedWorkflows srcId (conditions' ++ conditions)

-- Given a list of paths from "A" to "in" and their conditions, 
-- calculate the number of combinations by multiplying the ranges
-- of each of the keys
countCombinations :: [(Char, [Range.Range Int])] -> Int
countCombinations allRanges = 
    -- Multiply combinations for each key x m a s
    product $ map countCombosForKey "xmas"
    where
        -- Count the number of integers in each range
        countCombosForKey key = length $ Range.fromRanges $ combinedRanges key
        -- Calculate intersection for all ranges, including the base range defined by the challenge - [1..4000]
        combinedRanges key = foldl Range.intersection [1 Range.+=+ 4000] $ map snd $ rangesForKey key
        -- Filter for the given key
        rangesForKey key = filter (\x -> fst x == key) allRanges


-- Parse the input into workflows and parts
parseInput :: [Char] -> (Workflows, [Part])
parseInput input = (workflows', parts')
    where
        workflows' = parseWorkflows workflows
        parts' = map parsePart parts
        [workflows, parts] = splitOn [""] $ lines input

-- Convert the split string into a Map with the workflow name
-- as the key and the list of conditions as the value
parseWorkflows :: [String] -> Workflows
parseWorkflows = Map.fromList . map (parseWorkflow . wordsBy (`elem` "{},"))
    where
        parseWorkflow (workflowName:conditions) = (workflowName, parseConditions conditions)


-- Parse each of the conditions in a workflow into a key, a range, and a destination
parseConditions :: [String] -> [WorkflowCondDest]
parseConditions conditions = conditions' ++ [lastCondition]
    where
        -- Everything but the last condition has both a
        -- function and a destination. Parse into an actual functino
        conditions' = map (parseCondition . splitOn ":") $ init conditions
        -- The last condition is the infinite range that always evaluates to true to 
        -- send the part to the last dest
        lastCondition = (('x', [Range.inf]), lastDest)
        lastDest = last conditions

-- Convert a condition string / destination into a condition function/dest tuple
parseCondition :: [String] -> WorkflowCondDest
parseCondition [condKey:condChar:condVal, dest] = ((condKey, toRange condChar $ parseInt condVal), dest)
    where
        toRange '>' val = [Range.lbe val] -- Lower bound exclusive
        toRange '<' val = [Range.ube val] -- Upper bound exclusive


parsePart :: String -> Part
parsePart =
    -- Convert each pair to a an entry in a Map
    Map.fromList . map (\[key, val] -> (head key, parseInt val))
    -- Group by every two elements in the list
    . chunksOf 2
    -- Remove all special characters
    -- Will be left with something like ["a", "1234", "b", "2345"]
    . wordsBy (`elem` "{},=")
