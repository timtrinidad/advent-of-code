module AOC19 (day19) where
import Data.List.Split (splitOn, wordsBy, chunksOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Common (parseInt)
import Data.List (find)
import Data.Maybe (fromJust)

day19 :: (String -> String, String -> String)
day19 = (part1, part2)

type Workflows = Map String [WorkflowCondition]
type WorkflowCondition = (Part -> Bool, String)
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

part2 :: String -> String
part2 = const "Hello"

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
        nextWorkflowId = snd $ fromJust $ find (\(x, _) -> x part) conditions
        conditions = workflows Map.! workflowId

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


parseConditions :: [String] -> [WorkflowCondition]
parseConditions conditions = conditions' ++ [lastCondition]
    where
        -- Everything but the last condition has both a
        -- function and a destination. Parse into an actual functino
        conditions' = map (parseCondition . splitOn ":") $ init conditions
        -- The last condition shoudl always resolve to true
        -- and send the part to the last dest
        lastCondition = (const True, lastDest)
        lastDest = last conditions



-- Convert a condition string / destination into a condition function/dest tuple
parseCondition :: [String] -> (Map Char Int -> Bool, [Char])
parseCondition [condKey:condChar:condVal, dest] = (condFunc, dest)
    where
        -- This function checks the part value based on key
        condFunc part = comparisonFunc (part Map.! condKey) condVal'
        condVal' = parseInt condVal
        comparisonFunc
            | condChar == '>' = (>)
            | condChar == '<' = (<)


parsePart :: String -> Part
parsePart = 
    -- Convert each pair to a an entry in a Map
    Map.fromList . map (\[key, val] -> (head key, parseInt val))
    -- Group by every two elements in the list
    . chunksOf 2
    -- Remove all special characters
    -- Will be left with something like ["a", "1234", "b", "2345"]
    . wordsBy (`elem` "{},=")
