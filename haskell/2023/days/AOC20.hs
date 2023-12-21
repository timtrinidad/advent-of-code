{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module AOC20 (day20) where
import Data.List.Split (wordsBy, splitOn)
import Text.Show.Pretty (ppShow)
import Data.List ( intercalate, isInfixOf, foldl' )
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

day20 :: (String -> String, String -> String)
day20 = (part1, part2)

type Config = Map String PulseModule
type PulseCounts = Map PMState Int
data PMState = HighPulse | LowPulse | FFOn | FFOff deriving (Show, Eq, Ord)
data PMType = Broadcaster | Conjunction | FlipFlop | None deriving (Show, Eq)
data PulseAction = PA String PMState String deriving (Show)
data PulseModule = PM {
    pmType :: PMType,
    pmName :: String,
    pmDests :: [String],
    pmState :: Map String PMState
}
instance Show PulseModule where
  show (PM a b c d) = "{" ++ show a ++ " " ++ b ++ " -> " ++ intercalate "," c ++ " | " ++ show (Map.assocs d) ++ "}"

part1 :: String -> String
part1 input = show $ product -- multiply counts
    $ Map.elems $ snd -- Get counts
    $ foldl pressButton (config, Map.empty) [1..1000] -- Press the button 1000 times
    where
        config = parseInput input
        pressButton (config', counts') _ = processPulses [PA "button" LowPulse "broadcaster"] config' counts'

part2 :: String -> String
part2 input = outputDiagram config
    where
        config = parseInput input

-- Output a graph diagram to be rendered by mermaid.live
outputDiagram :: Config -> String
outputDiagram config = unlines $ concatMap split $ Map.elems config
    where
        split pulseModule = map (\x -> "\"" ++ pmTypeName pulseModule ++ "\" --> \"" ++ (if Map.member x config then pmTypeName (config Map.! x) else x) ++ "\"") $ pmDests pulseModule
        pmTypeName pulseModule' = (if pmType pulseModule' == FlipFlop then "%" else "&") ++ pmName pulseModule'

-- Recursively process pulses until none are left in the queue
processPulses :: [PulseAction] -> Config -> PulseCounts -> (Config, PulseCounts )
-- Base case - no more pulses
processPulses [] config pulseCounts = (config, pulseCounts)
-- Recurse, incrementing pulse counts with updated state
processPulses ((PA srcName pulseType destName):restActions) config pulseCounts = processPulses restActions' config' pulseCounts'
    where
        -- Increment counts
        pulseCounts' = Map.insertWith (+) pulseType 1 pulseCounts
        -- Append new actions to list
        restActions' = restActions ++ newActions
        -- Determine next actions and updated state based on module type
        (config', newActions) = generateActions config pulseType srcName destModule
        destModule = Map.findWithDefault defaultModule destName config
        defaultModule = PM { pmName = destName, pmType = None, pmDests = [], pmState = Map.empty }


-- Update the state and generate new pulses depending on the module type
generateActions :: Config -> PMState -> String -> PulseModule -> (Config, [PulseAction])
generateActions config pulseType _ pulseModule@PM { pmType=FlipFlop } = (config', newActions pulseType currOn)
    where
        -- Update state
        config' = Map.insert currModuleName (pulseModule { pmState = Map.fromList [("on", nextOn pulseType)]}) config
        -- If high pulse, keep current state and don't send new pulses
        -- If low pulse, alternate state and send pulses
        nextOn HighPulse = currOn
        nextOn LowPulse = if currOn == FFOn then FFOff else FFOn
        newActions HighPulse _ = []
        newActions LowPulse FFOn = map (PA currModuleName LowPulse) $ pmDests pulseModule
        newActions LowPulse FFOff = map (PA currModuleName HighPulse) $ pmDests pulseModule
        -- Whether or not this switch is currently on
        currOn  = pmState pulseModule Map.! "on"
        currModuleName = pmName pulseModule
-- Conjunction - update state before determining pulse and send to destinations
generateActions config pulseType srcName pulseModule@PM { pmType=Conjunction } = (config', newActions)
    where
        -- Update state
        config' = Map.insert currModuleName (pulseModule { pmState = incomingPulseHist }) config
        -- Send pulse to all destinations
        newActions = map (PA currModuleName pulseToSend) $ pmDests pulseModule
        -- Low if all True, High otherwise
        pulseToSend = if all (==HighPulse) $ Map.elems incomingPulseHist then LowPulse else HighPulse
        -- Existing state keyed by all incoming connections
        incomingPulseHist = Map.insert srcName pulseType $ pmState pulseModule
        currModuleName = pmName pulseModule
-- Broadcaster - resend pulse to destinations
generateActions config pulseType _ pulseModule@PM { pmType=Broadcaster } = (config, newActions)
    where
        newActions = map (PA "broadcaster" pulseType) $ pmDests pulseModule
-- All other types - don't update config and don't generate new actions
generateActions config _ _ _ = (config, [])

parseInput :: String -> Config
parseInput input = Map.fromList $ map (parseLine allLines . wordsBy (`elem` " ->,")) allLines
    where allLines = lines input

parseLine :: [String] -> [String] -> (String, PulseModule)
parseLine allLines (moduleKey:destinations) =  (moduleName, pm)
    where
        (moduleType, moduleName) = parseModuleKey moduleKey
        pm = PM {
            pmType = moduleType,
            pmName = moduleName,
            pmDests = destinations,
            pmState = initState moduleType
        }
        initState FlipFlop = Map.fromList [("on", FFOff)]
        initState Conjunction = Map.fromList $ map (\x -> (x, LowPulse)) $ findInputs allLines moduleName
        initState _ = Map.empty

parseModuleKey :: String -> (PMType, String)
parseModuleKey ('&':moduleName) = (Conjunction, moduleName)
parseModuleKey ('%':moduleName) = (FlipFlop, moduleName)
parseModuleKey "broadcaster" = (Broadcaster, "broadcaster")
parseModuleKey other = (None, other)

findInputs :: [String] -> String -> [String]
findInputs allLines dest = map (tail . head . splitOn " ") $ filter (isInfixOf (' ':dest)) allLines

