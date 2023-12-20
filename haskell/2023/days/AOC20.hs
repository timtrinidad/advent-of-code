{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module AOC20 (day20) where
import Data.List.Split (wordsBy, splitOn)
import Text.Show.Pretty (ppShow)
import Data.List (intercalate, isInfixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

day20 :: (String -> String, String -> String)
day20 = (part1, part2)

type Config = Map String PulseModule
type PulseCounts = Map Bool Int
data PMType = Broadcaster | Conjunction | FlipFlop | None deriving (Show)
data PulseModule = PM {
    pmType :: PMType,
    pmName :: String,
    pmDests :: [String],
    pmState :: Map String Bool
}
data PulseAction = PA String String Bool deriving (Show)
instance Show PulseModule where
  show (PM a b c d) = "{" ++ show a ++ " " ++ b ++ " -> " ++ intercalate "," c ++ " | " ++ show (Map.assocs d) ++ "}"

part1 :: String -> String
part1 input = show $ product $ Map.elems $ pushButton 1000 config Map.empty
    where config = parseInput input

part2 :: String -> String
part2 = show . parseInput

pushButton 0 _ counts = counts
pushButton num config counts = pushButton (num-1) config' counts'
    where
        (config', counts') = processPulses config [PA "button" "broadcaster" False] counts

processPulses :: Num a => Map String PulseModule -> [PulseAction] -> Map Bool a -> (Map String PulseModule, Map Bool a)
processPulses config [] pulseCounts = (config, pulseCounts)
processPulses config (currAction@(PA srcName destName isHighPulse):restActions) pulseCounts = processPulses config' restActions' pulseCounts'
    where
        pulseCounts' = Map.insertWith (+) isHighPulse 1 pulseCounts
        restActions' = restActions ++ newActions
        (config', newActions) = generateActions config isHighPulse srcName destModule
        destModule = Map.findWithDefault defaultModule destName config
        defaultModule = PM { pmName = destName, pmType = None, pmDests = [], pmState = Map.empty }

generateActions config isHighPulse _ pulseModule@PM { pmType=FlipFlop } = (config', newActions isHighPulse currOn)
    where
        config' = Map.insert currModuleName (pulseModule { pmState = Map.fromList [("on", nextOn isHighPulse currOn)]}) config
        newActions True _ = []
        newActions False True = map (\pmDest -> PA currModuleName pmDest False) $ pmDests pulseModule
        newActions False False = map (\pmDest -> PA currModuleName pmDest True) $ pmDests pulseModule
        nextOn True = id
        nextOn False = not
        currOn  = pmState pulseModule Map.! "on"
        currModuleName = pmName pulseModule
generateActions config isHighPulse srcName pulseModule@PM { pmType=Conjunction } = (config', newActions)
    where
        config' = Map.insert currModuleName (pulseModule { pmState = incomingPulseHist }) config
        newActions = map (\pmDest -> PA currModuleName pmDest pulseToSend) $ pmDests pulseModule
        pulseToSend = not $ and $ Map.elems incomingPulseHist
        incomingPulseHist = Map.insert srcName isHighPulse $ pmState pulseModule
        currModuleName = pmName pulseModule
generateActions config isHighPulse _ pulseModule@PM { pmType=Broadcaster } = (config, newActions)
    where
        newActions = map (\pmDest -> PA "broadcaster" pmDest isHighPulse) $ pmDests pulseModule
generateActions config _ _ _ = (config, [])

parseInput :: String -> Map String PulseModule
parseInput input = Map.fromList $ map (parseLine allLines . wordsBy (`elem` " ->,")) $ allLines
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
        initState FlipFlop = Map.fromList [("on", False)]
        initState Conjunction = Map.fromList $ map (\x -> (x, False)) $ findInputs allLines moduleName
        initState _ = Map.empty

parseModuleKey :: String -> (PMType, String)
parseModuleKey ('&':moduleName) = (Conjunction, moduleName)
parseModuleKey ('%':moduleName) = (FlipFlop, moduleName)
parseModuleKey "broadcaster" = (Broadcaster, "broadcaster")
parseModuleKey other = (None, other)

findInputs :: [String] -> String -> [String]
findInputs allLines dest = map (tail . head . splitOn " ") $ filter (isInfixOf (' ':dest)) allLines

