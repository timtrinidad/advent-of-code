{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module AOC25 (day25) where
import Data.List.Split (wordsBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Data.Maybe (isNothing, fromJust)
import Data.List (find, nub, sortBy)
import Algorithm.Search (dijkstra)
import System.Random ( randomR, uniformR, getStdGen, randomRs, mkStdGen )
import Text.Show.Pretty (ppShow)

type Vertices = [String]
type Edges = [(String, String)]
type Graph = Map String [String]

day25 :: (String -> String, String -> String)
day25 = (part1, part2)

part1 :: String -> String
part1 input = ppShow $ numVerticesInSubtree * (numVertices - numVerticesInSubtree)
    where
        numVertices = length vertices
        -- Find the number of connected nodes in the new graph (which should now be disconnected)
        -- It doesn't really matter which of the two sides we end up with so we arbitrarily pick the first vertex
        numVerticesInSubtree = length $ findSubtree graph' $ Set.fromList [fst $ head edges']
        -- Generate a graph without the top three connections
        graph' = toGraph edges'
        edges' = filter (\(a, b) -> (a, b) `notElem` topThreeConnections && (b, a) `notElem` topThreeConnections) edges
        -- Find the top three connections traversed in the paths of a set of random vertex pairs
        topThreeConnections = findTopThreeConnections graph $ randomVertexPairs vertices
        graph = toGraph edges
        (vertices, edges) = parseInput input


part2 :: String -> String
part2 = const "Not Applicable"

findTopThreeConnections :: Graph -> [(String, String)] -> Edges
findTopThreeConnections graph randomPairs = map fst topThree
    where
        topThree = take 3 $ sortBy (\a b -> compare (snd b) (snd a)) $ Map.assocs connectionCounts
        connectionCounts = foldl (\acc x -> Map.insertWith (+) x 1 acc) Map.empty seenConnections
        seenConnections = concatMap (findPathConnections graph) randomPairs

randomVertexPairs :: Vertices -> [(String, String)]
randomVertexPairs vertices = map (pairs !!) $ take 1000 randomIdxs
    where
        randomIdxs = randomRs (0, length pairs - 1) (mkStdGen 2023) :: [Int]
        pairs = [(a, b) | a <- vertices, b <- vertices, a /= b]

-- Given a graph and a start/end, return the path between the two vertices
-- in the format of a tuple from each connection (e.g. a -> b -> c returns [(a, b), (b, c)])
-- The tuple is sorted to ensure that (a, b) == (b, a)
findPathConnections :: Graph -> (String, String) -> Edges
findPathConnections graph (start, end) = map sortedTuple traversedConnections
    where
        -- Sort to ensure (a, b) == (b, a)
        sortedTuple (a, b) = if a < b then (a, b) else (b, a)
        -- Turn the path into tuples
        traversedConnections = zip pathWithStart $ tail pathWithStart
        pathWithStart = start:path
        -- Dijkstra to find shortest path which returns a list [a, b, c]
        path = snd $ fromJust $ dijkstra next cost found initial
        next x = graph Map.! x
        cost _ = const 1
        found x = x == end
        initial = start


-- Given a full graph and a set of vertices, find the set of nodes connected to the set
findSubtree :: Graph -> Set String -> Set String
findSubtree graph seen
    | null toExplore = seen
    | otherwise = findSubtree graph seen'
    where
        seen' = Set.insert toAdd seen
        toAdd = head toExplore
        toExplore = nub $ concatMap (filter (\x -> not $ Set.member x seen) . (graph Map.!)) (Set.toList seen)

-- Given a set of connections/edges, create a mapping of vertices to other vertices
toGraph :: Edges -> Graph
toGraph edges = foldl combine Map.empty $ concatMap (\(from, to) -> [(from, [to]), (to, [from])]) edges
    where combine acc (k, v) = Map.insertWith (++) k v acc

-- Parse the input into a set of vertices and connections between vertices
parseInput :: String -> (Vertices, Edges)
parseInput input = (vertices, edges)
    where
        -- Unique set of all vertices
        vertices = nub $ concatMap (\(a, b) -> [a, b]) edges
        edges = concatMap handleLine allLines
        -- Convert each line into a set tuple of source/dest strings
        handleLine (from:tos) = map (\to -> (from, to)) tos
        -- Split into a list of strings, the first one being the source node
        allLines = map (wordsBy (`elem` ": ")) $ lines input