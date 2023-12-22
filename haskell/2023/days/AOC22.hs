module AOC22 (day22) where
import Data.List.Split (splitOneOf)
import Data.List ( sort, mapAccumL, foldl', nub )
import Common (parseInt)
import Debug.Trace
import qualified Data.Map as Map
import Data.Map (Map)

data Point = Pt Int Int Int deriving (Eq, Ord)
instance Show Point where
    show (Pt x y z) = show (x, y, z)
data Brick = Brick Point Point deriving (Eq)
instance Show Brick where
    show (Brick from to) = "[" ++ show from ++ "~" ++ show to ++ "]"
instance Ord Brick where
    compare (Brick (Pt _ _ z11) (Pt _ _ z12)) (Brick (Pt _ _ z21) (Pt _ _ z22)) = compare (min z11 z12) (min z21 z22)
type Mapping = Map Point Brick

day22 :: (String -> String, String -> String)
day22 = (part1, part2)

part1 :: String -> String
part1 input = show $ length $ filter (canBeDisintegrated occupied') bricks'
    where
        (occupied', bricks') = mapAccumL dropBrick occupied $ sort bricks
        occupied = occupiedPoints bricks
        bricks = parseInput input

part2 :: String -> String
part2 = show . parseInput

canBeDisintegrated :: Mapping -> Brick -> Bool
canBeDisintegrated occupied brick = null bricksAboveCurr || all (brickIsSupported otherBrickPts) bricksAboveCurr
    where
        bricksAboveCurr = nub $ map (otherBrickPts Map.!) $ filter (`Map.member` otherBrickPts) $ brickToPoints $ translateBrick brick (0, 0, 1)
        otherBrickPts = removeBrick occupied brick

brickIsSupported :: Mapping -> Brick -> Bool
brickIsSupported occupied brick = isOnFloor || hasBrickUnder
    where
        isOnFloor = any (\(Pt _ _ z) -> z == 1) $ brickToPoints brick
        hasBrickUnder = any (`Map.member` occupied') $ brickToPoints $ translateBrick brick (0, 0, -1)
        occupied' = removeBrick occupied brick

dropBrick :: Mapping -> Brick -> (Mapping, Brick)
dropBrick occupied brick = (occupied', brick')
    where
        occupied' = replaceBrick occupied brick brick'
        brick' = last $ takeWhile (not . brickCollides otherBrickPts) $ lowerOptions
        lowerOptions = map (translateBrick brick) $ [(0, 0, z) | z <- [0,-1..]]
        otherBrickPts = removeBrick occupied brick

brickCollides :: Mapping -> Brick -> Bool
brickCollides occupied brick = any collides $ brickToPoints brick
    where collides point@(Pt _ _ z) =
            z <= 0 -- At or below floor
            || Map.member point occupied -- occupied by another brick

translateBrick :: Brick -> (Int, Int, Int) -> Brick
translateBrick (Brick (Pt x1 y1 z1) (Pt x2 y2 z2)) (dx, dy, dz) =
    Brick (Pt (x1 + dx) (y1 + dy) (z1 + dz)) (Pt (x2 + dx) (y2 + dy) (z2 + dz))

removeBrick :: Mapping -> Brick -> Mapping
removeBrick mapping brick = foldl' (flip Map.delete) mapping $ brickToPoints brick

addBrick :: Mapping -> Brick -> Mapping
addBrick mapping brick  = foldl' (\acc x -> Map.insert x brick acc) mapping $ brickToPoints brick

replaceBrick :: Mapping -> Brick -> Brick -> Mapping
replaceBrick mapping oldBrick newBrick = flip addBrick newBrick $ removeBrick mapping oldBrick

occupiedPoints :: [Brick] -> Map Point Brick
occupiedPoints = foldl' addBrick Map.empty

brickToPoints :: Brick -> [Point]
brickToPoints (Brick (Pt x1 y1 z1) (Pt x2 y2 z2)) = [Pt x y z | x <- [x1..x2], y <- [y1..y2], z <- [z1..z2]]

parseInput :: String -> [Brick]
parseInput = map processLine . lines
    where
        processLine = toBrick . map parseInt . splitOneOf ",~"
        toBrick [x1, y1, z1, x2, y2, z2] = Brick (Pt x1 y1 z1) (Pt x2 y2 z2)

