module AOC22 (day22) where
import Data.List.Split (splitOneOf)
import Data.List ( sort, mapAccumL, foldl', nub )
import Common (parseInt)
import Debug.Trace
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

-- A 3D coordinate
data Point = Pt Int Int Int deriving (Eq, Ord)
instance Show Point where
    show (Pt x y z) = show (x, y, z)

-- A brick with a starting point and ending point (inclusive)
data Brick = Brick Point Point deriving (Eq)
instance Show Brick where
    show (Brick from to) = "[" ++ show from ++ "~" ++ show to ++ "]"
instance Ord Brick where
    -- Sort bricks by height
    compare (Brick (Pt _ _ z11) (Pt _ _ z12)) (Brick (Pt _ _ z21) (Pt _ _ z22)) = compare (min z11 z12) (min z21 z22)

-- A Map of occupied points and the bricks to which they belong
type Mapping = Map Point Brick

day22 :: (String -> String, String -> String)
day22 = (part1, part2)

-- Count the number of blocks which can be disintegrated without causing other blocks to fall
part1 :: String -> String
part1 input = show $ length $ filter (canBeDisintegrated occupied') bricks'
    where
        ((occupied', _), bricks') = dropBricks occupied bricks
        occupied = occupiedPoints bricks
        bricks = parseInput input

-- For each brick that would cause other blocks to fall if disintegrated,
-- count the number of blocks that would fall
part2 :: String -> String
part2 input = show $ sum $ map numBricksThatWoudlFall criticalBricks
    where
        -- Update list of bricks and points as if the current brick didn't exist, run drop
        numBricksThatWoudlFall brick = countDroppedBricks (removeBrick occupied' brick) $ filter (/=brick) bricks'
        -- Bricks which would cause other bricks to fall if removed
        criticalBricks = filter (not . canBeDisintegrated occupied') bricks'
        -- Initial drop
        ((occupied', _), bricks') = dropBricks occupied bricks
        occupied = occupiedPoints bricks
        bricks = parseInput input

-- After dropping all bricks, return the number of bricks
-- which were actually moved.
countDroppedBricks :: Map Point Brick -> [Brick] -> Int
countDroppedBricks occupied bricks = numMoved
    where
        ((_, numMoved), _) = dropBricks occupied bricks

-- A brick can be disintegrated if it has no blocks above or if any blocks above
-- would otherwise still be supported
canBeDisintegrated :: Mapping -> Brick -> Bool
canBeDisintegrated occupied brick = null bricksAboveCurr || all (brickIsSupported otherBrickPts) bricksAboveCurr
    where
        -- Given a set of points, get a unique list of bricks occupying each point
        bricksAboveCurr = nub $ map (otherBrickPts Map.!) occupiedPointsAboveCurrent
        -- Get a list of points above the current block that are occupied
        occupiedPointsAboveCurrent = filter (`Map.member` otherBrickPts) 
            $ brickToPoints $ translateBrick brick (0, 0, 1)
        -- Consider only other bricks, not the one being checked
        otherBrickPts = removeBrick occupied brick

-- Determine if a given brick is supported (i.e. would not fall)
brickIsSupported :: Mapping -> Brick -> Bool
brickIsSupported occupied brick = isOnFloor || hasBrickUnder
    where
        isOnFloor = any (\(Pt _ _ z) -> z == 1) $ brickToPoints brick
        -- Has a brick supporting this one if it cannot be lowered without colliding with other points
        hasBrickUnder = any (`Map.member` occupied') $ brickToPoints $ translateBrick brick (0, 0, -1)
        -- Consider other bricks without the current one being checked
        occupied' = removeBrick occupied brick

-- Given a Map of occupied points and a list of bricks,
-- lower each brick as low as possible without collisions.
dropBricks :: Mapping -> [Brick] -> ((Mapping, Int), [Brick])
dropBricks occupied bricks = mapAccumL dropBrick (occupied, 0) $ sort bricks

-- Given an accumulator (currently occupied points and number of previously moved bricks)
-- translate a brick as far down as it will go before it collides with another brick.
dropBrick :: (Mapping, Int) -> Brick -> ((Mapping, Int), Brick)
dropBrick (occupied, numMoved) brick = ((occupied', numMoved'), brick')
    where
        -- Increment if the brick was actually moved
        numMoved' = if brick' == brick then numMoved else numMoved + 1
        -- Updated Map with new brick location
        occupied' = replaceBrick occupied brick brick'
        -- Get the last option before we collide
        brick' = last $ takeWhile (not . brickCollides otherBrickPts) lowerOptions
        -- Enumerate options lowering the brick one point at a time
        lowerOptions = map (translateBrick brick) $ [(0, 0, z) | z <- [0,-1..]]
        -- Map containing only other bricks
        otherBrickPts = removeBrick occupied brick

-- Given a Map of occupied points, determine if the given brick
-- has points that are already occupied
brickCollides :: Mapping -> Brick -> Bool
brickCollides occupied brick = any collides $ brickToPoints brick
    where collides point@(Pt _ _ z) =
            z <= 0 -- At or below floor
            || Map.member point occupied -- occupied by another brick

-- Given a brick, return a new brick based on how it should be moved
translateBrick :: Brick -> (Int, Int, Int) -> Brick
translateBrick (Brick (Pt x1 y1 z1) (Pt x2 y2 z2)) (dx, dy, dz) =
    Brick (Pt (x1 + dx) (y1 + dy) (z1 + dz)) (Pt (x2 + dx) (y2 + dy) (z2 + dz))

-- Given a list of bricks, create a Map keyed by point with the full Brick object as the value
occupiedPoints :: [Brick] -> Map Point Brick
occupiedPoints = foldl' addBrick Map.empty

-- In a point mapping, replace an old location with a new brick location
replaceBrick :: Mapping -> Brick -> Brick -> Mapping
replaceBrick mapping oldBrick newBrick = flip addBrick newBrick $ removeBrick mapping oldBrick

-- Remove a brick from the Map of individual points
removeBrick :: Mapping -> Brick -> Mapping
removeBrick mapping brick = foldl' (flip Map.delete) mapping $ brickToPoints brick

-- Add a brick to the Map of individual points
addBrick :: Mapping -> Brick -> Mapping
addBrick mapping brick  = foldl' (\acc x -> Map.insert x brick acc) mapping $ brickToPoints brick


-- Expand a brick (start, end) to a list of points
brickToPoints :: Brick -> [Point]
brickToPoints (Brick (Pt x1 y1 z1) (Pt x2 y2 z2)) = [Pt x y z | x <- [x1..x2], y <- [y1..y2], z <- [z1..z2]]

-- Convert the input into a list of Brick objects
parseInput :: String -> [Brick]
parseInput = map processLine . lines
    where
        processLine = toBrick . map parseInt . splitOneOf ",~"
        toBrick [x1, y1, z1, x2, y2, z2] = Brick (Pt x1 y1 z1) (Pt x2 y2 z2)

