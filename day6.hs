{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
import Prelude hiding (Left, Right)
import Control.Lens
import Control.Parallel.Strategies
import Data.ByteString qualified as BS
import Data.ByteString.Internal (w2c)
import Data.Hashable
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import GHC.Generics
import Linear.V2

type Point = V2 Int

data Direction = Up | Down | Left | Right deriving (Show, Eq, Generic, Hashable)

turn :: Direction -> Direction
turn Up = Right
turn Right = Down
turn Down = Left
turn Left = Up

type Obstacles = HashSet Point

-- Build a set of obstacles from the given map.
obstacles :: Point -> [Char] -> Obstacles
obstacles _ [] = mempty
obstacles pt ('\n' : xs) = obstacles (pt & _y +~ 1 & _x .~ 0) xs
obstacles pt ('#' : xs) = HS.singleton pt <> obstacles (pt & _x +~ 1) xs
obstacles pt (_ : xs) = obstacles (pt & _x +~ 1) xs

obstacles' :: [Char] -> Obstacles
obstacles' = obstacles $ V2 0 0

-- Walk from the given location in the given direction
walk :: Point -> Direction -> Point
walk loc Up = loc & _y -~ 1
walk loc Down = loc & _y +~ 1
walk loc Left = loc & _x -~ 1
walk loc Right = loc & _x +~ 1

-- Is the guard blocked ahead?
blocked :: Point -> Direction -> Obstacles -> Bool
blocked loc dir obs = walk loc dir `HS.member` obs

-- Move in the given direction if we can, or turn.
move :: Point -> Direction -> Obstacles -> (Point, Direction)
move loc dir obs = if blocked loc dir obs
    then (loc, turn dir)
    else (walk loc dir, dir)

-- Where does the guard start?
guard :: Point -> [Char] -> Point
guard _ [] = error "no guard!"
guard pt ('\n' : xs) = guard (pt & _y +~ 1 & _x .~ 0) xs
guard pt ('^' : _) = pt
guard pt (_ : xs) = guard (pt & _x +~ 1) xs

guard' :: [Char] -> Point
guard' = guard $ V2 0 0

width :: [Char] -> Int
width [] = error "empty grid"
width ('\n' : _) = 0
width (_ : xs) = 1 + width xs

-- Returns the path of the guard (both in location and directoin) until they walk off the map.
path :: Point -> Point -> Direction -> Obstacles -> [(Point, Direction)]
path bounds loc dir obs
    | (loc^._x) < 0 || (loc^._y) < 0 || (loc^._x >= bounds^._x) || (loc^._y >= bounds^._y) = [] -- We're outta here
    | otherwise = (loc, dir) : path bounds newLoc newDir obs where
        (newLoc, newDir) = move loc dir obs

-- All coords but one (the guard's starting posit)
allCoordsBut :: Point -> Point -> Point -> [Point]
allCoordsBut bounds loc pt
    | pt ^. _y >= bounds ^. _y = []
    | pt ^. _x >= bounds ^. _x = allCoordsBut bounds loc (pt & _y +~ 1 & _x .~ 0)
    | pt == loc = allCoordsBut bounds loc (pt & _x +~ 1)
    | otherwise = pt : allCoordsBut bounds loc (pt & _x +~ 1)

allCoordsBut' :: Point -> Point -> [Point]
allCoordsBut' bounds loc = allCoordsBut bounds loc $ V2 0 0

-- All flavors of the dungeon with a new obstacle, except at the player's posit.
obstructeds :: Point -> Point -> Obstacles -> [Obstacles]
obstructeds bounds loc obs = (`HS.insert` obs) <$> allCoordsBut' bounds loc

isLoop :: Point -> Point -> Direction -> Obstacles -> Bool
isLoop bounds loc dir obs = retread mempty p where
    p = path bounds loc dir obs

-- We're in a loop if we're in the same spot + direction we once were.
retread :: Hashable a => HashSet a -> [a] -> Bool
retread _ [] = False
retread been (x:xs) = x `HS.member` been || retread (x `HS.insert` been) xs

main :: IO ()
main = do
    bytes <- BS.readFile "input/day6.txt"
    let i = w2c <$> BS.unpack bytes
        obs = obstacles' i
        gloc = guard' i
        gdir = Up
        w = width i
        h = BS.length bytes `div` (w + 1)
        bounds = V2 w h
    -- Part 1
    let visited = HS.fromList . fmap fst $ path bounds gloc gdir obs
    print $ HS.size visited
    -- Part 2
    -- Evaluate this shit in paralell; it's still a few thousand runs.
    print . length . filter id . withStrategy (parBuffer 32 rseq) $
        isLoop bounds gloc gdir <$> obstructeds bounds gloc obs
