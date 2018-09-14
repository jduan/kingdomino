-- https://airbnb.quip.com/fMQFASRC710u/Kingdomino
module Kingdomino where

import Data.Char (isLetter)
import qualified Data.Map as Map
import Data.Maybe (isJust)

data Tile =
  Tile String
       Int
  deriving (Show, Eq)

type Row = [Tile]

data Board =
  Board [Row]
        Int
        Int
  deriving (Show, Eq)

-- check if a position is on the board or not
onBoard :: Board -> Position -> Bool
onBoard board@(Board rows height width) (row, col) =
  row >= 0 && row < height && col >= 0 && col < width

getTile :: Board -> Position -> Tile
getTile board@(Board rows height width) (row, col) = (rows !! row) !! col

buildBoard :: [String] -> Board
buildBoard strs = Board rows (length rows) (length $ head rows)
  where
    rows :: [Row]
    rows = map f strs
    f :: String -> Row
    f s = map toTile $ words s
    toTile :: String -> Tile
    toTile s =
      let chars = takeWhile isLetter s
          digits = read (dropWhile isLetter s) :: Int
       in Tile chars digits

type Visited = Map.Map (Int, Int) Bool

initVisited = Map.empty

type Property = [Tile]

-- calculate the total points of a Property
propertyPoints :: Property -> Int
propertyPoints property = crowns * length property
  where
    crowns = sum $ map getCrown property
    getCrown (Tile _ n) = n

initProperty = []

type DiscoverState = (Tile, Property, Visited)

type Position = (Int, Int)

-- get four neighbors of a position
getNeighbors :: Board -> Position -> [Position]
getNeighbors board@(Board rows height width) (row, col) =
  filter (onBoard board) neighbors
  where
    neighbors :: [Position]
    neighbors = [(row, col - 1), (row, col + 1), (row - 1, col), (row + 1, col)]

hasVisited :: Visited -> Position -> Bool
hasVisited visited pos = isJust $ Map.lookup pos visited

sameTileType :: Tile -> Tile -> Bool
sameTileType (Tile t1 _) (Tile t2 _) = t1 == t2

-- add tile to property and mark position as visited
updateDiscoverState :: DiscoverState -> Tile -> Position -> DiscoverState
updateDiscoverState state@(startTile, property, visited) tile pos =
  (startTile, newProperty, newVisited)
  where
    newProperty = tile : property
    newVisited = Map.insert pos True visited

-- explore all the tiles of the same type starting from a position
discoverProperty :: Board -> Position -> DiscoverState -> DiscoverState
discoverProperty board@(Board rows height width) (row, col) discoverState =
  foldr ff discoverState neighbors
  where
    neighbors :: [Position]
    neighbors = getNeighbors board (row, col)
    ff :: Position -> DiscoverState -> DiscoverState
    ff pos@(row', col') state =
      let (startTile, property, visited) = state
          currentTile = getTile board pos
       in if not (hasVisited visited pos) && sameTileType startTile currentTile
            then discoverProperty
                   board
                   pos
                   (updateDiscoverState state currentTile pos)
            else state

-- Play a game.
game :: Board -> Int
game board@(Board rows height width) = sum $ map propertyPoints properties
  where
    properties :: [Property]
    properties = map getProperty (fst $ go board)
    getProperty :: DiscoverState -> Property
    getProperty (_, p, _) = p
    go :: Board -> ([DiscoverState], Visited)
    go board@(Board rows height width) =
      let positions =
            [ (x, y)
            | x <- enumFromTo 0 (height - 1)
            , y <- enumFromTo 0 (width - 1)
            ]
       in foldr ff ([], initVisited) positions
    ff :: Position -> ([DiscoverState], Visited) -> ([DiscoverState], Visited)
    ff pos (states, visited) =
      let tile = getTile board pos
          initState = (tile, [tile], Map.insert pos True visited)
       in if hasVisited visited pos
            then (states, visited)
            else let newState@(_, _, newVisited) =
                       discoverProperty board pos initState
                  in (newState : states, newVisited)

input :: [String]
input =
  [ "G0 W1 W1 W0 P2"
  , "W0 W0 F0 F0 F0"
  , "W0 W1 F0 S2 S1"
  , "G0 X0 G1 G0 G0"
  , "S0 M2 M0 G1 F0"
  ]

main :: IO ()
main =
  let board = buildBoard input
      score = game board
   in print score
-- test case 1
-- position = (1 :: Int, 2 :: Int)
--
-- initDiscoverState =
--   ( Tile "F" 0
--   , getTile board position : initProperty
--   , Map.insert position True initVisited)
--
-- test case 2
--
-- position = (0 :: Int, 1 :: Int)
--
-- initDiscoverState =
--   ( Tile "W" 1
--   , getTile board position : initProperty
--   , Map.insert position True initVisited)
--
-- test case 3
--
-- position = (0 :: Int, 0 :: Int)
--
-- initDiscoverState =
--   ( Tile "G" 0
--   , getTile board position : initProperty
--   , Map.insert position True initVisited)
--
-- discover = discoverProperty board position initDiscoverState
