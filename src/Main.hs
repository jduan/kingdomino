module Main where

import Data.Char (digitToInt)
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  putStrLn "hello world"

data Tile =
  Tile Char
       Int
  deriving (Show, Eq)

getTileType :: Tile -> Char
getTileType (Tile char _) = char

data Board =
  Board [[Tile]]
        Int
        Int
  deriving (Show, Eq)

getTile :: Board -> Int -> Int -> Tile
getTile (Board tiles height width) x y = (tiles !! x) !! y

-- Given an input of a list of Strings, build a board
-- Example input:
-- [
--   "G0 W1 W1 W0 P2",
--   "W0 W0 F0 F0 F0",
--   "W0 W1 F0 S2 S1",
--   "G0 X0 G1 G0 G0",
--   "S0 M2 M0 G1 F0"
-- ]
makeBoard :: [String] -> Board
makeBoard rows = Board tiles (length tiles) (length $ head tiles)
    -- Parse "G0 W1 W1 W0 P2" into a list of Tiles
  where
    tiles = map parseTiles rows
    parseTiles :: String -> [Tile]
    parseTiles str = map parseTile $ words str
    -- Parse "G0" into a Tile
    parseTile :: String -> Tile
    parseTile str = Tile (head str) (digitToInt $ str !! 1)

inputRows =
  [ "G0 W1 W1 W0 P2"
  , "W0 W0 F0 F0 F0"
  , "W0 W1 F0 S2 S1"
  , "G0 X0 G1 G0 G0"
  , "S0 M2 M0 G1 F0"
  ]

calculateScore :: Board -> Int
calculateScore (Board tiles height width) = 99

type Visited = M.Map (Int, Int) Bool

isVisited :: Visited -> (Int, Int) -> Bool
isVisited map t =
  case M.lookup t map of
    Nothing -> False
    _ -> True

markVisited :: Visited -> (Int, Int) -> Visited
markVisited map t = M.insert t True map

data VisitState =
  VisitState Visited
             Char
  deriving (Show, Eq)

-- From a tile of (x, y), use DFS to discover all the connected tiles of
-- the same type.
findTiles :: Board -> Int -> Int -> VisitState -> [Tile]
findTiles (Board tiles height width) x y (VisitState visitedMap tileType) =
  if isVisited visitedMap (x, y)
     then []
     else if getTileType (getTile tiles x y) /= tileType
             then []
             else
