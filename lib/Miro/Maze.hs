{-|
Description : Core maze generation algorithms.
Copyright   : (c) 2016-2017 Linus Arver <linus@ucla.edu>
License     : BSD2
Maintainer  : linus@ucla.edu
Stability   : experimental
Portability : POSIX

This module has the 'mkMaze' function that generates mazes.
-}

{-# LANGUAGE RecordWildCards #-}

module Miro.Maze
  ( module Miro.Maze
  , module System.Random.PCG
  ) where

import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Safe
import System.Random.PCG

-- | Describe the location of a cell in a 2D grid. There 9 cases: the four
-- corners, the four edges, and cells that can be in the middle. This
-- information is used by, e.g., 'binaryTree' to help guide its algorithm.
data CellLoc
  = ClTopLeft
  | ClTopRight
  | ClBotLeft
  | ClBotRight
  | ClTop
  | ClBot
  | ClLeft
  | ClRight
  | ClMid
  deriving (Eq, Show)

-- | Given a 'Grid' and 'Cell', determine what type of cell ('CellLoc') it is.
getCellLoc :: Grid -> Cell -> CellLoc
getCellLoc g@Grid{..} Cell{..}
  | surroundedBy [DNorth, DSouth, DEast, DWest] = ClMid
  | surroundedBy [DNorth, DSouth, DEast] = ClLeft
  | surroundedBy [DNorth, DSouth, DWest] = ClRight
  | surroundedBy [DEast, DWest, DNorth] = ClBot
  | surroundedBy [DEast, DWest, DSouth] = ClTop
  | surroundedBy [DEast, DNorth] = ClBotLeft
  | surroundedBy [DEast, DSouth] = ClTopLeft
  | surroundedBy [DWest, DNorth] = ClBotRight
  | otherwise = ClTopRight
  where
  surroundedBy deltas
    = all isJust
    $ map (\d -> getCell (go d cCoord) g) deltas

data MazeType
  = MTBinaryTree
  | MTSidewinder
  deriving (Enum, Eq, Show)

mkMaze :: MazeType -> Grid -> GenIO -> IO Grid
mkMaze m grid rng = case m of
  MTBinaryTree -> foldM (binaryTree rng) grid cells
  MTSidewinder -> fst
    <$> foldM
      (sidewinder rng)
      (grid, fst . cCoord $ head cells)
      cells
  where
  cells = concat $ rowWise grid

binaryTree :: GenIO -> (Grid -> Cell -> IO Grid)
binaryTree rng g@Grid{..} cell = do
  randDelta <- case cl of
    ClTop -> pure DEast
    ClTopLeft -> pure DEast
    ClRight -> pure DNorth
    ClBotRight -> pure DNorth
    _ -> randPick [DNorth, DEast] rng
  return $ g
    { gCells = M.adjustWithKey (mkLink g randDelta) (cCoord cell) gCells
    }
  where
  cl = getCellLoc g cell

sidewinder :: GenIO -> ((Grid, Int) -> Cell -> IO (Grid, Int))
sidewinder rng (g@Grid{..}, runStartXCoord) cell = do
  randDelta <- case cl of
    ClTop -> pure DEast
    ClTopLeft -> pure DEast
    ClRight -> pure DNorth
    ClBotRight -> pure DNorth
    _ -> randPick [DNorth, DEast] rng
  case randDelta of
    DEast -> do
      let
        g' = g
          { gCells = M.adjustWithKey (mkLink g randDelta) coord gCells
          }
      return (g', runStartXCoord)
    _ -> do
      linkNorthXCoord <- randPick [runStartXCoord..x] rng
      let
        g' = g
          { gCells = M.adjustWithKey (mkLink g randDelta) (linkNorthXCoord, y) gCells
          }
      return (g', nextXCoord)
  where
  coord@(x, y) = cCoord cell
  cl = getCellLoc g cell
  nextXCoord = if x == gSizeX - 1
    then 0
    else x + 1

mkLink :: Grid -> CoordDelta -> Coord -> Cell -> Cell
mkLink Grid{..} d coord c = case M.lookup dCoord gCells of
    Just neighbor -> linkCell neighbor c
    Nothing -> c
  where
  dCoord = go d coord

-- | Given a list of elements, randomly pick one.
randPick :: [a] -> GenIO -> IO a
randPick xs rng
  = (xs !!)
  <$> (uniformR (0, length xs - 1) rng :: IO Int)

type XCoord = Int
type YCoord = Int
type Coord = (XCoord, YCoord)

type Links = S.Set Coord

data Cell = Cell
  { cCoord :: Coord
  , cLinks :: Links
  } deriving (Eq, Ord, Show)

-- | Add b into a's list of linked Cells.
linkCell :: Cell -> Cell -> Cell
linkCell b a = a
  { cLinks = S.insert (cCoord b) (cLinks a)
  }

-- | Link cells together (bidirectionally).
linkCells :: Cell -> Cell -> (Cell, Cell)
linkCells a b = (a', b')
  where
  a' = linkCell b a
  b' = linkCell a b

-- | Remove b from a's list of linked Cells.
unlinkCell :: Cell -> Cell -> Cell
unlinkCell b a = a
  { cLinks = S.delete (cCoord b) (cLinks a)
  }

getLinked :: Cell -> Grid -> [Cell]
getLinked Cell{..} Grid{..}
  = mapMaybe (flip M.lookup gCells)
  $ S.elems cLinks

-- | See if a's list of links includes b.
isLinked :: Cell -> Cell -> Bool
isLinked b a = S.member (cCoord b) $ cLinks a

isLinkedBoth :: Cell -> Cell -> Bool
isLinkedBoth b a = isLinked b a && isLinked a b

-- | A cell is a neighbor if it is a potential linkable Cell depending on the
-- Maze algorithm we are using.
neighbors :: [Coord -> Coord] -> Cell -> Grid -> [Cell]
neighbors deltas Cell{..} g = mapMaybe (flip getCell g . ($ cCoord)) deltas

data Grid = Grid
  { gSizeX :: Int
  , gSizeY :: Int
  , gCoords :: [Coord]
  , gCells :: M.Map Coord Cell
  } deriving (Eq, Ord, Show)

getCell :: Coord -> Grid -> Maybe Cell
getCell coord Grid{..} = M.lookup coord gCells

initGrid :: (Int, Int) -> Grid
initGrid (sizeX, sizeY) = Grid
    { gSizeX = sizeX
    , gSizeY = sizeY
    , gCoords = gridCoords
    , gCells = foldl step M.empty gridCoords
    }
  where
  gridCoords =
    [ (x, y)
    | x <- [0..(sizeX - 1)]
    , y <- [0..(sizeY - 1)]
    ]
  step acc coord = M.insert coord emptyCell acc
    where
    emptyCell = Cell
      { cCoord = coord
      , cLinks = S.empty
      }

go :: CoordDelta -> Coord -> Coord
go cDelta (x, y) = case cDelta of
  DNorth -> (x, y + 1)
  DSouth -> (x, y - 1)
  DEast -> (x + 1, y)
  DWest -> (x - 1, y)

-- | Check if a Coord is out of bounds.
oob :: Coord -> Bool
oob (x, y) = x < 0 || y < 0

data CoordDelta
  = DNorth
  | DSouth
  | DEast
  | DWest
  deriving (Eq, Enum, Ord, Show)

randCell :: Grid -> GenIO -> IO Cell
randCell Grid{..} rng = do
  randCoord <- uniform rng :: IO (Int, Int)
  return
    . fromJustNote "randCell"
    $ M.lookup randCoord gCells

rowWise :: Grid -> [[Cell]]
rowWise Grid{..} = map collectCells rows
  where
  rows = [0..(gSizeY - 1)]
  collectCells :: Int -> [Cell]
  collectCells row
    =  mapMaybe (flip M.lookup gCells)
    . sort
    $ filter ((row==) . snd) gCoords
