{-|
Description : Maze rendering.
Copyright   : (c) 2016-2017 Linus Arver <linus@ucla.edu>
License     : BSD2
Maintainer  : linus@ucla.edu
Stability   : experimental
Portability : POSIX

Maze rendering utilities.
-}

{-# LANGUAGE RecordWildCards #-}

module Miro.Output where

import Data.Maybe
import qualified Data.Set as S

import Miro.Maze

data Output
  = OAscii
  | OUnicode
  deriving (Enum, Eq, Show)

printGridAscii :: Grid -> IO ()
printGridAscii = putStr . gridToAscii

printGridUnicode :: Grid -> IO ()
printGridUnicode = putStr . asciiToUnicode . gridToAscii

validCoord :: Coord -> [String] -> Bool
validCoord (x, y) rows
  | y < 0 || length rows - 1 < y = False
  | x < 0 || length columns - 1 < x = False
  | otherwise = True
  where
  columns = rows !! y

getCoordChar :: Coord -> [String] -> Maybe Char
getCoordChar coord@(x, y) rows
  | validCoord coord rows = Just $ (rows !! y) !! x
  | otherwise = Nothing

gridToAscii :: Grid -> String
gridToAscii g@Grid{..}
  = init -- Drop extra trailing newline.
  . unlines
  . sandwich
  . reverse
  . map
      ( ( \tuples
        -> "+"
        ++ concatMap fst tuples
        ++ "\n"
        ++ "|"
        ++ concatMap snd tuples
        ++ "\n"
        )
      . map (cellToAscii g)
      )
  $ rowWise g
  where
  sandwich xs =
    [ init $ concat xs
    , horizRuler
    ]
  horizRuler = concatMap (const "+---") [0..(gSizeX - 1)] ++ "+\n"

-- | Each cell requires 2 lines to render, "top" and "bot".
cellToAscii :: Grid -> Cell -> (String, String)
cellToAscii Grid{..} Cell{..} = (top, bot)
  where
  body = "   "
  bot = body ++ wallEast
  wallEast = if S.member eastCoord cLinks then " " else "|"
  eastCoord = go DEast cCoord
  wallNorth = if S.member northCoord cLinks then "   " else "---"
  northCoord = go DNorth cCoord
  top = wallNorth ++ "+"

asciiToUnicode :: String -> String
asciiToUnicode xs
  = f
  . zip [0..] -- [(0, "....."), (1, "......"), ...]
  . reverse
  $ lines xs
  where
  f :: [(Int, String)] -> String
  f rowTuples
    = unlines
    . fst
    $ foldl rowStep ([], rowTuples) rowTuples
  rowStep (accRows, rowTuples) (y, rowStr) =
    ( ((\(a, _, _, _) -> a)
      . foldl colStep ("", rowTuples, rowStr, y)
      $ zip ([0..]::[Int]) rowStr) : accRows
    , rowTuples
    )
  colStep (accRow, rowTuples, rowStr, y) (x, c) =
    ( accRow ++ [unicode]
    , rowTuples
    , rowStr
    , y
    )
    where
    unicode = case c of
      '+'
        -- Corners do not need any special logic.
        | all isNothing [charNorth, charWest] -> topLeftCorner
        | all isNothing [charNorth, charEast] -> topRightCorner
        | all isNothing [charSouth, charWest] -> botLeftCorner
        | all isNothing [charSouth, charEast] -> botRightCorner
        -- For the rest, we try to see what neighboring characters exist.
        | isNothing charNorth && charIs ' ' charSouth -> horizBar
        | isNothing charNorth && charIs '|' charSouth -> topJoint
        | isNothing charSouth && charIs ' ' charNorth -> horizBar
        | isNothing charSouth && charIs '|' charNorth -> botJoint
        | isNothing charEast && charIs ' ' charWest -> vertBar
        | isNothing charEast && charIs '-' charWest -> rightJoint
        | isNothing charWest && charIs ' ' charEast -> vertBar
        | isNothing charWest && charIs '-' charEast -> leftJoint
        | charAround ' ' '-' '|' '-' -> topJoint
        | charAround '|' '-' ' ' '-' -> botJoint
        | charAround '|' '-' '|' ' ' -> leftJoint
        | charAround '|' ' ' '|' '-' -> rightJoint
        | charAround '|' '-' ' ' ' ' -> botLeftCorner
        | charAround '|' ' ' ' ' '-' -> botRightCorner
        | charAround ' ' '-' '|' ' ' -> topLeftCorner
        | charAround ' ' ' ' '|' '-' -> topRightCorner
        | charAround ' ' '-' ' ' '-' -> horizBar
        | charAround '|' ' ' '|' ' ' -> vertBar
        | charAround ' ' ' ' '|' ' ' -> botExtend
        | charAround ' ' ' ' ' ' '-' -> leftExtend
        | charAround ' ' '-' ' ' ' ' -> rightExtend
        | otherwise -> cross
      '|' -> vertBar
      '-' -> horizBar
      c' -> c'
    coord = (x, y)
    charAround northC eastC southC westC
      = charIs northC charNorth
      && charIs southC charSouth
      && charIs eastC charEast
      && charIs westC charWest
    charIs plainC maybeC = maybeC == Just plainC
    getCharDelta d = getCoordChar (go d coord) $ map snd rowTuples
    charNorth = getCharDelta DNorth
    charSouth = getCharDelta DSouth
    charEast = getCharDelta DEast
    charWest = getCharDelta DWest
  topLeftCorner
    , topRightCorner
    , botLeftCorner
    , botRightCorner
    , cross
    , horizBar
    , vertBar
    , leftJoint
    , rightJoint
    , topJoint
    , botExtend
    , leftExtend
    , rightExtend
    , botJoint :: Char
  topLeftCorner = toEnum 0x250c
  topRightCorner = toEnum 0x2510
  botLeftCorner = toEnum 0x2514
  botRightCorner = toEnum 0x2518
  cross = toEnum 0x253c
  horizBar = toEnum 0x2500
  vertBar = toEnum 0x2502
  leftJoint = toEnum 0x251c
  rightJoint = toEnum 0x2524
  topJoint = toEnum 0x252c
  botJoint = toEnum 0x2534
  botExtend = toEnum 0x2577
  leftExtend = toEnum 0x2574
  rightExtend = toEnum 0x2576
