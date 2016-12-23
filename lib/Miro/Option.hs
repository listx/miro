{-|
Description : Option handling.
Copyright   : (c) 2016-2017 Linus Arver <linus@ucla.edu>
License     : BSD2
Maintainer  : linus@ucla.edu
Stability   : experimental
Portability : POSIX

Option handling.
-}

{-# LANGUAGE RecordWildCards #-}

module Miro.Option where

import Control.Monad
import Data.List
import Data.Word
import Options.Applicative
--import qualified Text.Show.Pretty as Pr

import Miro.Maze
import Miro.Output

data Opts = Opts
  { optMazeType :: MazeType
  , optOutput :: Output
  , optSeed :: (Word64, Word64)
  , optSize :: (Int, Int)
  , optQuiet :: Bool
  }

parseShowOptable :: (ShowOpt a, Enum a) => ReadM a
parseShowOptable = eitherReader rawParse
  where
  rawParse s = case lookup s $ mkShowOptHash firstOpt of
    Just found -> Right found
    Nothing -> Left $ unwords
      [ "cannot parse"
      , "`" ++ s ++ "'" ++ "\n"
      , " " ++ usageAllVals firstOpt
      ]
  firstOpt = toEnum 0

optMazeTypeParser :: Parser MazeType
optMazeTypeParser = option parseShowOptable
  ( long "maze-type"
  <> short 't'
  <> value MTBinaryTree
  <> showDefaultWith showOpt
  <> metavar "MAZE-TYPE"
  <> help ("Maze algorithm to use. " ++ allVals MTBinaryTree)
  )

optOutputParser :: Parser Output
optOutputParser = option parseShowOptable
  ( long "output"
  <> short 'o'
  <> value OUnicode
  <> showDefaultWith showOpt
  <> metavar "OUTPUT"
  <> help ("Output type. " ++ allVals OAscii)
  )

optSeedParser :: (Word64, Word64) -> Parser (Word64, Word64)
optSeedParser randSeed = option auto
  ( long "rng-seed"
  <> short 'r'
  <> value randSeed
  <> metavar "(UINT64,UINT64)"
  <> help "Seed to use. If absent, a random seed is chosen."
  )

optSizeParser :: Parser (Int, Int)
optSizeParser = option auto
  ( long "size"
  <> short 's'
  <> value (20, 20)
  <> showDefault
  <> metavar "(SIZE-X,SIZE-Y)"
  <> help "Size of maze, in number of cells. If either SIZE-X or SIZE-Y is less than 2, it is set to 2."
  )

optQuietParser :: Parser Bool
optQuietParser = switch
  ( long "quiet"
  <> short 'q'
  <> help "Whether to be quiet. Good if you want to just get the maze as output, nothing else."
  )

generateMaze :: Opts -> IO ()
generateMaze o@Opts{..} = do
  let
    (s1, s2) = optSeed
  rng <- initialize s1 s2
  maze <- mkMaze optMazeType g rng
  case optOutput of
    OAscii -> printGridAscii maze
    OUnicode -> printGridUnicode maze
  when (not optQuiet) $ showMazeInfo o
  where
  g = initGrid optSize

showMazeInfo :: Opts -> IO ()
showMazeInfo Opts{..} = mapM_ putStrLn
  [ "Maze Type: " ++ showOpt optMazeType
  , "RNG Seed: " ++ show optSeed
  , "Size: " ++ show optSize
  ]

sanitizeOpts :: Opts -> IO Opts
sanitizeOpts o@Opts{..}
  | x < 2 = sanitizeOpts o
    { optSize = (2, y)
    }
  | y < 2 = sanitizeOpts o
    { optSize = (x, 2)
    }
  | otherwise = pure o
  where
  (x, y) = optSize

class ShowOpt a where
  showOpt :: a -> String

instance ShowOpt MazeType where
  showOpt a = case a of
    MTBinaryTree -> "binary-tree"
    MTSidewinder -> "sidewinder"

instance ShowOpt Output where
  showOpt a = case a of
    OAscii -> "ascii"
    OUnicode -> "unicode"

usageAllVals :: (Enum a, ShowOpt a) => a -> String
usageAllVals a = unwords
  [ "allowed values:"
  , allVals a
  ]

allVals :: (Enum a, ShowOpt a) => a -> String
allVals a
  = (\s -> "[" ++ s ++ "]")
  . intercalate "|"
  . map fst
  $ mkShowOptHash a

mkShowOptHash :: (Enum a, ShowOpt a) => a -> [(String, a)]
mkShowOptHash a = map (\a' -> (showOpt a', a') ) [a ..]
