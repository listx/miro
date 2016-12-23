{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.DeepSeq
import Criterion.Main
import Criterion.Types

import Miro.Maze

-- | This instance of NFData for the 'GenIO' type feels like rubbish, but it
-- should be OK for our purposes because the usage of GenIO in our maze
-- generation functions requires us to call out via FFI to the underlying C
-- library (this is how pcg-random works). There is no laziness at all in C.
instance NFData GenIO where rnf !_ = ()

-- | The use of 'seq' here is inspired by
-- https://bonsaicode.wordpress.com/2009/04/27/forcing-evaluation-in-haskell/.
instance NFData Grid where
  rnf !Grid{..}
    = rnf gSizeX
    `seq` rnf gSizeY
    `seq` rnf gCoords
    `seq` rnf gCells

instance NFData Cell where
  rnf !Cell{..}
    = rnf cCoord
    `seq` rnf cLinks

{-# ANN main "HLint: ignore Redundant irrefutable pattern" #-}
-- | We use a fixed RNG seed so that independent benchmark runs use the same
-- data.
main :: IO ()
main = do
  rng0 <- initialize 0 0 :: IO GenIO
  let
    mkRng = return rng0
    grid = initGrid (100,100)
  defaultMainWith
    (defaultConfig {reportFile = Just "bench.html"})
    [ env mkRng
      -- We have to lazy pattern match here because otherwise we get significant
      -- fluctuations in computation time (such that the more complicated
      -- Sidewinder algorithm performs faster than Binary Tree). My best guess
      -- is that the lazy pattern match ensures that we defer as much as
      -- possible the computation of RNG initialization until the start of nfIO.
      -- See the official docs for 'env' for more information.
      (\ ~rng1 -> bgroup "mkMaze"
        [ bench "MTBinaryTree" . nfIO $ mkMaze MTBinaryTree grid rng1
        , bench "MTSidewinder" . nfIO $ mkMaze MTSidewinder grid rng1
        ]
      )
    ]
