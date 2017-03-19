{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Miro.Output where

import Data.Maybe
import Test.Tasty
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series
import Test.Tasty.TH

import Miro.Maze
import Miro.Output

newtype StringGrid = StringGrid [String]
  deriving (Eq, Ord, Show)

stringGrid :: Int -> [String]
stringGrid n = replicate n $ replicate n 'a'

stringGridChoice :: Int -> [[String]]
stringGridChoice depth = [map stringGrid [1..] !! depth]

instance Monad m => Serial m StringGrid where
  series = StringGrid <$> generate stringGridChoice

prop_getCoordChar :: Coord -> StringGrid -> Bool
prop_getCoordChar coord (StringGrid rows)
  | validCoord coord rows
    = isJust $ getCoordChar coord rows
  | otherwise = isNothing $ getCoordChar coord rows

tests :: TestTree
tests = $(testGroupGenerator)
