{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Tasty
import Test.Tasty.TH

import Test.Miro.Option
import Test.Miro.Output

test_all :: [TestTree]
test_all =
  [ Test.Miro.Option.tests
  , Test.Miro.Output.tests
  ]

main :: IO ()
main = $(defaultMainGenerator)
