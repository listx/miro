{-# LANGUAGE TemplateHaskell #-}

module Test.Miro.Option where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Miro.Option

data Foo
  = Foo
  | Bar
  deriving (Enum, Eq, Show)

instance ShowOpt Foo where
  showOpt a = case a of
    Foo -> "foo"
    Bar -> "bar"

case_usageAllVals :: Assertion
case_usageAllVals
  = usageAllVals Foo
  @?= "allowed values: [foo|bar]"

tests :: TestTree
tests = $(testGroupGenerator)
