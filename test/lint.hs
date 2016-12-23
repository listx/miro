module Main where

import Language.Haskell.HLint (hlint)
import System.Console.ANSI
import System.Exit (exitFailure, exitSuccess)

arguments :: [String]
arguments =
  [ "app"
  , "bench"
  , "lib"
  , "test"
  ]

main :: IO ()
main = do
  hints <- hlint arguments
  if null hints
    then do
      setSGR [SetColor Foreground Dull Green]
      putStrLn "Lint OK"
      setSGR []
      exitSuccess
    else do
      setSGR [SetColor Foreground Dull Red]
      putStrLn "Lint FAIL"
      setSGR []
      exitFailure
