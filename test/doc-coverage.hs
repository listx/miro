module Main where

import Data.List (genericLength)
import Data.Maybe (catMaybes)
import System.Console.ANSI
import System.Exit (exitFailure, exitSuccess)
import System.Process (readProcessWithExitCode)
import Text.Regex (matchRegex, mkRegex)

average :: (Fractional a, Real b) => [b] -> a
average xs = realToFrac (sum xs) / genericLength xs

expected :: Fractional a => a
expected = 5

main :: IO ()
main = do
  (_, sout, serr) <- readProcessWithExitCode "stack" ["haddock", "--no-haddock-deps"] ""
  let
    output = sout ++ serr
  printDocCoverage output
  if (average (match output) :: Double) >= expected
    then do
      setSGR [SetColor Foreground Dull Green]
      putStrLn "Documentation coverage OK"
      setSGR []
      exitSuccess
    else do
      setSGR [SetColor Foreground Dull Red]
      putStrLn "Documentation coverage FAIL"
      setSGR []
      exitFailure
  where
  printDocCoverage output = do
    setSGR [SetColor Foreground Dull Yellow]
    putStrLn "--- BEGIN Documentation coverage ---"
    setSGR []
    putStr . unlines . map ("  "++) $ lines output
    setSGR [SetColor Foreground Dull Yellow]
    putStrLn "--- END   Documentation coverage ---"
    setSGR []

match :: String -> [Int]
match = fmap read . concat . catMaybes . fmap f . lines
  where
  f = matchRegex $ mkRegex "^ *([0-9]*)% "
