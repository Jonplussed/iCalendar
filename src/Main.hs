module Main where

import System.Environment (getArgs)
import Text.ICal (parseICal)

main :: IO ()
main = do
  args <- getArgs
  file <- readFile (args !! 0)
  putStrLn . parseICal $ file
