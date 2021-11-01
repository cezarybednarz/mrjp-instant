module Main where

import Compiler ( compile )
import System.Environment ( getArgs )
import Data.Map.Lazy as LazyMap ()
import System.Exit ( exitFailure, exitSuccess )

main :: IO ()
main = do
  compile