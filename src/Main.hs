module Main where

import System.Environment ( getArgs )
import Data.Map.Lazy as LazyMap ()
import System.Exit ( exitFailure, exitSuccess )
import System.IO ( stderr, hPutStrLn )

main :: IO ()
main = do
    args <- getArgs
    print args