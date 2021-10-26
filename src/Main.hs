module Main where

import System.Environment ( getArgs )
import Data.Map.Lazy as LazyMap ()
import System.Exit ( exitFailure, exitSuccess )
import ParInstant 

usage :: IO ()
usage = 
    putStrLn "usage: ./main <input file> <compilation type: llvm or jvm>"


main :: IO ()
main = do
    args <- getArgs
    case args of 
        [] -> usage
        [_] -> usage
        (inputFile:compilationType:_) -> do
            code <- readFile inputFile

            