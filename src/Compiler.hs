module Compiler (compile) where

import ParInstant ( pProgram, myLexer )
import AbsInstant ( Program )
import CompileJVM ( compileProgram )
import CompileLLVM ( compileProgram )

import System.Environment ( getArgs )
import Data.Map.Lazy as LazyMap ()
import System.Exit ( exitFailure, exitSuccess )
import System.IO (hPutStrLn, stderr)

usage :: String
usage = "cabal run compiler -- examples/test02.ins llvm"
  
chooseCompilerType :: Program -> String -> String
chooseCompilerType program compilationType = do
  case compilationType of 
    "jvm" -> CompileJVM.compileProgram program
    "llvm" -> CompileLLVM.compileProgram program
    _ -> usage

compile :: IO ()
compile = do
  args <- getArgs
  case args of 
    [] -> print usage
    [_] -> print usage
    (inputFile:compilationType:_) -> do
      code <- readFile inputFile
      case pProgram $ myLexer code of
        Left errMessage -> do
          hPutStrLn stderr errMessage
          exitFailure
        Right parseTree -> do
          -- print parseTree
          -- print ""
          putStr $ chooseCompilerType parseTree compilationType

            