module Compiler (compile) where


import ParInstant ( pProgram, myLexer )
import AbsInstant ( Program )
import CompileJVM ( compileProgram )
import CompileLLVM ( compileProgram )

import System.Environment ( getArgs )
import Data.Map.Lazy as LazyMap ()
import System.Exit ( exitFailure, exitSuccess )
import System.IO (hPutStrLn, stderr)


usage :: IO ()
usage = 
  hPutStrLn stderr "ERROR: exit code: "


chooseCompilerType :: Program -> String -> IO (Either String String)
chooseCompilerType program compilationType = do
  case compilationType of 
    "jvm" -> CompileJVM.compileProgram program
    "llvm" -> CompileLLVM.compileProgram program
    _ -> do
      usage
      return $ Left "wrong file name"


compile :: IO ()
compile = do
  args <- getArgs
  case args of 
    [] -> usage
    [_] -> usage
    (inputFile:compilationType:_) -> do
      code <- readFile inputFile
      case pProgram $ myLexer code of
        Left errMessage -> do
          hPutStrLn stderr errMessage
          exitFailure
        Right parseTree -> do
          output <- chooseCompilerType parseTree compilationType
          case output of 
            Left message -> hPutStrLn stderr message -- todo
            Right message -> hPutStrLn stderr message -- todo

            