module CompileLLVM where

import AbsInstant

import Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe

compileProgram :: Program -> IO (Either String String)
compileProgram program = 
    return $ Right "ok llvm"