module CompileJVM where

import AbsInstant

import Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe

-- ! skorzystać z map size i unionWith

newtype Val = Val Integer
newtype Reg = Reg Integer

data Op = Add | Sub | Mul | Div

data JVMStmt = Store Reg 
             | Load Reg
             | Const Val 
             | Print 
             | Swap 
             | Arithm Op 
             deriving Show

data JVMStmts = JVMStmts { stackCount :: Integer,
                           stmts :: [JVMStmt],
                           vars :: (Map.Map String Reg)
                         } deriving Show
--data JVMVars = Vars (Map.Map String Reg) deriving Show

setVar :: String -> Reg -> JVMStmts -> JVMStmts 
setVar ident reg jvmStmts = do
  -- todo uproscic to 
  if Map.notMember ident jvmStmts.JVMVars then
    JVMStmts { stackCount = jvmStmts.stackCount, 
               stmts = jvmStmts.stmts ++ [Store reg],
               vars = Map.insert ident reg jvmStmts.vars }
  else
    JVMStmts { stackCount = jvmStmts.stackCount, 
               stmts = jvmStmts.stmts ++ [Store reg],
               vars = jvmStmts.vars }
    

getVarReg :: String -> JVMStmts -> Reg
getVarReg ident jvmStmts =
  case Map.lookup ident jvmStmts.JVMVars of
    Nothing -> Reg $ -1
    Just reg -> reg

exprToJVMStmts :: Exp -> JVMStmts -> JVMStmts
exprToJVMStmts exp jvmStmts = do
  -- todo

stmtToJVMStmts :: Stmt -> JVMStmts -> JVMStmts 
stmtToJVMStmts (SAss (Ident ident) expr) jvmStmts = do
  let jvmStmts1 = exprToJVMStmts epxr jvmStmts 
  let reg = getVarReg ident jvmStmts.JVMVars
  JVMStmts {}

programToJVMStmts :: Program -> JVMStmts -> JVMStmts
programToJVMStmts (Prog []) jvmStmts _ _ = jvmStmts
programToJVMStmts (Prog (stmt:stmts)) jvmStmts jvmStmts jvmvars = do
  -- todo

-- writing all jvmStmts to JVM language in main function --
jvmStmtsToOutput :: [JVMStmt] -> [String]
jvmStmtsToOutput jvmStmts = show jvmStmts --todo

-- main compilation function --
compileProgram :: Program -> String
compileProgram program = 
  let jvmStmts = programToJVMStmts program
  unlines $ jvmStmtsToOutput jvmStmts