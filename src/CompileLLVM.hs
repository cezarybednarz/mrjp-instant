module CompileLLVM where

import AbsInstant

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe
import Data.Map as Map

-- values
data Val = VConst Integer | VReg Integer deriving Show
-- arithmetic operators
data Op = Add | Sub | Mul | Div deriving Show
-- abstract type describing llvm statement
data LLStmt = Arithm Op Reg Reg
            | Print Reg
            | Alloca
            | Store Val
            deriving Show
-- register value
newtype Reg = Reg Integer deriving Show
-- number of currently used registers
newtype CurrReg = CurrReg Integer deriving Show
-- current variable values, allocated and stored in registers
newtype Vars = Vars (Map.Map String Reg) deriving Show

setVar :: String -> Reg -> [LLStmt] -> CurrReg -> Vars -> ([LLStmt], CurrReg, Vars)
setVar ident (Reg reg) llstmts (CurrReg currReg) (Vars vars) = do
  let newVars = Vars (Map.insert ident (Reg (currReg + 1)) vars)
  (llstmts ++ [Store $ VReg reg], CurrReg (currReg + 1), newVars)

getVarReg :: String -> Vars -> Reg
getVarReg ident (Vars vars) =
  case Map.lookup ident vars of
    Nothing -> Reg $ -1
    Just reg -> reg


arithmExprTollstmts :: Op -> Exp -> Exp -> CurrReg -> Vars -> ([LLStmt], CurrReg, Reg)
arithmExprTollstmts op exp1 exp2 currReg vars = do
  let (llstmts1, CurrReg currReg1, reg1) = exprTollstmts exp1 currReg vars
  let (llstmts2, CurrReg currReg2, reg2) = exprTollstmts exp2 (CurrReg currReg1) vars
  (llstmts1 ++ llstmts2 ++ [Arithm op reg1 reg2], CurrReg $ currReg2 + 1, Reg $ currReg2 + 1)

exprTollstmts :: Exp -> CurrReg -> Vars -> ([LLStmt], CurrReg, Reg)
exprTollstmts (ExpLit val) (CurrReg currReg) vars = ([Store $ VConst val], CurrReg $ currReg + 1, Reg $ currReg + 1)
exprTollstmts (ExpVar (Ident ident)) currReg vars = ([], currReg, getVarReg ident vars)
exprTollstmts (ExpAdd x y) currReg vars = arithmExprTollstmts Add x y currReg vars
exprTollstmts (ExpSub x y) currReg vars = arithmExprTollstmts Sub x y currReg vars
exprTollstmts (ExpMul x y) currReg vars = arithmExprTollstmts Mul x y currReg vars
exprTollstmts (ExpDiv x y) currReg vars = arithmExprTollstmts Div x y currReg vars

stmtTollstmts :: Stmt -> CurrReg -> Vars -> ([LLStmt], CurrReg, Vars)
stmtTollstmts (SAss (Ident ident) expr) currReg vars = do
  let (llstmts1, currReg1, reg1) = exprTollstmts expr currReg vars
  let (llstmts2, currReg2, vars2) = setVar ident reg1 llstmts1 currReg1 vars
  (llstmts2, currReg2, vars2)
stmtTollstmts (SExp expr) currReg vars = do
  let (llstmts1, (CurrReg currReg1), reg1) = exprTollstmts expr currReg vars
  (llstmts1 ++ [Print reg1], (CurrReg $ currReg1 + 1), vars)

programToLLStmts :: Program -> [LLStmt] -> CurrReg -> Vars -> [LLStmt]
programToLLStmts (Prog [stmt]) llstmts currReg vars = do
  let (newllstmts, _, _) = stmtTollstmts stmt currReg vars
  (llstmts ++ newllstmts) 
programToLLStmts (Prog (stmt:stmts)) llstmts currReg vars = do
  let (newllstmts, newCurrReg, newVars) = stmtTollstmts stmt currReg vars
  programToLLStmts (Prog stmts) (llstmts ++ newllstmts) newCurrReg newVars

-- writing all llstmts to LLVM language in main function --
llStmtsToOutput :: [LLStmt] -> String -- todo zmienic na stringify
llStmtsToOutput = show

-- main compile function --
compileProgram :: Program -> String
compileProgram program = do
  let llstmts = programToLLStmts program [] (CurrReg 0) (Vars Map.empty)
  llStmtsToOutput llstmts


