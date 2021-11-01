module CompileLLVM where

import AbsInstant

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe
import Data.Map as Map

-- values
data Val = VConst Integer | VReg Reg deriving Show
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

cRegToReg :: CurrReg -> Reg
cRegToReg (CurrReg currReg) = Reg currReg

incCurrReg :: CurrReg -> Integer -> CurrReg
incCurrReg (CurrReg currReg) val = CurrReg (currReg + val)

setVar :: String -> Val -> [LLStmt] -> CurrReg -> Vars -> ([LLStmt], CurrReg, Vars)
setVar ident val llstmts (CurrReg currReg) (Vars vars) =
  if Map.member ident vars then do
    newVars <- Vars (Map.insert ident (Reg (currReg + 1)) vars)
    (llstmts ++ [Alloca] ++ [Store val], CurrReg (currReg + 2), newVars)
  else
    (llstmts ++ [Store val], CurrReg (currReg + 1), Vars vars)

getVarReg :: String -> Vars -> Reg
getVarReg ident vars =
  Map.lookup ident vars

arithmExprTollstmts :: Op -> Exp -> Exp -> CurrReg -> Vars -> ([LLStmt], CurrReg, Reg)
arithmExprTollstmts op exp1 exp2 currReg vars = do
  (llstmts1, currReg1, _) <- exprTollstmts exp1 currReg vars
  (llstmts2, currReg2, _) <- exprTollstmts exp2 currReg1 vars
  (llstmts2 ++ [Arithm op currReg1 currReg2], currReg2 + 1, currReg2 + 1)

exprTollstmts :: Exp -> CurrReg -> Vars -> ([LLStmt], CurrReg, Reg)
exprTollstmts (ExpLit val) currReg vars = ([Store val], currReg + 1, currReg + 1)
exprTollstmts (ExpVar ident) currReg vars = ([], currReg, getVarReg ident vars)
exprTollstmts (ExpAdd x y) currReg vars = arithmExprTollstmts Add x y currReg vars
exprTollstmts (ExpSub x y) currReg vars = arithmExprTollstmts Sub x y currReg vars
exprTollstmts (ExpMul x y) currReg vars = arithmExprTollstmts Mul x y currReg vars
exprTollstmts (ExpDiv x y) currReg vars = arithmExprTollstmts Div x y currReg vars

stmtTollstmts :: Stmt -> CurrReg -> Vars -> ([LLStmt], CurrReg, Vars)
stmtTollstmts (SAss ident expr) currReg vars = do
  (newllstmts, newCurrReg, val) <- exprTollstmts expr currReg vars
  return (newllstmts, newCurrReg, setVar ident val)
stmtTollstmts (SExp expr) currReg vars = do
  (newllstmts, newCurrReg, val) <- exprTollstmts expr currReg vars
  return (newllstmts ++ [Print val], newCurrReg, vars)

programToLLStmts :: Program -> [LLStmt] -> CurrReg -> Vars -> [LLStmt]
programToLLStmts (Prog []) llstmts _ _ = do
  llstmts
programToLLStmts (Prog (stmt:stmts)) llstmts currReg vars = do
  (newllstmts, newCurrReg, newVars) <- stmtTollstmt stmt currReg vars
  programToLLStmts (Prog stmts) (llstmts ++ newllstmts) newCurrReg newVars

-- writing all llstmts to LLVM language in main function --
llStmtsToOutput :: [LLStmt] -> String -- todo zmienic na stringify
llStmtsToOutput = show

-- main compile function --
compileProgram :: Program -> String
compileProgram program = do
  llstmts <- programToLLStmts program [] (CurrReg 0) (Vars Map.empty)
  llStmtsToOutput llstmts


