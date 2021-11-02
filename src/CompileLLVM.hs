module CompileLLVM where

import AbsInstant

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe
import Data.Map as Map

-- values
data Val = VVal Integer | VReg Integer deriving Show
-- arithmetic operators
data Op = Add | Sub | Mul | Div deriving Show
-- abstract type describing llvm statement
data LLStmt = Arithm Op Val Val
            | Print Reg
            | Alloca
            | Load Reg
            | Store Val Reg
            deriving Show
-- register value
newtype Reg = Reg Integer deriving Show
-- number of currently used registers
newtype CurrReg = CurrReg Integer deriving Show
-- current variable values, allocated and stored in registers
newtype Vars = Vars (Map.Map String Reg) deriving Show

setVar :: String -> Val -> [LLStmt] -> CurrReg -> Vars -> ([LLStmt], CurrReg, Vars)
setVar ident val llstmts (CurrReg currReg) (Vars vars) = do
  if Map.notMember ident vars then do
    let newVars = Vars (Map.insert ident (Reg (currReg + 1)) vars)
    (llstmts ++ [Alloca] ++ [Store val (Reg (currReg + 1))], CurrReg (currReg + 2), newVars)
  else do
    let reg = getVarReg ident (Vars vars)
    (llstmts ++ [Store val reg], CurrReg (currReg + 1), (Vars vars))

getVarReg :: String -> Vars -> Reg
getVarReg ident (Vars vars) =
  case Map.lookup ident vars of
    Nothing -> Reg $ -1
    Just reg -> reg

arithmExprTollstmts :: Op -> Exp -> Exp -> CurrReg -> Vars -> ([LLStmt], CurrReg, Reg)
arithmExprTollstmts op exp1 exp2 currReg vars = do
  let (llstmts1, CurrReg currReg1, Reg reg1) = exprTollstmts exp1 currReg vars
  let (llstmts2, CurrReg currReg2, Reg reg2) = exprTollstmts exp2 (CurrReg currReg1) vars
  (llstmts1 ++ llstmts2 ++ [Arithm op (VReg reg1) (VReg reg2)], CurrReg $ currReg2 + 1, Reg $ currReg2 + 1)

exprTollstmts :: Exp -> CurrReg -> Vars -> ([LLStmt], CurrReg, Reg)
exprTollstmts (ExpLit val) (CurrReg currReg) vars = ([Arithm Add (VVal val) (VVal 0)], CurrReg $ currReg + 1, Reg $ currReg + 1)
exprTollstmts (ExpVar (Ident ident)) (CurrReg currReg) vars = ([Load (getVarReg ident vars)], CurrReg $ currReg + 1, Reg $ currReg + 1)
exprTollstmts (ExpAdd x y) currReg vars = arithmExprTollstmts Add x y currReg vars
exprTollstmts (ExpSub x y) currReg vars = arithmExprTollstmts Sub x y currReg vars
exprTollstmts (ExpMul x y) currReg vars = arithmExprTollstmts Mul x y currReg vars
exprTollstmts (ExpDiv x y) currReg vars = arithmExprTollstmts Div x y currReg vars

stmtTollstmts :: Stmt -> CurrReg -> Vars -> ([LLStmt], CurrReg, Vars)
stmtTollstmts (SAss (Ident ident) expr) currReg vars = do
  let (llstmts1, currReg1, Reg reg1) = exprTollstmts expr currReg vars
  let (llstmts2, currReg2, vars2) = setVar ident (VReg reg1) llstmts1 currReg1 vars
  (llstmts2, currReg2, vars2)
stmtTollstmts (SExp expr) currReg vars = do
  let (llstmts1, CurrReg currReg1, reg1) = exprTollstmts expr currReg vars
  (llstmts1 ++ [Print reg1], CurrReg $ currReg1 + 1, vars)

programToLLStmts :: Program -> [LLStmt] -> CurrReg -> Vars -> [LLStmt]
programToLLStmts (Prog []) llstmts _ _ = llstmts
programToLLStmts (Prog (stmt:stmts)) llstmts currReg vars = do
  let (newllstmts, newCurrReg, newVars) = stmtTollstmts stmt currReg vars
  programToLLStmts (Prog stmts) (llstmts ++ newllstmts) newCurrReg newVars

-- writing all llstmts to LLVM language in main function --
-- outputBody :: [LLStmt] -> Integer -> [String] -> [String]
-- outputBody [] _ lines = lines
-- outputBody (llstmt:llstmts) lineNr lines =


llStmtsToOutput :: [LLStmt] -> String -- todo zmienic na stringify
llStmtsToOutput llstmts = show llstmts


-- main compile function --
compileProgram :: Program -> String
compileProgram program = do
  let llstmts = programToLLStmts program [] (CurrReg 0) (Vars Map.empty)
  llStmtsToOutput llstmts


