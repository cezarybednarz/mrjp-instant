module CompileJVM where

import AbsInstant

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe
import Data.Map as Map

newtype Val = Val Integer deriving Show
newtype Reg = Reg Integer deriving Show

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
                           vars :: Map.Map String Reg
                         } deriving Show

instance Show Op where
  show op = case op of 
    Add -> "iadd"
    Sub -> "isub" 
    Mul -> "imul"
    Div -> "idiv"

addJVMStmt :: JVMStmt -> JVMStmts -> JVMStmts
addJVMStmt jvmStmt jvmStmts =
  JVMStmts { stackCount = stackCount jvmStmts,
             stmts = stmts jvmStmts ++ [jvmStmt],
             vars = vars jvmStmts }

-- insert element to map and add Store to consume top of the stack into register --
setVar :: String -> JVMStmts -> JVMStmts
setVar ident jvmStmts = do
  if Map.notMember ident (vars jvmStmts) then do
    let nextReg = Reg $ (toInteger $ Map.size $ vars jvmStmts) + 1
    JVMStmts { stackCount = stackCount jvmStmts,
               stmts = stmts jvmStmts ++ [Store nextReg],
               vars = Map.insert ident nextReg (vars jvmStmts) }
  else do
    let currReg = getVarReg ident jvmStmts
    JVMStmts { stackCount = stackCount jvmStmts,
               stmts = stmts jvmStmts ++ [Store currReg],
               vars = vars jvmStmts }

getVarReg :: String -> JVMStmts -> Reg
getVarReg ident jvmStmts =
  case Map.lookup ident (vars jvmStmts) of
    Nothing -> Reg $ -1
    Just reg -> reg

printVal :: JVMStmts -> JVMStmts
printVal jvmStmts =
  JVMStmts { stackCount = stackCount jvmStmts,
             stmts = stmts jvmStmts ++ [Print],
             vars = vars jvmStmts
           }

addStackCount :: JVMStmts -> JVMStmts
addStackCount jvmStmts =
  JVMStmts { stackCount = stackCount jvmStmts + 1,
             stmts = stmts jvmStmts,
             vars = vars jvmStmts
           }

combineJVMStmts :: JVMStmts -> JVMStmts -> JVMStmts
combineJVMStmts jvmStmts1 jvmStmts2 = do
  JVMStmts { stackCount = max (stackCount jvmStmts1) (stackCount jvmStmts2),
             stmts = stmts jvmStmts1 ++ stmts jvmStmts2,
             vars = vars jvmStmts2
           }

commutative :: Op -> Bool
commutative op =
  case op of
    Add -> True
    Mul -> True
    Div -> False
    Sub -> False

-- doesnt append to existing statements
arithmExprToJVMStmts :: Op -> Exp -> Exp -> JVMStmts -> JVMStmts
arithmExprToJVMStmts op expr1 expr2 jvmStmts = do
  let jvmStmts1 = exprToJVMStmts expr1 jvmStmts
  let jvmStmts2 = exprToJVMStmts expr2 jvmStmts
  let jvmStmts3
        | stackCount jvmStmts1 >= stackCount jvmStmts2 =
        combineJVMStmts jvmStmts1 (addStackCount jvmStmts2)
        | commutative op =
        combineJVMStmts jvmStmts2 (addStackCount jvmStmts1)
        | otherwise =
        addJVMStmt Swap (combineJVMStmts jvmStmts2 (addStackCount jvmStmts1))
  let jvmStmts4 = addJVMStmt (Arithm op) jvmStmts3
  jvmStmts4

-- doesnt append to existing statements  
exprToJVMStmts :: Exp -> JVMStmts -> JVMStmts
exprToJVMStmts (ExpLit val) jvmStmts =
  JVMStmts { stackCount = 1,
             stmts = [Const (Val val)],
             vars = vars jvmStmts
           }
exprToJVMStmts (ExpVar (Ident ident)) jvmStmts =
  JVMStmts { stackCount = 1,
             stmts = [Load $ getVarReg ident jvmStmts],
             vars = vars jvmStmts
           }
exprToJVMStmts (ExpAdd x y) jvmStmts = arithmExprToJVMStmts Add x y jvmStmts
exprToJVMStmts (ExpSub x y) jvmStmts = arithmExprToJVMStmts Sub x y jvmStmts
exprToJVMStmts (ExpMul x y) jvmStmts = arithmExprToJVMStmts Mul x y jvmStmts
exprToJVMStmts (ExpDiv x y) jvmStmts = arithmExprToJVMStmts Div x y jvmStmts

-- 
stmtToJVMStmts :: Stmt -> JVMStmts -> JVMStmts
stmtToJVMStmts (SAss (Ident ident) expr) jvmStmts = do
  let jvmStmts1 = exprToJVMStmts expr jvmStmts
  setVar ident jvmStmts1
stmtToJVMStmts (SExp expr) jvmStmts = do
  let jvmStmts1 = exprToJVMStmts expr jvmStmts
  printVal jvmStmts1

programToJVMStmts :: Program -> JVMStmts -> JVMStmts
programToJVMStmts (Prog []) jvmStmts = jvmStmts
programToJVMStmts (Prog (stmt:stmts)) jvmStmts = do
  let jvmStmts1 = stmtToJVMStmts stmt jvmStmts
  combineJVMStmts jvmStmts (programToJVMStmts (Prog stmts) jvmStmts1)

-- writing all jvmStmts to JVM language in main function --
optimiseConst :: Integer -> String
optimiseConst val  
  | 0 <= val && val <= 5 = 
  "iconst_" ++ show val
  | val == -1 =
  "iconst_m1"
  | -128 <= val && val <= 127 = 
  "bipush " ++ show val
  | otherwise = 
  "ldc " ++ show val


outputBody :: [JVMStmt] -> [String] -> [String] 
outputBody [] lines = lines
outputBody (jstmt:jstmts) lines = do
  let newLine = "  " ++ case jstmt of 
          (Store (Reg reg)) -> "istore " ++ show reg
          (Load (Reg reg)) -> "iload " ++ show reg
          (Const (Val val)) -> optimiseConst val
          Print -> "invokevirtual  java/io/PrintStream/println(I)V"
          Swap -> "swap"
          (Arithm op) -> show op
  outputBody jstmts (lines ++ [newLine])

jvmStmtsToOutput :: JVMStmts -> [String]
jvmStmtsToOutput jvmStmts = do
  let limitStack = (stackCount jvmStmts) + 1
  let limitLocals = (Map.size $ vars jvmStmts) + 1
  [
    ".class public Compiler",
    ".super java/lang/Object",
    ".method public <init>()V",
    "  aload_0",
    "  invokespecial java/lang/Object/<init>()V",
    "  return",
    ".end method",
    "",
    ".method public static main([Ljava/lang/String;)V",
    ".limit stack " ++ show limitStack,
    ".limit locals " ++ show limitLocals,
    "  getstatic java/lang/System/out Ljava/io/PrintStream;"
    ] 
  ++
  outputBody (stmts jvmStmts) []
  ++
  [
    "  return",
    ".end method"
  ]

-- main compilation function --
compileProgram :: Program -> String
compileProgram program = do
  let jvmStmts = programToJVMStmts program JVMStmts { stackCount = 0, stmts = [], vars = Map.empty }
  unlines $ jvmStmtsToOutput jvmStmts