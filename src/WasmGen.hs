{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module WasmGen where

import qualified Closure as C
import qualified Parse as K
import Data.Maybe
import Data.List
import Control.Monad.State

data Inst = I32Const Int | 
            Local String | 
            SetLocal String | 
            GetLocal String | 
            I32Add |
            I32Sub |
            I32Mul |
            I32Less |
            I32Load |
            I32Store | 
            IfThenElse [Inst] [Inst] [Inst] |
            CallIndirect Int |
            Table Int |
            Func String [String] [Inst]
            deriving (Eq)
instance Show Inst where
  show (I32Const i) = "(i32.const " ++ show i ++ ")"
  show (Local v)    = "(local " ++ v ++ " i32)"
  show (SetLocal v) = "(set_local " ++ v ++ ")"
  show (GetLocal v) = "(get_local " ++ v ++ ")"
  show I32Add = "(i32.add)"
  show I32Sub = "(i32.sub)"
  show I32Mul = "(i32.mul)"
  show I32Less = "(i32.lt_s)"
  show I32Load = "(i32.load)"
  show I32Store = "(i32.store)"
  show (IfThenElse is1 is2 is3) = s1 ++ "\n(if (result i32)\n(i32.eqz)\n(then\n" ++ s3 ++ ")\n" ++ "(else\n" ++ s2 ++ "))"
    where s1 = intercalate "\n" (map show is1)
          s2 = intercalate "\n" (map show is2)
          s3 = intercalate "\n" (map show is3)
  show (CallIndirect n) = "(call_indirect " ++ concat (replicate n "(param i32) ") ++ "(result i32))"
  show (Table n) = "(table " ++ show n ++ " anyfunc)"
  show (Func fn args is) = "(func $" ++ fn ++ " " ++ sargs ++ "(result i32)\n" ++ intercalate "\n" (map show is) ++ ")"
    where sargs = concatMap (\x -> "(param $" ++ x ++ " i32) ") args

-- Wasm FunDefs Main
data Wasm = Wasm [Inst] [Inst]
instance Show Wasm where
  show (Wasm fds is) = 
    let prefix = "(module\n(memory 10)\n" in
    let table = "(table " ++ show (length fds) ++ " anyfunc)\n" in
    let fundefs = intercalate "\n" (map show fds) ++ "\n" in 
    let elem = "(elem (i32.const 0) " ++ unwords (map (\(Func fn _ _) -> "$" ++ fn) fds) ++ ")\n" in
    let mainPrefix = "(func (export \"main\") (result i32)\n" in
    let main = intercalate "\n" (map show is) in 
    let mainSuffix = "))" in
    prefix ++ table ++ fundefs ++ elem ++ mainPrefix ++ main ++ mainSuffix


stackTopVar = "$stack_top_var"

prog2Wasm :: C.Prog -> Wasm
prog2Wasm (C.Prog fds e) = 
  let (b,l) = runState (exp2Wasm e) GenMState {localVariables = [Local stackTopVar], label2index = f} in
  let fdiss = map (fundef2Wasm f) fds in
  Wasm fdiss (localVariables l ++ b)
    where f x = fromJust . lookup x $ zip (map (\(C.FunDef x _ _) -> x) fds) [0..]

fundef2Wasm :: (C.Var -> Int) -> C.FunDef -> Inst
fundef2Wasm f (C.FunDef (C.Label fname) args exp) =
  let (b,l) = runState (exp2Wasm exp) GenMState {localVariables = [Local stackTopVar], label2index = f} in
  Func fname (map l2s args) (localVariables l ++ b)
    where l2s (C.Var v) = v

data GenMState = GenMState { localVariables :: [Inst], label2index :: C.Var -> Int }
type GenM a = (State GenMState a)

putLocal :: String -> GenM ()
putLocal s = do
  d <- get
  put d {localVariables = Local ("$"++s):localVariables d}

getLabelIndex :: C.Var -> GenM Int
getLabelIndex l = do
  d <- get
  return (label2index d l)

exp2Wasm :: C.Exp -> GenM [Inst]
exp2Wasm (C.EIf e1 e2 e3) = do
  is1 <- exp2Wasm e1
  is2 <- exp2Wasm e2
  is3 <- exp2Wasm e3
  return [IfThenElse is1 is2 is3]
exp2Wasm (C.EInt i) = return [I32Const i]
exp2Wasm (C.EVar (C.Var v)) = return [GetLocal ("$"++v)]
exp2Wasm (C.EVar (C.Label l)) = do
  i <- getLabelIndex (C.Label l)
  return [I32Const i]
exp2Wasm (C.EOp o e1 e2) = do
  is1 <- exp2Wasm e1
  is2 <- exp2Wasm e2
  return $ is1 ++ is2 ++ [op o]
    where op K.OPlus = I32Add
          op K.OMinus = I32Sub
          op K.OTimes = I32Mul
          op K.OLess = I32Less
exp2Wasm (C.ELet (C.Var v) e1 e2) = do
  putLocal v
  is1 <- exp2Wasm e1
  is2 <- exp2Wasm e2
  return $ is1 ++ [SetLocal ("$"++v)] ++ is2
exp2Wasm (C.ETuple es) = do
  let setTupleAddr = [I32Const 0, I32Load, I32Const 4, I32Add]
  let prefix = [I32Const 0,I32Const 0,I32Load,I32Const 4,I32Add,I32Store, I32Const 0, I32Load]
  let suffix = [I32Store]
  is <- concat <$> mapM (\x -> do {y <- exp2Wasm x; return (prefix ++ y ++ suffix)}) es
  return $ setTupleAddr ++ is
exp2Wasm (C.EDTuple vs e1 e2) = do
  set <- concat <$> mapM f (zip vs (map (*4) [0..]))
  is1 <- exp2Wasm e1
  is2 <- exp2Wasm e2
  return $ is1 ++ [SetLocal stackTopVar] ++ set ++ is2
    where
      f :: (C.Var, Int) -> GenM [Inst]
      f (C.Var v, offset) = do
        putLocal v
        return [GetLocal stackTopVar, I32Const offset, I32Add, I32Load, SetLocal ("$"++v)]
exp2Wasm (C.EAppCls e1 args) = do
  isargs <- concat <$> mapM exp2Wasm args
  is1 <- exp2Wasm e1
  return $ isargs ++ is1 ++ [SetLocal stackTopVar, GetLocal stackTopVar, GetLocal stackTopVar, I32Load, CallIndirect (1 + length args)]
