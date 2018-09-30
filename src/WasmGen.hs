{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module WasmGen where

import qualified Closure as C
import qualified KNormal as K
import Data.List
import Control.Monad.State

data Inst = I32Const Integer | 
            Local String | 
            SetLocal String | 
            GetLocal String | 
            I32Add |
            I32Sub |
            I32Mul |
            I32Less |
            I32Load |
            I32Store | 
            IfThenElse [Inst] [Inst] [Inst]
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
  show (IfThenElse is1 is2 is3) = "(if (result i32)\n" ++ s1 ++ "(then\n" ++ s2 ++ ")\n" ++ "(else\n" ++ s3 ++ "))"
    where s1 = intercalate "\n" (map show is1)
          s2 = intercalate "\n" (map show is2)
          s3 = intercalate "\n" (map show is3)

newtype Wasm = Wasm [Inst]
instance Show Wasm where
  show (Wasm is) = "(module\n(memory 10)\n(func (export \"main\") (result i32)\n" ++ intercalate "\n" (map show is) ++ "))"


stackTopVar = "$stack_top_var"

genWasm :: C.Prog -> Wasm
genWasm (C.Prog fds e) = 
  let (b,l) = runState (exp2Wasm e) [(Local stackTopVar)] in
  Wasm (l ++ b)

-- The state of this State Monad is definition of local variables.
exp2Wasm :: C.Exp -> State [Inst] [Inst]
exp2Wasm (C.EIf e1 e2 e3) = do
  is1 <- exp2Wasm e1
  is2 <- exp2Wasm e2
  is3 <- exp2Wasm e3
  return $ [IfThenElse is1 is2 is3]
exp2Wasm (C.EInt i) = return [I32Const i]
exp2Wasm (C.EVar (C.Var v)) = return [GetLocal ("$"++v)]
exp2Wasm (C.EOp o e1 e2) = do
  is1 <- exp2Wasm e1
  is2 <- exp2Wasm e2
  return $ is1 ++ is2 ++ [op o]
    where op K.OPlus = I32Add
          op K.OMinus = I32Sub
          op K.OTimes = I32Mul
          op K.OLess = I32Less
exp2Wasm (C.ELet (C.Var v) e1 e2) = do
  d <- get
  put (Local ("$"++v):d)
  is1 <- exp2Wasm e1
  is2 <- exp2Wasm e2
  return $ is1 ++ [SetLocal ("$"++v)] ++ is2
exp2Wasm (C.ETuple es) = do
  let setTupleAddr = [I32Const 0, I32Load, I32Const 4, I32Add]
  let prefix = [I32Const 0,I32Const 0,I32Load,I32Const 4,I32Add,I32Store, I32Const 0, I32Load]
  let suffix = [I32Store]
  is <- concat <$> (mapM (\x -> do {y <- exp2Wasm x; return (prefix ++ y ++ suffix)}) es)
  return $ setTupleAddr ++ is
exp2Wasm (C.EDTuple vs e1 e2) = do
  set <- concat <$> mapM f (zip vs (map (*4) [0..]))
  is1 <- exp2Wasm e1
  is2 <- exp2Wasm e2
  return $ is1 ++ [SetLocal stackTopVar] ++ set ++ is2
    where
      f :: (C.Var, Integer) -> State [Inst] [Inst]
      f ((C.Var v), offset) = do
        d <- get
        put (Local ("$"++v):d)
        return $ [GetLocal stackTopVar, I32Const offset, I32Add, I32Load, SetLocal ("$"++v)]
