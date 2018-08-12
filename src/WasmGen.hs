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
            I32Mul
            deriving (Eq)
instance Show Inst where
  show (I32Const i) = "(i32.const " ++ show i ++ ")"
  show (Local v)    = "(local " ++ v ++ " i32)"
  show (SetLocal v) = "(set_local " ++ v ++ ")"
  show (GetLocal v) = "(get_local " ++ v ++ ")"
  show I32Add = "(i32.add)"
  show I32Sub = "(i32.sub)"
  show I32Mul = "(i32.mul)"

newtype Wasm = Wasm [Inst]
instance Show Wasm where
  show (Wasm is) = "(module\n(func (export \"main\") (result i32)\n" ++ intercalate "\n" (map show is) ++ "))"

genWasm :: C.Prog -> Wasm
genWasm (C.Prog fds e) = 
  let (b,l) = runState (exp2Wasm e) [] in
  Wasm (l ++ b)

-- The state of this State Monad is definition of local variables.
exp2Wasm :: C.Exp -> State [Inst] [Inst]
exp2Wasm (C.EInt i) = return [I32Const i]
exp2Wasm (C.EVar (C.Var v)) = return [GetLocal ("$"++v)]
exp2Wasm (C.EOp o e1 e2) = do
  is1 <- exp2Wasm e1
  is2 <- exp2Wasm e2
  return $ is1 ++ is2 ++ [op o]
    where op K.OPlus = I32Add
          op K.OMinus = I32Sub
          op K.OTimes = I32Mul
exp2Wasm (C.ELet (C.Var v) e1 e2) = do
  d <- get
  put (Local ("$"++v):d)
  is1 <- exp2Wasm e1
  is2 <- exp2Wasm e2
  return $ is1 ++ [SetLocal ("$"++v)] ++ is2
