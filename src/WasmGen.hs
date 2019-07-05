-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module WasmGen where

import qualified Closure             as C
import           Control.Monad.State
import           Data.List
import           Data.Maybe
import qualified Parse               as P
import qualified Type                as T
import qualified Link                as L

data Inst = I32Const Int |
            F32Const Float |
            Local P.Type String |
            SetLocal String |
            GetLocal String |
            I32Add |
            I32Sub |
            I32Mul |
            I32Div |
            I32Less |
            F32Add |
            F32Sub |
            F32Mul |
            F32Div |
            F32Less |
            I32Load |
            I32Store |
            F32Load |
            F32Store |
            IfThenElse P.Type [Inst] [Inst] [Inst] |
            CallIndirect P.Type |
            Table Int |
            Func String P.Type [C.Var] [Inst] |
            GCMalloc [Inst] Bool |
            GCIncreaseRC Int |
            GCDecreaseRC Int |
            PrintInt |
            PrintFloat |
            Comment String
            deriving (Eq)
instance Show Inst where
  show (I32Const i) = "(i32.const " ++ show i ++ ")"
  show (F32Const f) = "(f32.const " ++ show f ++ ")"
  show (Local t v)    = "(local " ++ v ++ " " ++ printType t ++ ")"
  show (SetLocal v) = "(set_local " ++ v ++ ")"
  show (GetLocal v) = "(get_local " ++ v ++ ")"
  show I32Add = "(i32.add)"
  show I32Sub = "(i32.sub)"
  show I32Mul = "(i32.mul)"
  show I32Div = "(i32.div_s)"
  show I32Less = "(i32.lt_s)"
  show F32Add = "(f32.add)"
  show F32Sub = "(f32.sub)"
  show F32Mul = "(f32.mul)"
  show F32Div = "(f32.div_s)"
  show F32Less = "(f32.lt)"
  show I32Load = "(i32.load)"
  show I32Store = "(i32.store)"
  show F32Load = "(f32.load)"
  show F32Store = "(f32.store)"
  show (IfThenElse t is1 is2 is3) = s1 ++ "\n(if (result " ++ printType t ++ ")\n(i32.eqz)\n(then\n" ++ s3 ++ ")\n" ++ "(else\n" ++ s2 ++ "))"
    where s1 = intercalate "\n" (map show is1)
          s2 = intercalate "\n" (map show is2)
          s3 = intercalate "\n" (map show is3)
  show (CallIndirect (P.TFun tyargs tyres)) = 
    "(call_indirect " ++ concatMap (const "(param i32) ") tyargs ++ "(param i32) " ++ "(result i32))"
    -- Because all values are boxed, their types are i32.
    -- The extra (param i32) is for the function number which corresponds to function pointer.
    -- "(call_indirect " ++ concatMap (\x -> "(param " ++ printType x ++ ") ") tyargs ++ "(param i32) " ++ "(result " ++ printType tyres ++ "))"
  show (Table n) = "(table " ++ show n ++ " anyfunc)"
  show (Func fn (P.TFun _ tres) args is) = "(func $" ++ fn ++ " " ++ sargs ++ "(result i32)\n" ++ intercalate "\n" (map show is) ++ ")"
    where sargs = concatMap f args
          f (C.Var t s) = "(param $" ++ s ++ " i32) "
          f (C.Label t s) = "(param $" ++ s ++ " i32) "
          -- Because all values are boxed, their types are i32.
          -- f (C.Var t s) = "(param $" ++ s ++ " " ++ printType t ++ ") "
          -- f (C.Label t s) = "(param $" ++ s ++ " " ++ printType t ++ ") "
  show (GCMalloc s v) =  intercalate "\n" (map show s) ++  "\n(i32.const " ++ (if v then "1" else "0") ++ ")\n" ++ "(call $gc_malloc)"
  show PrintInt = "(call $print_i32)"
  show PrintFloat = "(call $print_f32)"
  show (Comment s) = "(; " ++ s ++ " ;)"

-- Wasm FunDefs Main
data Wasm = Wasm P.Type [Inst] [Inst]

wasmToString :: Wasm -> String -> IO String
wasmToString (Wasm ty fds is) memoryFile = do
    (memoryGlobalVariables, memoryFunctions) <- L.parseMemoryFile memoryFile
    let prefix = "(module\n(import \"host\" \"print\" (func $print_f32 (param f32)))\n(import \"host\" \"print\" (func $print_i32 (param i32)))\n(memory 10000)\n"
    let table = "(table " ++ show (length fds) ++ " anyfunc)\n"
    let fundefs = intercalate "\n" (map show fds) ++ "\n"
    let elem = "(elem (i32.const 0) " ++ unwords (map (\(Func fn _ _ _) -> "$" ++ fn) fds) ++ ")\n"
    -- Because all values are boxed, their types are i32.
    let mainPrefix = "(func (export \"main\") (result i32)\n"
    let mainLocal = intercalate "\n" (map show (takeWhile isLocal is)) ++ "\n"
    let gcInit = "(call $gc_initalize)\n"
    let mainBody = intercalate "\n" (map show (dropWhile isLocal is))
    let mainSuffix = "))"
    return $ prefix ++ memoryGlobalVariables ++ memoryFunctions ++ table ++ fundefs ++ elem ++ mainPrefix ++ mainLocal ++ gcInit ++ mainBody ++ mainSuffix
    where
      isLocal :: Inst -> Bool
      isLocal (Local _ _) = True
      isLocal _ = False

printType :: P.Type -> String
printType P.TFloat = "f32"
printType P.TInt = "i32"
printType (P.TArray _) = "i32"
printType (P.TTuple _) = "i32"
printType P.TUnit = undefined
printType (P.TFun _ _) = "i32"

prog2Wasm :: C.Prog -> Wasm
prog2Wasm (C.Prog fds e) =
  let (b,l) = runState (exp2Wasm e) GenMState {localVariables = [Local P.TInt stackTopVar], label2index = f} in
  let fdiss = map (fundef2Wasm f) fds in
  Wasm (C.getTypeOfExp e) fdiss (localVariables l ++ b)
    where f x = fromJust . lookup x $ zip (map (\(C.FunDef _ x _ _) -> x) fds) [0..]

fundef2Wasm :: (C.Var -> Int) -> C.FunDef -> Inst
fundef2Wasm f (C.FunDef tyfundef (C.Label tyfname fname) args exp) =
  let (b,l) = runState (exp2Wasm exp) GenMState {localVariables = [Local P.TInt stackTopVar], label2index = f} in
  Func fname tyfundef args (localVariables l ++ b)
    where l2s (C.Var _ v) = v

data GenMState = GenMState { localVariables :: [Inst], label2index :: C.Var -> Int }
type GenM a = (State GenMState a)

putLocal :: P.Type -> String -> GenM ()
putLocal t s = do
  d <- get
  if Local t ("$"++s) `elem` localVariables d
  then return ()
  else put d {localVariables = Local P.TInt ("$"++s):localVariables d}

getLabelIndex :: C.Var -> GenM Int
getLabelIndex l = do
  d <- get
  return (label2index d l)

-- This local variable is mainly used in dup instruction.
stackTopVar = "$stack_top_var"
dupStackTop n = SetLocal stackTopVar : replicate n (GetLocal stackTopVar)

isValue :: P.Type -> Bool
isValue t = case t of
  P.TInt -> True
  P.TFloat -> True
  P.TFun _ _ -> True
  _ -> False

-- Utility function to store a value on the stack top
-- ''ist`` returns a row value like an integer or a float value.
storeRowValue t ist = 
  let i = if t == P.TFloat then F32Store else I32Store in
  [GCMalloc [I32Const 4] (isValue t)] ++ dupStackTop 2 ++ ist ++ [i]

loadValue t = if t == P.TFloat then F32Load else I32Load

exp2Wasm :: C.Exp -> GenM [Inst]
exp2Wasm (C.EIf t e1 e2 e3) = do
  is1 <- liftM2 (++) (exp2Wasm e1) (return [loadValue $ C.getTypeOfExp e1])
  is2 <- exp2Wasm e2
  is3 <- exp2Wasm e3
  return [Comment "if", IfThenElse t is1 is2 is3]
exp2Wasm (C.EInt t i) = return $ storeRowValue t [I32Const i]
exp2Wasm (C.EFloat t f) = return $ storeRowValue t [F32Const f]
exp2Wasm (C.EVar t (C.Var _ v)) = 
  return [GetLocal ("$"++v)]
exp2Wasm (C.EVar _ (C.Label t l)) = do
  i <- getLabelIndex (C.Label t l)
  return [I32Const i]
exp2Wasm (C.EOp t o e1 e2) = do
  is1 <- exp2Wasm e1
  is2 <- exp2Wasm e2
  return $ storeRowValue t (is1 ++ [loadValue (C.getTypeOfExp e1)] ++ is2 ++ [loadValue (C.getTypeOfExp e2)] ++ [op o])
    where op P.OPlus  = I32Add
          op P.OMinus = I32Sub
          op P.OTimes = I32Mul
          op P.OLess  = I32Less
          op P.ODiv   = I32Div
          op P.OFPlus = F32Add
          op P.OFMinus = F32Sub
          op P.OFTimes = F32Mul
          op P.OFLess  = F32Less
          op P.OFDiv   = F32Div
exp2Wasm (C.ELet _ (C.Var t v) e1 e2) = do
  putLocal t v
  is1 <- exp2Wasm e1
  is2 <- exp2Wasm e2
  return $ Comment "let" : is1 ++ [SetLocal ("$"++v)] ++ is2
  where
    isValue :: P.Type -> Bool
    isValue t = case t of
      P.TInt -> True
      P.TFloat -> True
      P.TFun _ _ -> True
      _ -> False
exp2Wasm (C.ETuple _ es) = do
  let reserve = [GCMalloc [I32Const (4 * length es)] False]
  is <- storeTupleBody
  return $ Comment "tuple construction": reserve ++ dupStackTop (1 + length es) ++ is
  where
    storeTupleBody = concat <$> mapM storeTupleBodyElement (zip es [0..])
    storeTupleBodyElement (e,i) = do 
      let storeToTuple is = [I32Const (4*i), I32Add] ++ is ++ [I32Store]
      y <- exp2Wasm e
      return $ storeToTuple y
exp2Wasm (C.EDTuple _ vs e1 e2) = do
  set <- concat <$> mapM f (zip vs (map (*4) [0..]))
  is1 <- exp2Wasm e1
  is2 <- exp2Wasm e2
  return $ Comment "tuple destruction": is1 ++ [SetLocal stackTopVar] ++ set ++ is2
    where
      f :: (C.Var, Int) -> GenM [Inst]
      f (C.Var t v, offset) = do
        putLocal t v
        return [GetLocal stackTopVar, I32Const offset, I32Add, load t, SetLocal ("$"++v)]
      load t = if t == P.TFloat then F32Load else I32Load
exp2Wasm (C.EAppCls rtype e1 args) = do
  isargs <- concat <$> mapM exp2Wasm args
  is1 <- exp2Wasm e1
  return $ isargs ++ is1 ++ is1 ++ [I32Load, CallIndirect (C.getTypeOfExp e1)]
exp2Wasm (C.ESeq _ e1 e2) = do
  is1 <- exp2Wasm e1
  is2 <- exp2Wasm e2
  return (is1 ++ is2)
exp2Wasm (C.EMakeA _ e1) = do
  is1 <- exp2Wasm e1
  -- You must use I32Load because is1 returns a boxed value.
  return [Comment "Array construction", GCMalloc (is1 ++ [I32Load, I32Const 4, I32Mul]) False]
exp2Wasm (C.EGetA t e1 e2) = do
  is1 <- exp2Wasm e1
  is2 <- exp2Wasm e2
  -- Because all values are boxed, we can use I32Load for all values.
  return $ Comment "Array get": is1 ++ is2 ++ [I32Load, I32Const 4, I32Mul, I32Add, I32Load]
exp2Wasm (C.ESetA _ e1 e2 e3) = do
  is1 <- exp2Wasm e1
  is2 <- exp2Wasm e2
  is3 <- exp2Wasm e3
  -- Because all values are boxed, we can use I32Store for all values.
  return $ Comment "Array set": is1 ++ is2 ++ [I32Load, I32Const 4, I32Mul, I32Add] ++ is3 ++ [I32Store]
exp2Wasm (C.EPrintI32 _ e) = do
  is <- exp2Wasm e
  return (is ++ [I32Load, PrintInt])
exp2Wasm (C.EPrintF32 _ e) = do
  is <- exp2Wasm e
  return (is ++ [F32Load, PrintFloat])
