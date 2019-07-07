{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module WasmGen (prog2Wasm, wasmToString) where

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
            GCIncreaseRC |
            GCDecreaseRC |
            GCFree |
            PrintInt |
            PrintFloat |
            Drop |
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
  show F32Div = "(f32.div)"
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
  show GCIncreaseRC = "(call $gc_increase_rc)"
  show GCDecreaseRC = "(call $gc_decrease_rc)"
  show GCFree = "(call $gc_free)"
  show Drop = "(drop)"
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
    let measureMalloc = "(call $malloc_measure)\n"
    let mainSuffix = "))"
    return $ prefix ++ memoryGlobalVariables ++ memoryFunctions ++ table ++ fundefs ++ elem ++ mainPrefix ++ mainLocal ++ gcInit ++ mainBody ++ measureMalloc ++ mainSuffix
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
  let (b,l) = runState (exp2Insts e) GenMState {localVariables = [Local P.TInt stackTopVar], label2index = f} in
  let fdiss = map (fundef2Wasm f) fds in
  Wasm (C.getTypeOfExp e) fdiss (localVariables l ++ b)
    where f x = fromJust . lookup x $ zip (map (\(C.FunDef _ x _ _) -> x) fds) [0..]

-- ``inc'' takes the body of the function and append GCIncreaseRC when the
-- function returns non TUnit value. We increase the RC of the return value 
-- so that it is not collected by GC.
fundef2Wasm :: (C.Var -> Int) -> C.FunDef -> Inst
fundef2Wasm f (C.FunDef tyfundef (C.Label tyfname fname) args exp) =
  Func fname tyfundef args $ localVariables vars ++ incArgs ++ body ++ incRes ++ decLocal ++ decArgs
  where
      (body,vars) = runState (exp2Insts exp) GenMState {localVariables = [Local P.TInt stackTopVar], label2index = f}
      incArgs = concatMap (\a->gl a : [GCIncreaseRC, Drop]) args
      decArgs = concatMap (\a->gl a : [GCDecreaseRC, Drop]) args
      gl (C.Var t s) = GetLocal $ "$" ++ s
      gl (C.Label t s) = GetLocal $ "$" ++ s
      -- If the function returns a value, we must increase RC.
      incRes = case tyfundef of
                P.TUnit -> []
                _ -> [GCIncreaseRC]
      -- Decrease all RCs of local variables except stackTopVar.
      decLocal = concatMap (\(Local _ v) -> [GetLocal v, GCDecreaseRC, Drop]) $ localVariables vars \\ [Local P.TInt stackTopVar]

data GenMState = GenMState { localVariables :: [Inst], label2index :: C.Var -> Int }
type GenM a = (State GenMState a)

putLocal :: P.Type -> String -> GenM ()
putLocal t s = do
  d <- get
  if Local P.TInt ("$"++s) `elem` localVariables d
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

exp2Insts :: C.Exp -> GenM [Inst]
exp2Insts (C.EIf t e1 e2 e3) = do
  is1 <- liftM2 (++) (exp2Insts e1) (return [loadValue $ C.getTypeOfExp e1])
  is2 <- exp2Insts e2
  is3 <- exp2Insts e3
  -- All values are boxed therefore return type become P.TInt.
  return [Comment "if", IfThenElse rt is1 is2 is3]
  where
    rt = if t == P.TUnit then P.TUnit else P.TInt
exp2Insts (C.EInt t i) = return $ storeRowValue t [I32Const i]
exp2Insts (C.EFloat t f) = return $ storeRowValue t [F32Const f]
exp2Insts (C.EVar t (C.Var _ v)) = 
  return [GetLocal ("$"++v)]
exp2Insts (C.EVar _ (C.Label t l)) = do
  i <- getLabelIndex (C.Label t l)
  return $ storeRowValue t [I32Const i]
-- If e1 or e2 is an immediate value, we do not need to store it to the heap.
exp2Insts (C.EOp t o e1 e2) = do
  is1 <- loade1
  is2 <- loade2
  return $ Comment "arith op" : storeRowValue tr (is1 ++ is2 ++ [op o]) ++ [Comment "arith op end"]
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
          t1 = if o == P.OPlus || o == P.OMinus || o == P.OTimes || o == P.OLess || o == P.ODiv 
                   then P.TInt
                   else P.TFloat
          t2 = t1
          tr = if o == P.OFPlus || o == P.OFMinus || o == P.OFTimes || o == P.OFDiv 
                   then P.TFloat
                   else P.TInt
          loade1 = case e1 of
                     (C.EInt _ i) -> return [I32Const i]
                     (C.EFloat _ f) -> return [F32Const f]
                     _ -> exp2Insts e1 >>= (\x->return (x++[loadValue t1]))
          loade2 = case e2 of
                     (C.EInt _ i) -> return [I32Const i]
                     (C.EFloat _ f) -> return [F32Const f]
                     _ -> exp2Insts e2 >>= (\x->return (x++[loadValue t2]))
exp2Insts (C.ELet _ (C.Var t v) e1 e2) = do
  putLocal t v
  is1 <- exp2Insts e1
  is2 <- exp2Insts e2
  return $ Comment ("let " ++ v) : is1 ++ [GCIncreaseRC, SetLocal ("$"++v)] ++ [Comment ("let " ++ v ++ " sub")] ++ is2 ++ [Comment ("let " ++ v ++ " end")]
exp2Insts (C.ETuple _ es) = do
  let reserve = [GCMalloc [I32Const (4 * length es)] False]
  -- let reserve = [GCMalloc [I32Const (4 * length es)] True]
  is <- storeTupleBody
  return $ Comment "tuple construction": reserve ++ dupStackTop (1 + length es) ++ is ++ [Comment "tuple construction end"]
  where
    storeTupleBody = concat <$> mapM storeTupleBodyElement (zip es [0..])
    storeTupleBodyElement (e,i) = do 
      let storeToTuple is = [I32Const (4*i), I32Add] ++ is ++ [GCIncreaseRC, I32Store]
      y <- exp2Insts e
      return $ storeToTuple y
exp2Insts (C.EDTuple _ vs e1 e2) = do
  is1 <- exp2Insts e1
  set <- concat <$> mapM f (zip vs (map (*4) [0..]))
  is2 <- exp2Insts e2
  return $ Comment "tuple destruction": is1 ++ [SetLocal stackTopVar] ++ set ++ is2 ++ [Comment "tuple destruction end"]
    where
      f :: (C.Var, Int) -> GenM [Inst]
      f (C.Var t v, offset) = do
        putLocal t v
        return [GetLocal stackTopVar, I32Const offset, I32Add, I32Load, SetLocal ("$"++v)]
exp2Insts (C.EAppCls rtype e1 args) = do
  isargs <- concat <$> mapM exp2Insts args
  is1 <- exp2Insts e1
  -- We must get the function number from the closure therefore we need two
  -- I32Load.
  return $ Comment "function call" : isargs ++ is1 ++ is1 ++ [I32Load, I32Load, CallIndirect (C.getTypeOfExp e1)] ++ [Comment "function call end"]
exp2Insts (C.ESeq _ e1 e2) = do
  is1 <- exp2Insts e1
  is2 <- exp2Insts e2
  return (is1 ++ is2)
exp2Insts (C.EMakeA _ e1) = do
  is1 <- exp2Insts e1
  -- You must use I32Load because is1 returns a boxed value.
  return [Comment "Array construction", GCMalloc (is1 ++ [I32Load, I32Const 4, I32Mul]) False]
exp2Insts (C.EGetA t e1 e2) = do
  is1 <- exp2Insts e1
  is2 <- exp2Insts e2
  -- Because all values are boxed, we can use I32Load for all values.
  return $ Comment "Array get": is1 ++ is2 ++ [I32Load, I32Const 4, I32Mul, I32Add, I32Load]
exp2Insts (C.ESetA _ array index value) = do
  isAr <- exp2Insts array
  isIn <- exp2Insts index
  isVal <- exp2Insts value
  -- Because all values are boxed, we can use I32Store for all values.
  -- We need to decrease the RC of the old value.
  return $ Comment "Array set": isAr ++ isIn ++ [I32Load, I32Const 4, I32Mul, I32Add] ++ decOld ++ isVal ++ [GCIncreaseRC, I32Store]
  where
      -- decOld = dupStackTop 2 ++ [I32Load, GCDecreaseRC, Drop]
      decOld = []
exp2Insts (C.EPrintI32 _ e) = do
  is <- exp2Insts e
  return (is ++ [I32Load, PrintInt])
exp2Insts (C.EPrintF32 _ e) = do
  is <- exp2Insts e
  return (is ++ [F32Load, PrintFloat])
