{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Closure where

import           Control.Monad.State
import           Data.List
import qualified Parse               as P
import qualified Type                as T

data Var = Var P.Type String | Label P.Type String deriving (Eq)
instance Show Var where
  show (Var t s)   = s ++ ":" ++ show t
  show (Label t s) = s ++ ":" ++ show t

data Exp = EInt P.Type Int |
           EFloat P.Type Float |
           EOp P.Type P.Op Exp Exp |
           EIf P.Type Exp Exp Exp |
           ELet P.Type Var Exp Exp |
           EDTuple P.Type [Var] Exp Exp |
           EVar P.Type Var |
           ERec P.Type Var [Var] Exp Exp |
           EAppCls P.Type Exp [Exp] |
           ETuple P.Type [Exp] |
           ESeq P.Type Exp Exp |
           EMakeA P.Type Exp |
           EGetA P.Type Exp Exp |
           ESetA P.Type Exp Exp Exp |
           EPrintI32 P.Type Exp |
           EPrintF32 P.Type Exp
           deriving (Eq)
instance Show Exp where
  show (EInt t i) = show i ++ ":" ++ show t
  show (EFloat t f) = show f ++ ":" ++ show t
  show (EIf t e1 e2 e3) = "(if " ++ show e1 ++ "\nthen " ++ show e2 ++ "\nelse " ++ show e3 ++ "):" ++ show t
  show (EOp t o e1 e2) = "(" ++ show o ++ " " ++ show e1 ++ " " ++ show e2 ++ ")" ++ ":" ++ show t
  show (ELet t v e1 e2) = "let " ++ show v ++ " = " ++ show e1 ++ " in\n" ++ show e2 ++ ":" ++ show t
  show (EDTuple t vs e1 e2) = "let (" ++ intercalate ", " (map show vs) ++ ") = " ++ show e1 ++ " in\n" ++ show e2 ++ " : " ++ show t
  show (EVar t v) = show v -- ++ ":" ++ show t
  show (ERec t x ys e1 e2) = "(let rec " ++ show x ++ " " ++ show ys ++ " = " ++ show e1 ++ " in\n" ++ show e2 ++ "):" ++ show t
  show (EAppCls t e1 e2s) = "(" ++ show e1 ++ " " ++ show e2s ++ "):" ++ show t
  show (ETuple t es) = "(" ++ intercalate ", " (map show es) ++ ")" ++ ":" ++ show t
  show (EMakeA t e1) = "(make_array " ++ show e1 ++ "):" ++ show t
  show (EGetA t e1 e2) = "(get_array " ++ show e1 ++ " " ++ show e2 ++ "):" ++ show t
  show (ESetA t e1 e2 e3) = "(set_array " ++ show e1 ++ " " ++ show e2  ++ " " ++ show e3 ++ "):" ++ show t
  show (ESeq t e1 e2) = show e1 ++ ";\n" ++ show e2 ++ "):" ++ show t
  show (EPrintI32 t e1) = "(print_i32 " ++ show e1 ++ "):" ++ show t
  show (EPrintF32 t e1) = "(print_f32 " ++ show e1 ++ "):" ++ show t



data FunDef = FunDef P.Type Var [Var] Exp deriving (Eq)
instance Show FunDef where
  show (FunDef t x ys e) = "fundef " ++ show x ++ " " ++ unwords (map show ys) ++ " =\n" ++ show e

data Prog = Prog [FunDef] Exp deriving (Eq)
instance Show Prog where
  show (Prog fd exp) = intercalate "\n" (map show fd) ++ "\n" ++ show exp

type ClsTransM = State [FunDef]

addFunDef :: FunDef -> ClsTransM ()
addFunDef fd = do
  fs <- get
  put (fd:fs)

clsTrans :: P.Exp -> Prog
clsTrans exp =
  let (e,fd) = runState (clsTrans' exp) [] in
  Prog fd e

clsTrans' :: P.Exp -> ClsTransM Exp
clsTrans' (P.EFloat t f) = return (EFloat t f)
clsTrans' (P.EInt t i) = return (EInt t i)
clsTrans' (P.EBool t b) = if b then return (EInt P.TInt 1) else return (EInt P.TInt 0)
clsTrans' (P.EPrintI32 t e) = EPrintI32 t <$> clsTrans' e
clsTrans' (P.EOp t o e1 e2) = EOp t o <$> clsTrans' e1 <*> clsTrans' e2
clsTrans' (P.EIf t e1 e2 e3) = EIf t <$> clsTrans' e1 <*> clsTrans' e2 <*> clsTrans' e3
clsTrans' (P.ELet t (P.Var t2 x) e1 e2) = ELet t (Var t2 x) <$> clsTrans' e1 <*> clsTrans' e2
clsTrans' (P.EDTuple t xs e1 e2) = EDTuple t (map v2v xs) <$> clsTrans' e1 <*> clsTrans' e2
clsTrans' (P.EVar t (P.Var t2 s)) = return (EVar t (Var t2 s))
clsTrans' (P.EApp t e1 e2) = EAppCls t <$> clsTrans' e1 <*> mapM clsTrans' e2
clsTrans' (P.EGetA t e1 e2) = EGetA t <$> clsTrans' e1 <*> clsTrans' e2
clsTrans' (P.ESetA t e1 e2 e3) = ESetA t <$> clsTrans' e1 <*> clsTrans' e2 <*> clsTrans' e3
clsTrans' (P.EMakeA t e1) = EMakeA t <$> clsTrans' e1
clsTrans' (P.ESeq t e1 e2) = ESeq t <$> clsTrans' e1 <*> clsTrans' e2
clsTrans' (P.ETuple t es) = ETuple t <$> mapM clsTrans' es
clsTrans' (P.ERec te2 x@(P.Var (P.TFun targ tres) xstr) ys e1 e2) = do
  e1' <- clsTrans' e1
  e2' <- clsTrans' e2
  let fvs = fv e1 `lminus` (x:ys)
  let clsType = P.TTuple (P.TInt:map T.getTypeOfVar fvs)
  let fundefType = P.TFun (targ ++ [clsType]) tres
  let fd = FunDef (P.TFun (targ ++ [clsType]) tres) (v2l x fundefType) (map v2v ys ++ [v2cls x clsType])
           (EDTuple (T.getTypeOfExp e1) (v2tmp x:map v2v fvs) (EVar clsType (v2cls x clsType))
           (ELet (T.getTypeOfExp e1) (v2v x) (ETuple clsType (map v2ev (v2l x fundefType:map v2v fvs))) e1'))
  addFunDef fd
  return $ ELet (T.getTypeOfExp e2) (v2v x) (ETuple clsType (map v2ev (v2l x fundefType:map v2v fvs))) e2'

v2ev :: Var -> Exp
v2ev (Var t s) = EVar t (Var t s)
v2ev (Label t s) = EVar t (Label t s)

v2v :: P.Var -> Var
v2v (P.Var t s) = Var t s

v2cls :: P.Var -> P.Type -> Var
v2cls (P.Var _ s) t = Var t ("cls_" ++ s)

v2tmp :: P.Var -> Var
v2tmp (P.Var t s) = Var t ("tmp_" ++ s)

v2l :: P.Var -> P.Type -> Var
v2l (P.Var _ s) t = Label t ("def_" ++ s)

fv :: P.Exp -> [P.Var]
fv (P.EInt _ _)           = []
fv (P.EFloat _ _)           = []
fv (P.EBool _ _)          = []
fv (P.EOp _ _ e1 e2)      = fv e1 ++ fv e2
fv (P.EIf _ e1 e2 e3)     = fv e1 ++ fv e2 ++ fv e3
fv (P.ELet _ v e1 e2)     = (fv e1 ++ fv e2) `lminus` [v]
fv (P.EVar _ v)           = [v]
fv (P.ERec _ x ys e1 e2)  = (fv e1 ++ fv e2) `lminus` (x:ys)
fv (P.EDTuple _ xs e1 e2) = (fv e1 ++ fv e2) `lminus` xs
fv (P.EApp _ e1 e2)       = fv e1 ++ concatMap fv e2
fv (P.ETuple _ e)         = concatMap fv e
fv (P.EGetA _ e1 e2)      = fv e1 ++ fv e2
fv (P.EMakeA _ e1)        = fv e1
fv (P.ESetA _ e1 e2 e3)   = fv e1 ++ fv e2 ++ fv e3
fv (P.ESeq _ e1 e2)       = fv e1 ++ fv e2
fv (P.EPrintI32 _ e)      = fv e
fv (P.EPrintF32 _ e)      = fv e

-- Return xs - ys
lminus :: Eq a => [a] -> [a] -> [a]
lminus xs ys = filter (`notElem` ys) xs
