{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Closure where

import           Control.Monad.State
import           Data.List
import qualified Parse               as P

data Var = Var String | Label String deriving (Eq)
instance Show Var where
  show (Var s)   = s
  show (Label s) = s

data Exp = EInt Int |
           EOp P.Op Exp Exp |
           EIf Exp Exp Exp |
           ELet Var Exp Exp |
           EDTuple [Var] Exp Exp |
           EVar Var |
           ERec Var [Var] Exp Exp |
           EAppCls Exp [Exp] |
           ETuple [Exp] |
           ESeq Exp Exp |
           EMakeA Exp |
           EGetA Exp Exp |
           ESetA Exp Exp Exp
           deriving (Eq)
instance Show Exp where
  show (EInt i) = show i
  show (EOp o e1 e2) = "(" ++ show o ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (EIf e1 e2 e3) = "if " ++ show e1 ++ "\nthen " ++ show e2 ++ "\nelse " ++ show e3
  show (ELet v e1 e2) = "let " ++ show v ++ " = " ++ show e1 ++ " in\n" ++ show e2
  show (EDTuple vs e1 e2) = "let (" ++ intercalate ", " (map show vs) ++ ") = " ++ show e1 ++ " in\n" ++ show e2
  show (EVar v) = show v
  show (ERec x ys e1 e2) = "let rec " ++ show x ++ " " ++ show ys ++ " =\n" ++ show e1 ++ " in\n" ++ show e2
  show (EAppCls e1 e2s) = show e1 ++ " " ++ show e2s
  show (ETuple es) = "(" ++ intercalate ", " (map show es) ++ ")"
  show (EMakeA e1) = "make_array " ++ show e1
  show (EGetA e1 e2) = "get_array " ++ show e1 ++ " " ++ show e2
  show (ESetA e1 e2 e3) = "set_array " ++ show e1 ++ " " ++ show e2 ++ " " ++ show e3
  show (ESeq e1 e2) = show e1 ++ "; " ++ show e2

data FunDef = FunDef Var [Var] Exp deriving (Eq)
instance Show FunDef where
  show (FunDef x ys e) = "fundef " ++ show x ++ " " ++ unwords (map show ys) ++ " =\n" ++ show e

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
clsTrans' (P.EInt i) = return (EInt i)
clsTrans' (P.EBool b) = if b then return (EInt 1) else return (EInt 0)
clsTrans' (P.EOp o e1 e2) = EOp o <$> clsTrans' e1 <*> clsTrans' e2
clsTrans' (P.EIf e1 e2 e3) = EIf <$> clsTrans' e1 <*> clsTrans' e2 <*> clsTrans' e3
clsTrans' (P.ELet (P.Var x) e1 e2) = ELet (Var x) <$> clsTrans' e1 <*> clsTrans' e2
clsTrans' (P.EDTuple xs e1 e2) = EDTuple (map v2v xs) <$> clsTrans' e1 <*> clsTrans' e2
clsTrans' (P.EVar (P.Var s)) = return (EVar (Var s))
clsTrans' (P.EApp e1 e2) = EAppCls <$> clsTrans' e1 <*> mapM clsTrans' e2
clsTrans' (P.EGetA e1 e2) = EGetA <$> clsTrans' e1 <*> clsTrans' e2
clsTrans' (P.ESetA e1 e2 e3) = ESetA <$> clsTrans' e1 <*> clsTrans' e2 <*> clsTrans' e3
clsTrans' (P.EMakeA e1) = EMakeA <$> clsTrans' e1
clsTrans' (P.ESeq e1 e2) = ESeq <$> clsTrans' e1 <*> clsTrans' e2
clsTrans' (P.ETuple es) = ETuple <$> mapM clsTrans' es
clsTrans' (P.ERec x ys e1 e2) = do
  e1' <- clsTrans' e1
  e2' <- clsTrans' e2
  let fvs = fv e1 `lminus` (x:ys)
  let fd = FunDef (v2l x) (map v2v ys ++ [v2cls x])
           (EDTuple (v2tmp x:map v2v fvs) (EVar (v2cls x))
           (ELet (v2v x) (ETuple (map EVar (v2l x:map v2v fvs))) e1'))
  addFunDef fd
  return $ ELet (v2v x) (ETuple (map EVar (v2l x:map v2v fvs))) e2'


v2v :: P.Var -> Var
v2v (P.Var s) = Var s

v2cls :: P.Var -> Var
v2cls (P.Var s) = Var ("cls_" ++ s)

v2tmp :: P.Var -> Var
v2tmp (P.Var s) = Var ("tmp_" ++ s)

v2l :: P.Var -> Var
v2l (P.Var s) = Label ("def_" ++ s)

adder :: P.Exp
adder = P.ERec (P.Var "make_adder") [P.Var "x"]
          (P.ERec (P.Var "adder") [P.Var "y"]
            (P.EOp P.OPlus (P.EVar (P.Var "x")) (P.EVar (P.Var "y"))) (P.EApp (P.EVar (P.Var "adder")) [P.EInt 3]))
          (P.EApp (P.EVar (P.Var "make_adder")) [P.EInt 7])

adder2 :: P.Exp
adder2 = P.ERec
          (P.Var "make_adder") [P.Var "x", P.Var "y"]
          (P.EOp P.OPlus (P.EVar (P.Var "x")) (P.EVar (P.Var "y")))
          (P.EApp (P.EVar (P.Var "make_adder")) [P.EInt 7, P.EInt 10])

fv :: P.Exp -> [P.Var]
fv (P.EInt _)           = []
fv (P.EBool _)          = []
fv (P.EOp _ e1 e2)      = fv e1 ++ fv e2
fv (P.EIf e1 e2 e3)     = fv e1 ++ fv e2 ++ fv e3
fv (P.ELet v e1 e2)     = (fv e1 ++ fv e2) `lminus` [v]
fv (P.EVar v)           = [v]
fv (P.ERec x ys e1 e2)  = (fv e1 ++ fv e2) `lminus` (x:ys)
fv (P.EDTuple xs e1 e2) = (fv e1 ++ fv e2) `lminus` xs
fv (P.EApp e1 e2)       = fv e1 ++ concatMap fv e2
fv (P.ETuple e)         = concatMap fv e
fv (P.EGetA e1 e2)      = fv e1 ++ fv e2
fv (P.EMakeA e1)        = fv e1
fv (P.ESetA e1 e2 e3)   = fv e1 ++ fv e2 ++ fv e3
fv (P.ESeq e1 e2)       = fv e1 ++ fv e2

-- Return xs - ys
lminus :: Eq a => [a] -> [a] -> [a]
lminus xs ys = filter (`notElem` ys) xs
