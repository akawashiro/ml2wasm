{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module UnusedVar(removeUnused) where

import qualified Closure as C
import Data.List

removeUnused :: C.Prog -> C.Prog
removeUnused (C.Prog fds exp) = C.Prog (map f fds) (removeUnusedExp exp)
  where f (C.FunDef v vs exp) = C.FunDef v vs (removeUnusedExp exp)

removeUnusedExp :: C.Exp -> C.Exp
removeUnusedExp (C.EInt i) = C.EInt i
removeUnusedExp (C.EVar v) = C.EVar v
removeUnusedExp (C.EOp o e1 e2) = C.EOp o (removeUnusedExp e1) (removeUnusedExp e2)
removeUnusedExp (C.EIf e1 e2 e3) = C.EIf (removeUnusedExp e1) (removeUnusedExp e2) (removeUnusedExp e3)
removeUnusedExp (C.EAppCls e1 e2s) = C.EAppCls (removeUnusedExp e1) (map removeUnusedExp e2s)
removeUnusedExp (C.ETuple es) = C.ETuple (map removeUnusedExp es)
removeUnusedExp (C.ERec x ys e1 e2) = C.ERec x ys (removeUnusedExp e1) (removeUnusedExp e2)
removeUnusedExp (C.ELet x e1 e2) = 
  if x `elem` (fv e2) 
  then C.ELet x (removeUnusedExp e1) (removeUnusedExp e2)
  else removeUnusedExp e2
removeUnusedExp (C.EDTuple xs e1 e2) =
  if intersect xs (fv e2) == []
  then removeUnusedExp e2
  else C.EDTuple xs (removeUnusedExp e1) (removeUnusedExp e2)
removeUnusedExp (C.ESeq e1 e2) = C.ESeq (removeUnusedExp e1) (removeUnusedExp e2)
removeUnusedExp (C.EMakeA e1 e2) = C.EMakeA (removeUnusedExp e1) (removeUnusedExp e2)
removeUnusedExp (C.EGetA e1 e2) = C.EGetA (removeUnusedExp e1) (removeUnusedExp e2)
removeUnusedExp (C.ESetA e1 e2) = C.ESetA (removeUnusedExp e1) (removeUnusedExp e2)


fv :: C.Exp -> [C.Var]
fv (C.EInt _) = []
fv (C.EOp _ e1 e2) = fv e1 ++ fv e2
fv (C.EIf e1 e2 e3) = fv e1 ++ fv e2 ++ fv e3
fv (C.ELet v e1 e2) = (fv e1 ++ fv e2) `lminus` [v]
fv (C.EVar v) = [v]
fv (C.ERec x ys e1 e2) = (fv e1 ++ fv e2) `lminus` (x:ys)
fv (C.EDTuple xs e1 e2) = (fv e1 ++ fv e2) `lminus` xs
fv (C.EAppCls e1 e2) = fv e1 ++ concatMap fv e2
fv (C.ETuple e) = concatMap fv e
fv (C.EGetA e1 e2) = fv e1 ++ fv e2
fv (C.EMakeA e1 e2) = fv e1 ++ fv e2
fv (C.ESetA e1 e2) = fv e1 ++ fv e2
fv (C.ESeq e1 e2) = fv e1 ++ fv e2


-- Return xs - ys
lminus :: Eq a => [a] -> [a] -> [a]
lminus xs ys = filter (`notElem` ys) xs
