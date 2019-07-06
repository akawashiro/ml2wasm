{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module UnusedVar(removeUnused) where

import qualified Closure   as C
import           Data.List

removeUnused :: C.Prog -> C.Prog
removeUnused (C.Prog fds exp) = C.Prog (map f fds) (removeUnusedExp exp)
  where f (C.FunDef t v vs exp) = C.FunDef t v vs (removeUnusedExp exp)

removeUnusedExp :: C.Exp -> C.Exp
removeUnusedExp (C.EInt t i) = C.EInt t i
removeUnusedExp (C.EFloat t f) = C.EFloat t f
removeUnusedExp (C.EVar t v) = C.EVar t v
removeUnusedExp (C.EOp t o e1 e2) = C.EOp t o (removeUnusedExp e1) (removeUnusedExp e2)
removeUnusedExp (C.EIf t e1 e2 e3) = C.EIf t (removeUnusedExp e1) (removeUnusedExp e2) (removeUnusedExp e3)
removeUnusedExp (C.EAppCls t e1 e2s) = C.EAppCls t (removeUnusedExp e1) (map removeUnusedExp e2s)
removeUnusedExp (C.ETuple t es) = C.ETuple t (map removeUnusedExp es)
removeUnusedExp (C.ERec t x ys e1 e2) = C.ERec t x ys (removeUnusedExp e1) (removeUnusedExp e2)
removeUnusedExp (C.ELet t x e1 e2) =
  if x `elem` fv e2
  then C.ELet t x (removeUnusedExp e1) (removeUnusedExp e2)
  else removeUnusedExp e2
removeUnusedExp (C.EDTuple t xs e1 e2) =
  if intersect xs (fv e2) == []
  then removeUnusedExp e2
  else C.EDTuple t xs (removeUnusedExp e1) (removeUnusedExp e2)
removeUnusedExp (C.ESeq t e1 e2) = C.ESeq t (removeUnusedExp e1) (removeUnusedExp e2)
removeUnusedExp (C.EMakeA t e1) = C.EMakeA t (removeUnusedExp e1)
removeUnusedExp (C.EGetA t e1 e2) = C.EGetA t (removeUnusedExp e1) (removeUnusedExp e2)
removeUnusedExp (C.ESetA t e1 e2 e3) = C.ESetA t (removeUnusedExp e1) (removeUnusedExp e2) (removeUnusedExp e3)
removeUnusedExp (C.EPrintI32 t e) = C.EPrintI32 t (removeUnusedExp e)
removeUnusedExp (C.EPrintF32 t e) = C.EPrintF32 t (removeUnusedExp e)


fv :: C.Exp -> [C.Var]
fv (C.EInt _ _)           = []
fv (C.EFloat _ _)           = []
fv (C.EOp _ _ e1 e2)      = fv e1 ++ fv e2
fv (C.EIf _ e1 e2 e3)     = fv e1 ++ fv e2 ++ fv e3
fv (C.ELet _ v e1 e2)     = (fv e1 ++ fv e2) `lminus` [v]
fv (C.EVar _ v)           = [v]
fv (C.ERec _ x ys e1 e2)  = (fv e1 ++ fv e2) `lminus` (x:ys)
fv (C.EDTuple _ xs e1 e2) = (fv e1 ++ fv e2) `lminus` xs
fv (C.EAppCls _ e1 e2)    = fv e1 ++ concatMap fv e2
fv (C.ETuple _ e)         = concatMap fv e
fv (C.EGetA _ e1 e2)      = fv e1 ++ fv e2
fv (C.EMakeA _ e1)        = fv e1
fv (C.ESetA _ e1 e2 e3)   = fv e1 ++ fv e2 ++ fv e3
fv (C.ESeq _ e1 e2)       = fv e1 ++ fv e2
fv (C.EPrintI32 _ e)      = fv e
fv (C.EPrintF32 _ e)      = fv e


-- Return xs - ys
lminus :: Eq a => [a] -> [a] -> [a]
lminus xs ys = filter (`notElem` ys) xs
