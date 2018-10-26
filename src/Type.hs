{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Type (typingExp, getTypeOfVar, getTypeOfExp) where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.List(nub)
import Data.Maybe(fromMaybe)

import Parse
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.List(nub)
import Data.Maybe(fromMaybe)

typingExp :: Exp -> Either String Exp
typingExp e = do
  s <- unify (ms e)
  return $ ae s e

type Restriction = (Type, Type)
type Substituition = [Restriction]
-- First element of each element in TypeSchemeEnvironment is EVariable
type MakeSubstituition = Either String

-- Apply substitution to an exp
ae :: Substituition -> Exp -> Exp
ae s (EInt t i) = (EInt TInt i)
ae s (EFloat t f) = (EFloat TFloat f)
ae s (EBool t b) = (EBool TInt b)
ae s (EOp t o e2 e3) = (EOp (at s t) o (ae s e2) (ae s e3))
ae s (EIf t e1 e2 e3) = (EIf (at s t) (ae s e1) (ae s e2) (ae s e3))
ae s (ELet t v e2 e3) = (ELet (at s t) (av s v) (ae s e2) (ae s e3)) 
ae s (EDTuple t vs e1 e2) = (EDTuple (at s t) (map (av s) vs) (ae s e1) (ae s e2))
ae s (ERec t x vs e1 e2) = (ERec (at s t) (av s x) (map (av s) vs) (ae s e1) (ae s e2))
ae s (EVar t v) = (EVar (at s t) (av s v))
ae s (EApp t e1 e2) = (EApp (at s t) (ae s e1) (map (ae s) e2))
ae s (ETuple t es) = (ETuple (at s t) (map (ae s) es))
ae s (ESeq t e1 e2) = ESeq (at s t) (ae s e1) (ae s e2)
ae s (EMakeA t e) = EMakeA (at s t) (ae s e)
ae s (EGetA t e1 e2) = EGetA (at s t) (ae s e1) (ae s e2)
ae s (ESetA t e1 e2 e3) = ESetA (at s t) (ae s e1) (ae s e2) (ae s e3)
ae s (EPrintI32 t e) = EPrintI32 (at s t) (ae s e)
ae s (EPrintF32 t e) = EPrintF32 (at s t) (ae s e)

-- Apply substitution to an type
at :: Substituition -> Type -> Type
at s (TVar v) = maybe (TVar v) id (lookup (TVar v) s)
at s (TFun ts t) = TFun (map (at s) ts) (at s t)
at s (TTuple ts) = TTuple (map (at s) ts)
at s (TArray t)  = TArray (at s t)
at s t = t

-- Apply substitution to an variable
av sub (Var t s) = (Var (at sub t) s)

getTypeOfExp = gt

-- Get Type of Exp
gt :: Exp -> Type
gt (EInt _ _) = TInt
gt (EFloat _ _) = TFloat
gt (EBool _ _) = TInt
gt (EOp t _ _ _) = t
gt (EIf t _ _ _) = t
gt (ELet t _ _ _) = t
gt (EDTuple t _ _ _) = t
gt (ERec t _ _ _ _) = t
gt (EVar t _) = t
gt (EApp t _ _) = t
gt (ETuple t _) = t
gt (ESeq t _ _) = t
gt (EMakeA t _) = t
gt (EGetA t _ _) = t
gt (ESetA t _ _ _) = t
gt (EPrintI32 t _) = t
gt (EPrintF32 t _) = t

getTypeOfVar = gt'

gt' (Var t _) = t

ms :: Exp -> Substituition
ms (EInt _ _) = []
ms (EFloat _ _) = []
ms (EBool _ _) = []
ms (EOp t op e1 e2) = [(t,rt),(et,gt e1),(et,gt e2)] ++ ms e1 ++ ms e2
  where (rt,et) = case op of
          OLess -> (TInt, TInt)
          OPlus -> (TInt, TInt)
          OMinus -> (TInt, TInt)
          OTimes -> (TInt, TInt)
          ODiv -> (TInt, TInt)
          OFLess -> (TInt, TFloat)
          OFPlus -> (TFloat, TFloat)
          OFMinus -> (TFloat, TFloat)
          OFTimes -> (TFloat, TFloat)
          OFDiv -> (TFloat, TFloat)
ms (EIf t e1 e2 e3) = [(t,gt e2),(t,gt e3),(TInt,gt e1)] ++ ms e1 ++ ms e2 ++ ms e3
ms (ELet t v e1 e2) = [(t,gt e2),(gt' v,gt e1)] ++ ms e1 ++ ms e2
ms (EDTuple t vs e1 e2) = [(TTuple (map gt' vs),gt e1),(t,gt e2)] ++ ms e1 ++ ms e2
ms (EVar t v) = [(t,gt' v)]
ms (ERec t v as e1 e2) = [(t,gt e2), (gt' v,TFun (map gt' as) (gt e1))] ++ ms e1 ++ ms e2
ms (EApp t e1 e2s) = [(gt e1,TFun (map gt e2s) t)] ++ ms e1 ++ concatMap ms e2s
ms (ETuple t es) = (t, TTuple (map gt es)) : concatMap ms es
ms (ESeq t e1 e2) = [(TUnit,gt e1), (t, gt e2)] ++ ms e1 ++ ms e2
ms (EMakeA t e) = (TInt, gt e) : ms e
ms (EGetA t e1 e2) = [(TArray t, gt e1), (TInt, gt e2)] ++ ms e1 ++ ms e2
ms (ESetA t e1 e2 e3) = [(TUnit, t),(gt e1, TArray (gt e3)), (gt e2,TInt)] ++ ms e1 ++ ms e2
ms (EPrintI32 t e1) = [(TUnit,t),(gt e1,TInt)]
ms (EPrintF32 t e1) = [(TUnit,t),(gt e1,TFloat)]

freeTVarIndex :: Type -> [TVarIndex]
freeTVarIndex = nub . freeTVarIndex'
freeTVarIndex' (TVar i) = [i]
freeTVarIndex' (TFun t1 t2) = concatMap freeTVarIndex' t1 ++ freeTVarIndex' t2
freeTVarIndex' (TTuple ts) = concatMap freeTVarIndex' ts
freeTVarIndex' (TArray t) = freeTVarIndex' t
freeTVarIndex' _ = []

--
-- replace all t1 in t3 with t2
replace :: Type -> Type -> Type -> Type
replace t1 t2 t3 = if t1 == t3
  then t2
  else case t3 of
    TFun t4s t5 -> TFun (map (replace t1 t2) t4s) (replace t1 t2 t5)
    TTuple ts -> TTuple (map (replace t1 t2) ts)
    TArray t -> TArray (replace t1 t2 t)
    otherwise -> t3

--
-- replace all t1 in s with t2
replaceSubstituition :: Type -> Type -> Substituition -> Substituition
replaceSubstituition t1 t2 s = map (\(x,y) -> ((replace t1 t2 x), (replace t1 t2 y))) s

unify :: Substituition -> Either String Substituition
unify s = unify' s []
unify' [] b = return b
unify' ((t1, t2) : r) b
  | t1 == t2 = unify' r b
  | otherwise = case (t1,t2) of
    (TFun t3 t4,TFun t5 t6) -> unify' ((t4,t6):zip t3 t5 ++ r) b
    (TTuple t3s,TTuple t4s) -> unify' (zip t3s t4s ++ r) b
    (TArray t3, TArray t4) -> unify' ((t3,t4):r) b
    (TVar tv1,t4) -> if tv1 `elem` freeTVarIndex t4
      then fail ""
      else do 
        u <- unify' (replaceSubstituition (TVar tv1) t4 (r)) ((TVar tv1,t4) : replaceSubstituition (TVar tv1) t4 b)
        return u
    (t4, TVar t3) -> unify' ((TVar t3,t4) : r) b
    otherwise -> Left $ "Cannot unify " ++ show t1 ++ " and " ++ show t2
