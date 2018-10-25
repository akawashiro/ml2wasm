module Type where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.List(nub)
import Data.Maybe(fromMaybe)

import qualified Parse as P
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.List(nub)
import Data.Maybe(fromMaybe)

data Type = TInt | TBool | TFloat | TFun Type Type | TTuple [Type] | TArray Type | TUnit | TVar TVarIndex deriving (Eq, Show)
-- First argument of TypeScheme are bounded variable.
data TypeScheme = TypeScheme [TVarIndex] Type

type TVarIndex = Int
type Restriction = (Type, Type)
type Substituition = [Restriction]
-- First element of each element in TypeSchemeEnvironment is EVariable
type TypeSchemeEnvironment = [(Expr, TypeScheme)]
type MakeSubstituition = MaybeT (State TVarIndex)

lookupTypeSchemeEnv :: Expr -> TypeSchemeEnvironment -> MakeSubstituition TypeScheme
lookupTypeSchemeEnv e [] = do
  i <- getNewTVarIndex
  return $ TypeScheme [] (TVar i)
lookupTypeSchemeEnv e1 ((e2,t):r)
  | e1 == e2 = return t
  | otherwise = lookupTypeSchemeEnv e1 r

getNewTVarIndex :: MakeSubstituition TVarIndex
getNewTVarIndex = do
  i <-get
  put (i+1)
  return i

freeTVarIndex :: Type -> [TVarIndex]
freeTVarIndex = nub . freeTVarIndex'
freeTVarIndex' (TVar i) = [i]
freeTVarIndex' (TFun t1 t2) = freeTVarIndex' t1 ++ freeTVarIndex' t2
freeTVarIndex' _ = []

-- replace all t1 in t3 with t2
replace :: Type -> Type -> Type -> Type
replace t1 t2 t3 = if t1 == t3
  then t2
  else case t3 of
    TFun t4 t5 -> TFun (replace t1 t2 t4) (replace t1 t2 t5)
    otherwise -> t3

-- replace all t1 in s with t2
replaceSubstituition :: Type -> Type -> Substituition -> Substituition
replaceSubstituition t1 t2 s = map (\(x,y) -> ((replace t1 t2 x), (replace t1 t2 y))) s

applySub :: Substituition -> Type -> Type
applySub s (TVar i) = fromMaybe (TVar i) (lookup (TVar i) s)
applySub s (TFun t1 t2) = TFun (applySub s t1) (applySub s t2)
applySub _ t = t

applySubEnv :: Substituition -> TypeSchemeEnvironment -> TypeSchemeEnvironment
applySubEnv s [] = []
applySubEnv s ((e,TypeScheme tvs ty) : r) = (e,TypeScheme (freeTVarIndex ty') ty') : r'
  where
    ty' = applySub s ty
    r' = applySubEnv s r

unify :: Substituition -> MakeSubstituition Substituition
unify s = unify' s []
unify' [] b = return b
unify' ((t1, t2) : r) b
  | t1 == t2 = unify' r b
  | otherwise = case (t1,t2) of
    ((TFun t3 t4),(TFun t5 t6)) -> unify' ((t3,t5):(t4,t6):r) b
    (TVar tv1,t4) -> if tv1 `elem` freeTVarIndex t4
      then fail ""
      else do 
        u <- unify' (replaceSubstituition (TVar tv1) t4 (r)) ((TVar tv1,t4) : replaceSubstituition (TVar tv1) t4 b)
        return u
    (t4, TVar t3) -> unify' ((TVar t3,t4) : r) b
    otherwise -> fail ""

exprToSubstituition :: Expr -> Maybe (Substituition, Type)
exprToSubstituition e = evalState (runMaybeT $ exprToSubstituition' [] (TVar 0) e) 1

-- First argument is bounder to given expr
exprToSubstituition' :: TypeSchemeEnvironment -> Type -> Expr -> MakeSubstituition (Substituition, Type)
exprToSubstituition' env t e = case e of
  EInt _ -> return ([(t, TInt)], TInt)
  EBool _ -> return ([(t, TBool)], TBool)
  EBinOp op e1 e2 -> do
    (sub1, t1) <- exprToSubstituition' env TInt e1
    (sub2, t2) <- exprToSubstituition' env TInt e2
    sub3 <- unify $ (rt, t) : (t1, TInt) : (t2, TInt) : sub1 ++ sub2
    return $ (sub3, rt)
      where rt = if op == Lt then TBool else TInt
  EIf e1 e2 e3 -> do
    (sub1, t1) <- exprToSubstituition' env TBool e1
    (sub2, t2) <- exprToSubstituition' env t e2
    (sub3, t3) <- exprToSubstituition' env t e3
    sub4 <- unify $ (t1, TBool) : (t2, t3) : sub1 ++ sub2 ++ sub3
    return $ (sub4, applySub sub4 t2)
  ELet s1 e1 e2 -> do
    tv1 <- getNewTVarIndex
    (sub1, t1) <- exprToSubstituition' env (TVar tv1) e1
    let env1 = applySubEnv sub1 $ (EVariable s1, TypeScheme (freeTVarIndex t1) (TVar tv1)) : env
    (sub2, t2) <- exprToSubstituition' env1 t e2
    sub3 <- unify $ (t, t2) : sub1 ++ sub2
    return $ (sub3, applySub sub3 t)
  EFun s1 e1 -> do
    tv1 <- getNewTVarIndex
    tv2 <- getNewTVarIndex
    let env1 = (EVariable s1, TypeScheme [] (TVar tv1)) : env
    (sub1, t1) <- exprToSubstituition' env1 (TVar tv2) e1
    sub2 <- unify $ (t, TFun (TVar tv1) (TVar tv2)) : (t1, TVar tv2) : sub1
    return $ (sub2, applySub sub2 (TFun (TVar tv1) (TVar tv2)))
  EApp e1 e2 -> do
    tv2 <- getNewTVarIndex
    (sub1, t1) <- exprToSubstituition' env (TFun (TVar tv2) t) e1
    (sub2, t2) <- exprToSubstituition' env (TVar tv2) e2
    sub3 <- unify $ sub1 ++ sub2
    return $ (sub3, applySub sub3 t2)
  ELetRec s1 s2 e1 e2 -> do
    tv1 <- getNewTVarIndex
    tv2 <- getNewTVarIndex
    let env1 = (EVariable s1, TypeScheme [] (TFun (TVar tv1) (TVar tv2))) : (EVariable s2, TypeScheme [] (TVar tv1)) : env
    (sub1, t1) <- exprToSubstituition' env1 (TVar tv2) e1
    sub2 <- unify sub1
    let t2 = applySub sub2 (TFun (TVar tv1) t1)
    let env2 = (EVariable s1, TypeScheme (freeTVarIndex t2) t2) : env
    (sub2, t3) <- exprToSubstituition' (applySubEnv sub1 env1) t e2
    sub3 <- unify (sub1 ++ sub2)
    return $ (sub3, applySub sub3 t3)
  EVariable s1 -> do
    TypeScheme tvs1 t1 <- lookupTypeSchemeEnv (EVariable s1) env
    sub1 <- mapM f tvs1
    return (sub1, applySub sub1 t1)
      where
        f x = do
          y <- getNewTVarIndex
return (TVar x, TVar y)
