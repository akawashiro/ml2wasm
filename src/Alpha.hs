{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Alpha where

import           Control.Monad
import           Control.Monad.State
import qualified Data.Map            as Map
import           Data.Maybe
import           Parse               (Exp (..), Var (..))

exprToAlphaExpr :: Exp -> Exp
exprToAlphaExpr exp = evalState (exprToAlphaExpr' exp) (Map.empty,0)

type NameState = (Map.Map String String, Int)

addNewName :: String -> Var -> State NameState ()
addNewName prefix (Var t s) = do
  (m,i) <- get
  unless (Map.member s m) $ put (Map.insert s (prefix ++ s ++ "_" ++ show i) m , i+1)

rename :: Var -> State NameState Var
rename (Var t s) = do
  (m,_) <- get
  return $ maybe (Var t $ "Cannot find name of " ++ show s) (Var t) (Map.lookup s m)

isFun :: Exp -> Bool
isFun exp = case exp of
  ERec {}    -> True
  ELet _ _ _ e -> isFun e
  _          -> False

exprToAlphaExpr' :: Exp -> State NameState Exp
exprToAlphaExpr' exp = case exp of
  EInt t i -> return $ EInt t i
  EFloat t f -> return $ EFloat t f
  EBool t b -> return $ EBool t b
  EOp t o e1 e2 -> do
    e1' <- exprToAlphaExpr' e1
    e2' <- exprToAlphaExpr' e2
    return $ EOp t o e1' e2'
  EIf t e1 e2 e3 -> do
    e1' <- exprToAlphaExpr' e1
    e2' <- exprToAlphaExpr' e2
    e3' <- exprToAlphaExpr' e3
    return $ EIf t e1' e2' e3'
  ELet t s e1 e2 -> do
    addNewName "val_" s
    s' <- rename s
    e1' <- exprToAlphaExpr' e1
    e2' <- exprToAlphaExpr' e2
    return $ ELet t s' e1' e2'
  EDTuple t vs e1 e2 -> do
    mapM_ (addNewName "val_") vs
    vs' <- mapM rename vs
    e1' <- exprToAlphaExpr' e1
    e2' <- exprToAlphaExpr' e2
    return $ EDTuple t vs' e1' e2'
  EApp t e1 e2 -> do
    e1' <- exprToAlphaExpr' e1
    e2' <- mapM exprToAlphaExpr' e2
    return $ EApp t e1' e2'
  ERec t s1 s2 e1 e2 -> do
    addNewName "fun_" s1
    s1' <- rename s1
    e2' <- exprToAlphaExpr' e2
    mapM_ (addNewName "val_") s2
    s2' <- mapM rename s2
    e1' <- exprToAlphaExpr' e1
    return $ ERec t s1' s2' e1' e2'
  EVar t s -> do
    s' <- rename s
    return $ EVar t s'
  ETuple t es -> do
    es' <- mapM exprToAlphaExpr' es
    return $ ETuple t es'
  ESeq t e1 e2 -> do
    e1' <- exprToAlphaExpr' e1
    e2' <- exprToAlphaExpr' e2
    return $ ESeq t e1' e2'
  EMakeA t e1 -> do
    e1' <- exprToAlphaExpr' e1
    return $ EMakeA t e1'
  EGetA t e1 e2 -> do
    e1' <- exprToAlphaExpr' e1
    e2' <- exprToAlphaExpr' e2
    return $ EGetA t e1' e2'
  ESetA t e1 e2 e3 -> do
    e1' <- exprToAlphaExpr' e1
    e2' <- exprToAlphaExpr' e2
    e3' <- exprToAlphaExpr' e3
    return $ ESetA t e1' e2' e3'
  EPrintI32 t e1 -> EPrintI32 t <$> exprToAlphaExpr' e1
  EPrintF32 t e1 -> EPrintF32 t <$> exprToAlphaExpr' e1
