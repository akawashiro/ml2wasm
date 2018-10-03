{-# LANGUAGE DataKinds #-}
module Main where

import Control.Monad.Trans
import Options.Declarative
import Parse
import Control.Monad
import Alpha
import KNormal
import NestedLet
import Closure
import WasmGen

printWasmCode :: String -> IO ()
printWasmCode input = do
  let parsed = stringToExp input
  let knormaled = (liftM exprToKNormalExpr) parsed
  let alphad = exprToAlphaExpr `liftM` knormaled
  let nonNest = expToNonNest `liftM` alphad
  let closured = clsTrans `liftM` nonNest
  let wasm = prog2Wasm `liftM` closured
  putStrLn (f wasm)
    where f a = either show show a


showDetails :: String -> IO ()
showDetails input = do
  putStr $ "Input = \n" ++ input
  let parsed = stringToExp input
  putStr $ "After parse = \n" ++ f parsed ++ "\n\n"
  let knormaled = (liftM exprToKNormalExpr) parsed
  putStrLn $ "After K-normalization = \n" ++ f knormaled ++ "\n"
  let alphad = exprToAlphaExpr `liftM` knormaled
  putStrLn $ "After alpha conversion = \n" ++ f alphad ++ "\n"
  let nonNest = expToNonNest `liftM` alphad
  putStrLn $ "After no-nested-let conversion = \n" ++ f nonNest ++ "\n"
  let closured = clsTrans `liftM` nonNest
  putStrLn $ "After closure translation = \n" ++ f closured ++ "\n"
  let wasm = prog2Wasm `liftM` closured
  putStrLn $ "Generated wasm code = \n" ++ f wasm ++ "\n"
    where f a = either show show a

compile :: Flag "d" '["debug"] "" "debug option" Bool
        -> Arg "Sorce file" String
        -> Cmd "MiniML compiler" ()
compile debug source = do
  let f | get debug = readFile (get source) >>= showDetails
        | otherwise = readFile (get source) >>= printWasmCode
  liftIO f

main :: IO ()
main = run_ compile
