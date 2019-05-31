{-# LANGUAGE DataKinds #-}
module Main where

import Control.Monad.Trans
import System.Environment
import Parse
import Type
import Control.Monad
import Alpha
import Closure
import UnusedVar
import WasmGen

printWasmCode :: String -> IO ()
printWasmCode input = do
  let parsed = stringToExp input
  let alphad = exprToAlphaExpr <$> parsed
  let typed = alphad >>= typingExp
  let closured = clsTrans `liftM` typed
  let unused = removeUnused `liftM` closured
  let wasm = prog2Wasm `liftM` unused
  putStrLn (f wasm)
    where f a = either show show a


showDetails :: String -> IO ()
showDetails input = do
  putStr $ "Input = \n" ++ input

  let parsed = stringToExp input
  putStr $ "After parse = \n" ++ f parsed ++ "\n\n"

  let alphad = exprToAlphaExpr <$> parsed
  putStrLn $ "After alpha conversion = \n" ++ f alphad ++ "\n"

  let typed = parsed >>= typingExp
  putStr $ "After typing = \n" ++ f typed ++ "\n\n"

  let closured = clsTrans `liftM` typed
  putStrLn $ "After closure translation = \n" ++ f closured ++ "\n"

  let unused = removeUnused `liftM` closured
  putStrLn $ "After unused variable translation = \n" ++ f unused ++ "\n"

  let wasm = prog2Wasm `liftM` unused
  putStrLn $ "Generated wasm code = \n" ++ f wasm ++ "\n"
    where f a = either show show a

-- compile :: Flag "d" '["debug"] "" "debug option" Bool
--         -> Arg "Sorce file" String
--         -> Cmd "MiniML compiler" ()
-- compile debug source = do
--   let f | get debug = readFile (get source) >>= showDetails
--         | otherwise = readFile (get source) >>= printWasmCode
--   liftIO f

main :: IO ()
main = do
  fs <- getArgs
  readFile (head fs) >>= showDetails
