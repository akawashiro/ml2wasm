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
import Options.Applicative
import Data.Semigroup ((<>))

data Option = Option
  { input :: String,
    memory :: String,
   verbose :: Bool  }

optionParser :: Parser Option
optionParser = Option
  <$> strArgument (metavar "FILE")
  <*> strOption
    (  long "memory"
    <> metavar "PATH"
    <> help "Path to the wast file of memory allocater")
  <*> switch
    (  long "verbose"
    <> short 'v'
    <> help "Output debug information" )

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

main' :: Option -> IO ()
main' o = do
  f <- readFile (input o)
  if verbose o then showDetails f else printWasmCode f

main :: IO ()
main = main' =<< execParser opts
  where
    opts = info (optionParser <**> helper)
      ( fullDesc
     <> progDesc "Compile ML to WASM"
     <> header "ml2wasm - a toy compiler from MiniML to WASM." )