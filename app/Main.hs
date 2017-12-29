{-# LANGUAGE LambdaCase #-}

module Main where

import Lang

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import LLVM.Context
import LLVM.Module
import System.Environment

import Codegen
import Parser

main :: IO ()
main = getArgs >>=
  \case
    [filename] -> compileFile filename
    []         -> putStrLn "No filename specified."
    _          -> putStrLn "Too many arguments specified."

compileFile :: String -> IO ()
compileFile filename =
  loadUnit filename >>= \case
    Left err ->
      print err
    Right unit ->
      let llvmAST = genLLVM unit in
      withContext $ \context ->
        withModuleFromAST context llvmAST (moduleLLVMAssembly >=> BS.writeFile "a.ll")

loadUnit :: String -> IO (Either String CompilationUnit)
loadUnit filename = readFile filename >>= \ src ->
  let cu = parseUnit "" src in
  case cu of
    Left err -> return $ Left $ show err
    Right cu -> return $ Right cu
