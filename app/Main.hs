{-# LANGUAGE LambdaCase #-}

module Main where

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
  readFile filename >>= \src ->
    let ast = parseUnit "" src in
    case ast of
      Left err ->
        print err
      Right ast ->
        let llvmAST = genLLVM ast in
        withContext $ \context ->
          withModuleFromAST context llvmAST (moduleLLVMAssembly >=> BS.writeFile "a.ll")
