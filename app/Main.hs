module Main where

import qualified Data.ByteString.Char8 as BS
import LLVM.Context
import LLVM.Module
import System.Environment

import Codegen
import Parser

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> compileFile filename
    []         -> putStrLn "No filename specified."
    _          -> putStrLn "Too many arguments specified."

compileFile :: String -> IO ()
compileFile filename = do
  src <- readFile filename
  let ast = parseUnit "" src
  case ast of
    Left err ->
      print err
    Right ast -> do
      let mod = genLLVM ast
      withContext $ \context ->
        withModuleFromAST context mod $ \llmod -> do
          llstr <- moduleLLVMAssembly llmod
          BS.putStrLn llstr
