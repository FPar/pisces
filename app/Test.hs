{-# LANGUAGE OverloadedStrings #-}

module Test where

import Data.String

import qualified Control.Monad

import LLVM.AST hiding (function)
import LLVM.AST.Float
import LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C

import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

import Lang

genTest :: Module
genTest = buildModule "program" test

test :: MonadModuleBuilder m => m Operand
test =
  function "foo" [(i32, "a"), (i32, "b")] i32 $ \ [a, b] -> do
    c <- add (ConstantOperand (C.Int 64 5)) (ConstantOperand (C.Int 64 5)) `named` "c"
    mul c (ConstantOperand (C.Int 64 5)) `named` "d"
    ret (LocalReference i64 "c")
