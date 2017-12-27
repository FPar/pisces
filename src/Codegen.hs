{-# LANGUAGE OverloadedStrings #-}

module Codegen
    ( genLLVM
    ) where

import Data.String

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

genLLVM :: CompilationUnit -> Module
genLLVM (CompilationUnit functions) = buildModule "program" $ mapM genFunction functions

genFunction :: MonadModuleBuilder m => Lang.Function -> m Operand
genFunction (Lang.Function name parameters returnType (Block definition)) =
  function (fromString name) [] (llvmReturnType returnType) $ \[] ->
    block `named` "entry" >> mapM_ genStatement definition

genStatement :: MonadIRBuilder m => Lang.Statement -> m ()
{-genStatement (Assignment ident expr) = ()-}
genStatement (Return expr) = genExpression expr >>= ret

genExpression :: MonadIRBuilder m => Lang.Expression -> m Operand
genExpression (Atomic a) = return $ genAtomic a
genExpression (Math op a b) =
  let ins = case op of
        Addition -> add
        Subtraction -> sub
        Division -> sdiv
        Multiplication -> mul
        Modulo -> urem
    in
  genExpression a >>= \opa -> genExpression b >>= \opb -> ins opa opb

genAtomic :: Atomic -> Operand
genAtomic (Integer i) = ConstantOperand (C.Int 64 i)
genAtomic (Float f) = ConstantOperand (C.Float $ Double f)

llvmReturnType :: ReturnType -> AST.Type
llvmReturnType (Just t) = llvmType t
llvmReturnType Nothing = VoidType

llvmType :: Lang.Type -> AST.Type
llvmType t =
  case t of
    I64 -> i64
    F64 -> AST.double
