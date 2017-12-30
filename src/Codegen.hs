{-# LANGUAGE OverloadedStrings #-}

module Codegen
    ( genLLVM
    ) where

import Data.String

import qualified Control.Monad
import Control.Monad.State

import LLVM.AST.AddrSpace
import LLVM.AST hiding (function)
import LLVM.AST.Float
import qualified LLVM.AST.IntegerPredicate as C
import LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C

import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

import Lang

type SymbolTable = [(String, Operand)]

data CodegenState = CodegenState { symtab :: SymbolTable, locals :: SymbolTable }

emptyCodegen :: CodegenState
emptyCodegen = CodegenState [] []

genLLVM :: CompilationUnit -> Module
genLLVM (CompilationUnit functions) = evalState (buildModuleT "program" $ mapM_ genFunction functions) emptyCodegen

genFunction :: Lang.Function -> ModuleBuilderT (State CodegenState) ()
genFunction (Lang.Function name parameters returnType (Block definition)) = do
  modify $ \ s -> s { locals = map (\ (ParameterDeclaration name langType) -> (name, LocalReference (llvmType langType) (fromString name))) parameters }

  let retty = llvmReturnType returnType
  (ConstantOperand (C.GlobalReference fn _)) <- function (fromString name) (map parameter parameters) (llvmReturnType returnType) $ \ _ ->
    block `named` "entry" >> mapM_ genStatement definition
  let op = ConstantOperand $ C.GlobalReference (PointerType fn (AddrSpace 0)) (Name $ fromString name)

  symbols <- gets symtab
  modify $ \ s -> s { symtab = (name, op) : symbols }

parameter :: ParameterDeclaration -> (AST.Type, ParameterName)
parameter (ParameterDeclaration identifier langType) = (llvmType langType, fromString identifier)

genStatement :: Lang.Statement -> IRBuilderT (ModuleBuilderT (State CodegenState)) ()
genStatement (Return expr) = ret =<< genExpression expr
genStatement (Declaration name langType Nothing) = return ()
genStatement (Declaration name langType (Just definition)) = do
  op <- genExpression definition
  lcls <- gets locals
  modify $ \ s -> s { locals = (name, op) : lcls }
genStatement (Assignment name expr) = do
  op <- genExpression expr
  lcls <- gets locals
  modify $ \ s -> s { locals = (name, op) : lcls }
genStatement (Unary op (Variable x)) = do
  let one = ConstantOperand (C.Int 64 1)
  let ins = case op of
              Increment -> add one
              Decrement -> flip sub one
  var <- genExpression (Variable x)
  var' <- ins var
  lcls <- gets locals
  modify $ \ s -> s { locals = (x, var') : lcls }
genStatement (If cond (Block thenBlock) elseBlock) = do
  result <- genComparison cond
  condBr result (Name "then") (Name "else")

  block `named` "then"
  mapM_ genStatement thenBlock

  block `named` "else"
  case elseBlock of
    Nothing -> return ()
    Just (Block elseBlock) -> mapM_ genStatement elseBlock

genExpression :: Lang.Expression -> IRBuilderT (ModuleBuilderT (State CodegenState)) Operand
genExpression (Atomic a) = return $ genAtomic a
genExpression (Variable name) = do
  lcls <- gets locals
  case lookup name lcls of
    Nothing -> fail $ "Variable " ++ name ++ " not declared."
    Just op -> return op
genExpression (Invocation name parameters) = do
  symbols <- gets symtab
  case lookup name symbols of
    Nothing -> fail $ "Function " ++ name ++ " not declared."
    Just callee -> do
      parameters <- mapM genExpression parameters
      call callee (map (\ p -> (p, [])) parameters)
genExpression (Math op a b) =
  let ins = case op of
        Addition -> add
        Subtraction -> sub
        Division -> sdiv
        Multiplication -> mul
        Modulo -> urem
    in
  genExpression a >>= \opa -> genExpression b >>= \opb -> ins opa opb

genComparison :: Lang.Expression -> IRBuilderT (ModuleBuilderT (State CodegenState)) Operand
genComparison (Comparison op a b) = do
  let ty = case op of
        Eq -> C.EQ
        Ne -> C.NE
        Lt -> C.SLT
        Gt -> C.SGT
        Le -> C.SLE
        Ge -> C.SGE
  opa <- genExpression a
  opb <- genExpression b
  icmp ty opa opb

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
