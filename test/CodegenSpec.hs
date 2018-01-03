{-# LANGUAGE OverloadedStrings #-}

module CodegenSpec (spec) where

import Data.List
import Data.Maybe

import Test.Hspec 

import LLVM.AST.AddrSpace
import LLVM.AST.Constant
import LLVM.AST.IntegerPredicate
import LLVM.AST.Operand
import LLVM.AST.Type

import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

import Codegen
import Lang hiding (Function, Type)
import qualified Lang as L

spec :: Spec
spec =
  describe "genLLVM" $ do
    let main = "main"
    it "generates an empty function" $
      genLLVM (genUnit [genFunction main [] Nothing []]) `shouldBe`
        (buildModule "program" $ do
          function "main" [] void $ \ _ -> do
            entry <- block `named` "entry"; do
              retVoid)
    it "generates a return number function" $
      genLLVM (genUnit [genFunction main [] (Just I64) [Return $ Atomic $ Integer 12]]) `shouldBe`
        (buildModule "program" $ do
          function "main" [] i64 $ \ _ -> do
            entry <- block `named` "entry"; do
              ret $ ConstantOperand (Int 64 12))
    it "generates a function incrementing a parameter and returning it." $
      let langStmts = [ Unary Increment $ Variable "a"
                      , Return $ Variable "a"
                      ] in
      genLLVM (genUnit [genFunction "a" [ParameterDeclaration "a" I64] (Just I64) langStmts]) `shouldBe`
        (buildModule "program" $ do
          function "a" [(i64, "a")] i64 $ \ [a] -> do
            entry <- block `named` "entry"; do
              a <- add (ConstantOperand (Int 64 1)) a
              ret a)
    it "generates a function call with parameters that returns a value." $
      let langA = genFunction "a" [ParameterDeclaration "x" I64] (Just I64) [Return $ Variable "x"]
          langMain = genFunction main [] (Just I64) [Return $ Invocation "a" [Atomic $ Integer 12]]
      in
      genLLVM (genUnit [langA, langMain]) `shouldBe`
        (buildModule "program" $ do
          function "a" [(i64, "x")] i64 $ \ [x] -> do
            entry <- block `named` "entry"; do
              ret x
          function "main" [] i64 $ \ _ -> do
            entry <- block `named` "entry"; do
              a <- call (ConstantOperand $ GlobalReference (PointerType (FunctionType i64 [i64] False) (AddrSpace 0)) "a") [(ConstantOperand (Int 64 12),[])]
              ret a)
    it "generates an if statement" $
      let langMax = genFunction "max" [ParameterDeclaration "a" I64, ParameterDeclaration "b" I64] (Just I64)
                      [If (Comparison Gt (Variable "a") (Variable "b")) (Block [Return $ Variable "a"]) (Just $ Block [Return $ Variable "b"])]
      in
      genLLVM (genUnit [langMax]) `shouldBe`
        (buildModule "program" $ do
          function "max" [(i64, "a"), (i64, "b")] i64 $ \ [a, b] -> do
            entry <- block `named` "entry"; do
              x <- icmp SGT a b
              condBr x "then" "else"
            thenBlock <- block `named` "then"; do
              ret a
            elseBlock <- block `named` "else"; do
              ret b)

genUnit :: [L.Function] -> CompilationUnit
genUnit = CompilationUnit

genFunction :: String -> [ParameterDeclaration] -> ReturnType -> [Statement] -> L.Function
genFunction name params retty statements =
  L.Function name params retty $ Block statements

