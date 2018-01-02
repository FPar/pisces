{-# LANGUAGE OverloadedStrings #-}

module CodegenSpec (spec) where

import Data.List
import Data.Maybe

import Test.Hspec 

import LLVM.AST
import LLVM.AST.Constant hiding (Add)
import LLVM.AST.Global
import LLVM.AST.Instruction
import LLVM.AST.Type

import Codegen
import Lang hiding (Function, Type)
import qualified Lang as L

spec :: Spec
spec =
  describe "genLLVM" $ do
    let main = "main"
    it "generates an empty function" $
      genLLVM (genUnit [genFunction main [] Nothing []]) `shouldBe` buildModule [buildFunction "main" [] VoidType $ [BasicBlock "entry" [] (Do $ Ret Nothing [])]]
    it "generates a return number function" $
      genLLVM (genUnit [genFunction main [] (Just I64) [Return $ Atomic $ Integer 12]]) `shouldBe` buildModule [buildFunction "main" [] i64 $ [BasicBlock "entry" [] (Do $ Ret (Just $ ConstantOperand $ Int 64 12) [])]]
    it "generates a function incrementing a parameter and returning it." $
      let langStmts = [ Unary Increment $ Variable "a"
                      , Return $ Variable "a"
                      ] in
      genLLVM (genUnit [genFunction "a" [ParameterDeclaration "a" I64] (Just I64) langStmts]) `shouldBe` buildModule [buildFunction "a" [buildParam i64 "a"] i64 $
        [BasicBlock "entry" [UnName 0 := add (ConstantOperand (Int 64 1)) (LocalReference i64 "a" )] (Do (Ret (Just (LocalReference i64 (UnName 0))) []))]
      ]

genUnit :: [L.Function] -> CompilationUnit
genUnit = CompilationUnit

genFunction :: String -> [ParameterDeclaration] -> ReturnType -> [Statement] -> L.Function
genFunction name params retty statements =
  L.Function name params retty $ Block statements

buildModule :: [Definition] -> Module
buildModule definitions = defaultModule { moduleName = "program", moduleDefinitions = definitions }

buildFunction :: Name -> [Parameter] -> Type -> [BasicBlock] -> Definition
buildFunction fnname params retty blocks = GlobalDefinition $ functionDefaults { name = fnname
                                                                               , parameters = (params,False)
                                                                               , returnType = retty
                                                                               , basicBlocks = blocks }

buildParam :: Type -> Name -> Parameter
buildParam t n = Parameter t n []

add :: Operand -> Operand -> Instruction
add a b = Add False False a b []
