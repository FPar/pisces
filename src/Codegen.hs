module Codegen
    ( genLLVM
    ) where

import Data.String
import Lang
import LLVM.AST
import LLVM.AST.Constant
import LLVM.AST.Global
import LLVM.AST.Instruction
import LLVM.AST.Type

genLLVM :: Lang.CompilationUnit -> Module
genLLVM (CompilationUnit functions) =
  defaultModule { moduleName           = fromString "program"
                , moduleDefinitions    = map definition functions
                }

definition :: Lang.Function -> Definition
definition (Lang.Function name parameters returnType def) =
  GlobalDefinition $
    functionDefaults { name        = Name $ fromString name
                     , returnType  = i32
                     , basicBlocks = [block def]
                     }

block :: Lang.Block -> BasicBlock
block def =
  let val = Int 32 4 in
  BasicBlock (fromString "__block") [] $ Do $ Ret (Just $ ConstantOperand val) []
